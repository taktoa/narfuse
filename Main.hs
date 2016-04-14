{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Lazy    as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Word
import           Data.List
import           GHC.Generics         (Generic)
import           System.IO.Posix.MMap
import           Debug.Trace
import           System.Directory
import           System.Environment
import           System.Fuse
import           System.Posix.Files

type Map = Map.HashMap

type Offset = Int

type Size = Int

data MMap a = MMapPure   !a
            | MMapModify !Offset !Size
                         !(LBS.ByteString -> (Maybe LBS.ByteString, a))

instance Functor MMap where
  fmap f (MMapPure x)        = MMapPure $ f x
  fmap f (MMapModify o s cb) = MMapModify o s $ second f . cb

--------------------------------------------------------------------------------
---- NOT DONE YET --------------------------------------------------------------
--------------------------------------------------------------------------------

-- class Applicative m where
--   pure  :: a -> m a
--   (<*>) :: m (a -> b) -> m a -> m b


-- instance Applicative MMap where
--   pure = MMapPure
--   (MMapPure f)        <*> mx                     = fmap f mx
--   (MMapModify o s cb) <*> (MMapPure x)           = MMapModify o s (second (\f -> f x) . cb)
--   (MMapModify o s cb) <*> (MMapModify o' s' cb') = MMapModify on sn cbn
--     where
--       -- o,  o'  :: Offset
--       -- s,  s'  :: Size
--       -- cb      :: ByteString -> (Maybe ByteString, a -> b)
--       -- cb'     :: ByteString -> (Maybe ByteString, a)
--       on = undefined :: Offset
--       sn = undefined :: Size
--       cbn = undefined :: ByteString -> (Maybe ByteString, b)
--   -- cb  :: ByteString -> (Maybe ByteString, a -> b)
--   -- cb' :: ByteString -> (Maybe ByteString, a)

-- [(o o o o o o o o) (o o o o o o o o) (o o o o o o o o) (o o o o o o o o)]
-- modify at offset 4 with size 8 to get x
-- [(o o o o x x x x) (x x x x o o o o) (o o o o o o o o) (o o o o o o o o)]
-- modify at offset 6 with size 4 to get y
-- [(o o o o x x y y) (y y x x o o o o) (o o o o o o o o) (o o o o o o o o)]

--------------------------------------------------------------------------------
---- REFERENCE -----------------------------------------------------------------
--------------------------------------------------------------------------------

type Path = LBS.ByteString
type Name = LBS.ByteString

data NARNode = File      { _isExecutable :: !Bool
                         , _contents     :: LBS.ByteString }
             | Symlink   { _target :: !Path }
             | Directory { _children :: Map.HashMap Name NARNode }
             deriving (Eq, Read, Generic)

newtype NARFile = NARFile { _root :: NARNode }
                deriving (Eq, Show, Read, Generic)

-- Shuts up the compiler for now, but orphan instances are bad,
-- so this should be deleted.
instance (Eq k, Hashable k, Binary k, Binary v) => Binary (Map k v) where
  put = put . Map.toList
  get = Map.fromList <$> get

mkNARFile :: NARNode -> NARFile
mkNARFile = NARFile -- FIXME: normalize NARFile

choice :: (Alternative m) => [m a] -> m a
choice = foldl' (<|>) empty

whenFail :: (Monad m) => String -> Bool -> m ()
whenFail str True  = fail str
whenFail _   False = return ()

padNum :: (Integral n) => n -> n -> n
padNum m x = if r > 0 then x + m - r else x
  where
    r = x `mod` m

getLBS :: Integral i => i -> Get LBS.ByteString
getLBS = getLazyByteString . fromIntegral

narStringG :: Get (Word64, LBS.ByteString)
narStringG = do
  len <- getWord64le
  if len == 0
    then return (8, "")
    else do str <- getLBS len
            let padded = padNum 8 len
            let expectedZeros = fromIntegral $ padded - len
            zeros <- replicateM expectedZeros getWord8
            whenFail "padding was not filled with zeroes"
              $ zeros /= replicate expectedZeros 0
            return (8 + padded, str)

match :: (Eq a, Show a) => Get a -> a -> Get ()
match g bs = do
  input <- g
  whenFail ("match failed: " <> show input <> " != " <> show bs) (input /= bs)

matchNSG :: LBS.ByteString -> Get ()
matchNSG = match (snd <$> narStringG)

matchNSGPair :: LBS.ByteString -> LBS.ByteString -> Get ()
matchNSGPair bs1 bs2 = matchNSG bs1 >> matchNSG bs2

tryGet :: Get a -> Get (Maybe a)
tryGet g = (Just <$> g) <|> return Nothing

execGet :: Get Bool
execGet = isJust <$> tryGet (matchNSGPair "executable" "")

contentsGet :: Get LBS.ByteString
contentsGet = matchNSG "contents" >> snd <$> narStringG

entryGet :: Get (Name, NARNode)
entryGet = do matchNSG "entry"
              matchNSG "("
              matchNSG "name"
              name <- snd <$> narStringG
              matchNSG "node"
              node <- get
              matchNSG ")"
              return (name, node)

regularGet :: Get NARNode
regularGet = File <$> execGet <*> contentsGet

symlinkGet :: Get NARNode
symlinkGet = matchNSG "target" >> Symlink . snd <$> narStringG

directoryGet :: Get NARNode
directoryGet = Main.Directory . Map.fromList <$> many entryGet

magicGet :: Get ()
magicGet = do (_, magic) <- narStringG
              when (magic /= "nix-archive-1") $ fail "magic number was wrong"

data Foo = Foo { getFoo :: [(Word64, LBS.ByteString)] }
         deriving (Eq, Generic)

instance Show Foo where
  show = concatMap ((<> "\n") . showX) . getFoo
    where
      showB bs = if (LBS.length bs > 1024) || isJust (LBS.findIndex (== 0) bs)
                 then "\ESC[31m<too long>\ESC[0m"
                 else show bs
      showX (l, bs) = "{ length = " <> show l <> ", bs = " <> showB bs <> " }"

instance Show NARNode where
    show = go 2
        where
            go :: Int -> NARNode -> String
            go i (File executable contents) = (if executable then "executable " else "") <> "FILE " <> showB contents <> "\n"
            go i (Main.Directory entries) = "{\n" <> (mconcat $ intersperse "" $ toList $ Map.mapWithKey showX entries) <> indent (i-2) <> "}\n"
                where
                    showX name node = indent i <> "dir entry " <> show name <> "=" <> go (i+2) node
            go i (Symlink l) = "symlink to " <> show l <> "\n"
            indent :: Int -> String
            indent i = if i < 1 then "" else ' ' : indent (i - 1)
            showB bs = if (LBS.length bs > 1024) || isJust (LBS.findIndex (== 0) bs)
                 then "\ESC[31m<too long>\ESC[0m"
                 else show bs

showDot (NARFile node) = "digraph test123 {\n" <> go "" "root" node <> "}"
    where
        go :: String -> String -> NARNode -> String
        go parent name (File executable contents) = quote parent <> " -> " <> quote (name <> (if executable then " (executable)" else "")) <> ";\n"
        go parent name (Main.Directory entries) = quote parent <> " -> " <> quote name <> ";\n" <> (mconcat $ intersperse "" $ toList $ Map.mapWithKey showX entries)
                where
                    showX entry_name node = go name (LBSC.unpack entry_name) node
        go parent name (Symlink l) = quote parent <> " -> " <> quote name <> ";\n" <> quote name <> " -> " <> quote (LBSC.unpack l) <> " [color=blue];\n"
        indent :: Int -> String
        indent i = if i < 1 then "" else ' ' : indent (i - 1)
        quote x = "\"" <> x <> "\""

readDir :: NARFile -> [String] -> Maybe NARNode
readDir (NARFile node) path = go path node
    where
        go (x : xs) (Main.Directory entries)
            = case Map.lookup (LBSC.pack x) entries of
                Just e -> go xs e
                Nothing -> Nothing
        go _ n = Just n

instance Binary Foo where
  get = Foo <$> many narStringG

instance Binary NARNode where
  get = do matchNSG "("
           matchNSG "type"
           (_, t) <- narStringG
           r <- case t of "regular"   -> regularGet
                          "symlink"   -> symlinkGet
                          "directory" -> directoryGet
                          _           -> fail "invalid type"
           matchNSG ")"
           return r

instance Binary NARFile where
  get = magicGet >> NARFile <$> get

tinyFile, shortFile, mediumFile, longFile :: FilePath
tinyFile   = "./container_data/5kfrplg1gj753j10k8xka9c9ggap6918-etc-fstab.nar"
shortFile  = "./container_data/s0aqc77hi1vhm95j0rd3xhdynspccik7-system-units.nar"
mediumFile = "./container_data/14fqnkfb0dqs3grn4jh2xyii0kaik9br-util-linux-2.27.1.nar"
longFile   = "./container_data/0b0y9jz2b1q0hlf40p50ygrj2vhbk0fq-glibc-locales-2.23.nar"

decodeNARFile :: FilePath -> IO NARFile
decodeNARFile = decodeFile

decodeFooFile :: FilePath -> IO Foo
decodeFooFile = decodeFile

splitPath :: String -> [String]
splitPath x = map reverse $ go "" x
    where
        go :: String -> String -> [String]
        go "" ('/':ys) = go "" ys
        go sofar ('/':ys) = sofar : go "" ys
        go sofar (y:ys) = go (y : sofar) ys
        go sofar "" = [ sofar ]

main :: IO ()
main = do
  args <- getArgs
  file <- decodeNARFile (head args)
  --decodeNARFile (head args) >>= print
  --decodeNARFile (head args) >>= putStrLn . showDot
  let path = splitPath "/share/man/man1"
  case readDir file path of
    Nothing -> print "404"
    Just n -> print ("found " <> show n)

-- return storePaths or Nothings
isNar :: String -> Maybe String
isNar name = go "" name
    where
        go :: String -> String -> Maybe String
        go rest (x:".nar") = Just $ reverse (x : rest)
        go rest (x:xs) = go (x : rest) xs
        go _ _ = Nothing

-- strip out the Nothings
reFilter :: [ Maybe String ] -> [ String ]
reFilter list = go [] list
    where
        go rest (x:xs) = case x of
            Nothing -> go rest xs
            Just n -> go (n : rest) xs
        go rest _ = rest

thing :: [ FilePath ] -> [ FilePath ]
thing files = reFilter (map isNar files)

test1 :: IO ()
test1 = do
    listing <- getDirectoryContents "/home/clever/apps/data_files/"
    print $ (thing listing)

test2 :: IO ()
test2 = do
    prog <- getProgName
    fuseRun prog [ "-f", "-o", "allow_other", "/home/clever/apps/narparser/mnt" ] narFSOps defaultExceptionHandler

type HT = ()

narFSOps :: FuseOperations HT
narFSOps = defaultFuseOps {
    fuseGetFileStat   = narGetFileStat,
    fuseOpenDirectory = narOpenDirectory,
    fuseReadDirectory = narReadDirectory
}

dirStat ctx = FileStat {
    statEntryType = System.Fuse.Directory,
    statFileMode = foldr1 unionFileModes [
        ownerReadMode,
        ownerExecuteMode,
        groupReadMode,
        groupExecuteMode,
        otherReadMode,
        otherExecuteMode
    ],
    statLinkCount = 2,
    statFileOwner = 0,
    statFileGroup = 0,
    statSpecialDeviceID = 0,
    statFileSize = 4096,
    statBlocks = 1,
    statAccessTime = 0,
    statModificationTime = 0,
    statStatusChangeTime = 0
}

fileStat ctx = FileStat {
    statEntryType = RegularFile,
    statFileMode = foldr1 unionFileModes [ ownerReadMode, groupReadMode, otherReadMode ],
    statLinkCount = 1,
    statFileOwner = 0,
    statFileGroup = 0,
    statSpecialDeviceID = 0,
    statFileSize = 0, -- FIXME
    statBlocks = 1,
    statAccessTime = 0,
    statModificationTime = 0,
    statStatusChangeTime = 0
}

narGetFileStat :: FilePath -> IO (Either Errno FileStat)
narGetFileStat "/" = do
    ctx <- getFuseContext
    return $ Right $ dirStat ctx

narOpenDirectory "/" = return eOK

thing2 :: FuseContext -> FilePath -> (FilePath, FileStat)
thing2 ctx nar = (nar, (dirStat ctx))

narReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
narReadDirectory "/" = do
    ctx <- getFuseContext
    listing <- getDirectoryContents "/home/clever/apps/data_files/"
    --modified <- map (thing2 ctx) $ thing listing
    return $ Right $ [(".",    dirStat  ctx),
                      ("..",   dirStat  ctx)] <> (map (thing2 ctx) $ thing listing)
