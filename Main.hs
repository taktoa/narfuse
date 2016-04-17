{-# OPTIONS_GHC -fno-warn-unused-do-bind -Wall #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Concurrent.MVar
import           Control.Exception
import           Data.Binary
import           Data.Binary.Get
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Lazy          as Map
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Word
import           Data.Int
import           Debug.Trace
import           GHC.Generics               (Generic)
import           System.Directory
import           System.Environment
import           System.Fuse
import           System.IO.Posix.MMap
import           System.Posix.Files
import           System.Posix.Types
import Data.Time
import Data.Time.Clock.POSIX


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

data NARNode = NFile      { _isExecutable :: !Bool
                          , _contents     :: LBS.ByteString }
             | NSymlink   { _target :: Path }
             | NDirectory { _children :: Map.HashMap Name NARNode }
             deriving (Eq, Read, Generic)

newtype NARFile = NARFile { _root :: NARNode }
                deriving (Eq, Show, Read, Generic)

data RuntimeState = RuntimeState { _DataDir :: FilePath
                                 , _NarCache :: MVar (Map.HashMap String NARFile) }

data HT = HT { _handle :: NARNode }

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
            let
              padded = padNum 8 len
              expectedZeros = fromIntegral $ padded - len
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
              return $ trace "entry thunked" (name, node)

regularGet :: Get NARNode
regularGet = trace "got file" $ NFile <$> execGet <*> contentsGet

symlinkGet :: Get NARNode
symlinkGet = matchNSG "target" >> NSymlink . snd <$> narStringG

directoryGet :: Get NARNode
directoryGet = NDirectory . Map.fromList <$> many entryGet

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
        go _ (NFile exec contents) = (if exec then "executable " else "") <> "FILE " <> showB contents <> "\n"
        go i (NDirectory entries) = "{\n" <> (mconcat $ intersperse "" $ toList $ Map.mapWithKey showX entries) <> indent (i-2) <> "}\n"
          where
            showX name node = indent i <> "dir entry " <> show name <> "=" <> go (i+2) node
        go _ (NSymlink l) = "symlink to " <> show l <> "\n"
        indent :: Int -> String
        indent i = if i < 1 then "" else ' ' : indent (i - 1)
        showB bs = if (LBS.length bs > 1024) || isJust (LBS.findIndex (== 0) bs)
                   then "\ESC[31m<too long>\ESC[0m"
                   else show bs

showDot :: NARFile -> String
showDot (NARFile node) = "digraph test123 {\n" <> go "" "root" node <> "}"
    where
        go :: String -> String -> NARNode -> String
        go parent name (NFile exec _) = quote parent <> " -> " <> quote (name <> (if exec then " (executable)" else "")) <> ";\n"
        go parent name (NDirectory entries) = quote parent <> " -> " <> quote name <> ";\n" <> (mconcat $ intersperse "" $ toList $ Map.mapWithKey showX entries)
                where
                    showX entry_name node2 = go name (LBSC.unpack entry_name) node2
        go parent name (NSymlink l) = quote parent <> " -> " <> quote name <> ";\n" <> quote name <> " -> " <> quote (LBSC.unpack l) <> " [color=blue];\n"
        quote x = "\"" <> x <> "\""

traverseNodes :: NARNode -> [String] -> Maybe NARNode
traverseNodes node path = go path node
  where
    go (x : xs) (NDirectory es) = Map.lookup (LBSC.pack x) es >>= go xs
    go _        n               = Just n

instance Binary Foo where
  get = Foo <$> many narStringG

instance Binary NARNode where
  put = undefined
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

oldmain :: IO ()
oldmain = do
  args <- getArgs
  file <- decodeNARFile (head args)
  --decodeNARFile (head args) >>= print
  --decodeNARFile (head args) >>= putStrLn . showDot
  let path = splitPath "/share/man/man1"
  case traverseNodes (_root file) path of
    Nothing -> putStrLn "404"
    Just n -> print ("found " <> show n)

endsWith :: String -> String -> Maybe String
endsWith str ext = go "" (length ext) (length str) str
  where
    go _ _  _  []                            = Nothing
    go _ _  0  _                             = Nothing
    go a le ls s      | ls == le && s == ext = Just $ reverse a
    go a le ls (s:ss)                        = go (s:a) le (ls - 1) ss

-- return storePaths or Nothings
isNar :: String -> Maybe String
isNar name = name `endsWith` ".nar"


-- strip out the Nothings
reFilter :: [Maybe String] -> [String]
reFilter list = go [] list
  where
    go rest (x:xs) = case x of Nothing -> go rest xs
                               Just n -> go (n : rest) xs
    go rest _ = rest

getNARFiles :: [FilePath] -> [FilePath]
getNARFiles = concatMap (toList . isNar)

test1 :: IO ()
test1 = do
  listing <- getDirectoryContents "/home/clever/apps/data_files/"
  print $ getNARFiles listing

newNarCache :: Map.HashMap String NARFile
newNarCache = Map.empty

tmain :: IO ()
tmain = do
  args <- getArgs
  prog <- getProgName
  --let opts = [ "-f", "-o", "allow_other", "/home/clever/apps/narparser/mnt" ]
  empty_map <- newMVar newNarCache
  let state = RuntimeState "/home/clever/apps/container_data" empty_map
  --let state = RuntimeState "/home/clever/apps/narparser/sample" empty_map
  --fuseRun prog opts (narFSOps state) defaultExceptionHandler
  fuseMain (narFSOps state) defaultExceptionHandler

main :: IO ()
main = do
  file <- decodeNARFile "/nix/store/wla2an5q64wddgz7zjxkkllpvibzxw7p-data/0b0y9jz2b1q0hlf40p50ygrj2vhbk0fq-glibc-locales-2.23.nar"
  a <- getPOSIXTime
  print file
  b <- getPOSIXTime
  print file
  c <- getPOSIXTime
  print (b-a)
  print (c-b)

narFSOps :: RuntimeState -> FuseOperations HT
narFSOps x = defaultFuseOps { fuseGetFileStat   = narGetFileStat x
                            , fuseReadSymbolicLink = narReadSymbolicLink x
                            , fuseOpenDirectory = narOpenDirectory x
                            , fuseReadDirectory = narReadDirectory x
                            , fuseOpen          = narOpen x
                            , fuseRead          = narRead x
                            }

unionModes :: [FileMode] -> FileMode
unionModes = foldr1 unionFileModes

dirStat :: FileStat
dirStat = FileStat { statEntryType        = System.Fuse.Directory
                       , statFileMode         = unionModes [ ownerReadMode
                                                           , ownerExecuteMode
                                                           , groupReadMode
                                                           , groupExecuteMode
                                                           , otherReadMode
                                                           , otherExecuteMode ]
                       , statLinkCount        = 2
                       , statFileOwner        = 0
                       , statFileGroup        = 0
                       , statSpecialDeviceID  = 0
                       , statFileSize         = 4096
                       , statBlocks           = 1
                       , statAccessTime       = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0 }

fileStat :: Bool -> Int64 -> FileStat
fileStat exec size = FileStat { statEntryType        = RegularFile
                        , statFileMode         = unionModes ([ ownerReadMode
                                                            , groupReadMode
                                                            , otherReadMode ] <> if exec then [ ownerExecuteMode, groupExecuteMode, otherExecuteMode ] else [])
                        , statLinkCount        = 1
                        , statFileOwner        = 0
                        , statFileGroup        = 0
                        , statSpecialDeviceID  = 0
                        , statFileSize         = fromIntegral size
                        , statBlocks           = 1
                        , statAccessTime       = 0
                        , statModificationTime = 0
                        , statStatusChangeTime = 0 }

linkStat :: Path -> FileStat
linkStat t = FileStat   { statEntryType        = SymbolicLink
                        , statFileMode         = unionModes [ ownerReadMode
                                                            , groupReadMode
                                                            , otherReadMode ]
                        , statLinkCount        = 1
                        , statFileOwner        = 0
                        , statFileGroup        = 0
                        , statSpecialDeviceID  = 0
                        , statFileSize         = fromIntegral $ LBSC.length t
                        , statBlocks           = 1
                        , statAccessTime       = 0
                        , statModificationTime = 0
                        , statStatusChangeTime = 0 }

statNode :: NARNode -> FileStat
statNode (NFile e c) = fileStat e $ fromIntegral $ LBS.length c
statNode (NDirectory _) = dirStat
statNode (NSymlink t) = linkStat t

narGetFileStat :: RuntimeState -> FilePath -> IO (Either Errno FileStat)
narGetFileStat _ "/" = do
    return $ Right $ dirStat
narGetFileStat state x = do
  let (storepath:pathparts) = splitPath x
  hnd1 <- getNarHandle state storepath
  case hnd1 of
    Just (NARFile root) -> do
      case traverseNodes root pathparts of
        Just n -> return $ Right $ statNode n
        Nothing -> return $ Left $ eNOENT
    Nothing -> return $ Left eNOENT

narReadSymbolicLink :: RuntimeState -> FilePath -> IO (Either Errno FilePath)
narReadSymbolicLink state x = do
  let (storepath:pathparts) = splitPath x
  hnd1 <- getNarHandle state storepath
  case hnd1 of
    Just (NARFile root) -> do
      case traverseNodes root pathparts of
        Just (NSymlink t) -> return $ Right $ LBSC.unpack t
        Nothing -> return $ Left eNOENT
    Nothing -> return $ Left eNOENT

narOpen :: RuntimeState -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
narOpen state path _ _ = do
  let (storepath:pathparts) = splitPath path
  putStrLn $ "fopen " <> show storepath <> show pathparts
  hnd1 <- getNarHandle state storepath
  case hnd1 of
    Just (NARFile root) -> do
      case traverseNodes root pathparts of
        Just n -> return $ Right $ HT n
        Nothing -> return $ Left eNOENT
    Nothing -> return $ Left eNOENT

narRead :: RuntimeState -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno BS.ByteString)
narRead _ path hnd byteCount offset = do
  let pathparts = splitPath path
  let fullContents = _contents (_handle hnd)
  putStrLn $ "read " <> show pathparts <> " x " <> show (_handle hnd)
  return $ Right $ LBSC.toStrict (substr (fromIntegral offset) (fromIntegral byteCount) fullContents)
  where
    substr :: Int64 -> Int64 -> LBSC.ByteString -> LBSC.ByteString
    substr offset2 size input = LBSC.take size (LBSC.drop offset2 input)

getNarHandle :: RuntimeState -> String -> IO (Maybe NARFile)
getNarHandle state storepath = newHandle file1
  where
    go = do
      cache <- takeMVar (_NarCache state)
      --putStrLn "dumping cache"
      --print cache
      let result = Map.lookup storepath cache
      decide result cache
    file1 = _DataDir state <> "/" <> (head $ splitPath storepath) <> ".nar"
    unsafeNewHandle :: FilePath -> IO NARFile
    unsafeNewHandle file2 = decodeNARFile $ trace ("opening file " <> file2) file2
    newHandle :: FilePath -> IO (Maybe NARFile)
    newHandle file = catch
      (do
        hnd <- unsafeNewHandle file
        return $ Just hnd)
      (\x -> do
        putStrLn $ "caught:" <> show (x :: IOException)
        return Nothing
      )
    decide :: Maybe NARFile -> Map.HashMap String NARFile -> IO (Maybe NARFile)
    decide res cache = case res of
      (Just f) -> do
        putMVar (_NarCache state) cache
        return $ Just f
      (Nothing) -> do
        new_entry <- newHandle file1
        case new_entry of
          Just e -> do
            let new_cache = Map.insert storepath e cache
            putMVar (_NarCache state) new_cache
            return new_entry
          Nothing -> do
            putStrLn "open fail"
            putMVar (_NarCache state) cache
            return Nothing

narOpenDirectory :: RuntimeState -> FilePath -> IO Errno
--narOpenDirectory state "/" = return eOK
narOpenDirectory _ _ = return eOK -- TODO, check if dir is a directory

narReadDirectory :: RuntimeState -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
narReadDirectory state "/" = do
  listing <- getDirectoryContents $ _DataDir state
  return $ Right $ [(".",    dirStat),
                    ("..",   dirStat)] <> makeListing listing
  where
    makeListing listing = map (\nar -> (nar, dirStat)) $ getNARFiles listing -- FIXME, return the right stat
narReadDirectory state dir = do
  let (storepath:pathparts) = splitPath dir
  hnd1 <- getNarHandle state storepath
  --putStrLn $ "readdir " <> show pathparts
  case hnd1 of
    Just (NARFile root) -> do
      let
        cwd = traverseNodes root pathparts
      case cwd of
        Just dir -> return $ Right $ [(".",    dirStat),("..",   dirStat)] <> addStats dir
        Nothing -> return $ Left $ eNOENT
    Nothing -> return $ Left eNOENT
  where
    mapFn1 :: Name -> NARNode -> (String, FileStat)
    mapFn1 key value = (LBSC.unpack key, statNode value)

    addStats :: NARNode -> [(String, FileStat)]
    addStats (NDirectory e) = toList $ Map.mapWithKey mapFn1 e
