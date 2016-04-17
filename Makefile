Main: Main.hs
	ghc -O2 Main.hs -rtsopts -threaded
do_test: Main
	./Main ../input.nar
do_test2: Main
	./Main ../input.nar +RTS -t --machine-readable -s
do_test3: Main
	./Main ../input.nar +RTS -s
do_test4: Main
	./Main ../container_data/ikzdbd65z7453spdvm05r0izd56zdvkx-gcc-4.9.3.nar
install:
	mkdir -p ${out}/bin
	cp Main ${out}/bin/narparser
