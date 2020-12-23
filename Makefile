DUNE := opam exec -- dune

bcotp:
	$(DUNE) build
	echo "\nYou may find a binary in _build/default/bin/"

clean:
	$(DUNE) clean
