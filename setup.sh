oasis setup -setup-update dynamic
ocaml setup.ml -configure --bindir $(opam config var bin)
mv Makefile.bak Makefile
