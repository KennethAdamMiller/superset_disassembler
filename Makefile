SETUP=LDFLAGS=-L/usr/local/lib LIBRARY_PATH=/opt/local/lib ocaml setup.ml

default: all

all:
	oasis setup -setup-update dynamic
	touch setup.data
	$(SETUP) -build -cflag -annot -cflag -bin-annot

profile:
	ocaml setup.ml -build -cflag -annot -cflag -bin-annot -tag profile -tag debug

install:
	oasis setup -setup-update dynamic
	$(SETUP) -configure --bindir $(shell opam config var bin)
	$(SETUP) -install

clean:
	$(SETUP) -clean
