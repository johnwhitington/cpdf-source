SOURCES = compressor.ml

RESULT = compressor

PACKS = camlpdf

TARGETS := native-code

OCAMLFLAGS = -bin-annot
OCAMLNCFLAGS = -g -safe-string -w -3
OCAMLBCFLAGS = -g -safe-string -w -3
OCAMLLDFLAGS = -g

all : $(TARGETS)

clean ::
	rm -rf doc foo foo2 out.pdf out2.pdf *.cmt *.cmti *.zlib

-include OCamlMakefile
