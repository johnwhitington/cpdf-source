# Build the cpdf command line tools and top level
SOURCES = cpdfstrftime.mli cpdfstrftime.ml cpdf.mli cpdf.ml cpdfcommand.mli \
cpdfcommand.ml cpdfcommandrun.ml

RESULT = cpdf

PACKS = camlpdf

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : native-code top htdoc

-include OCamlMakefile

