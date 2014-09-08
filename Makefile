# Build the cpdf command line tools and top level
MODS = cpdfstrftime cpdf cpdfcommand

SOURCES = $(foreach x,$(MODS),$(x).ml $(x).mli) cpdfcommandrun.ml

RESULT = cpdf
ANNOTATE = true
PACKS = camlpdf xml-light

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : native-code native-code-library byte-code-library top htdoc

LIBINSTALL_FILES = cpdf.a cpdf.cma cpdf.cmxa \
$(foreach x,$(MODS),$x.mli) $(foreach x,$(MODS),$x.cmi)

install : libinstall

-include OCamlMakefile

