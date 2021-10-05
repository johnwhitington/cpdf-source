# Build the cpdf command line tools and top level
MODS = cpdfyojson cpdfstream tjutil tjutf16 tjllist tjparsermonad tjjson \
       cpdfxmlm cpdferror cpdfjson cpdfstrftime cpdfcoord cpdfattach \
       cpdfpagespec cpdfposition cpdf cpdfcommand

SOURCES = $(foreach x,$(MODS),$(x).ml $(x).mli) cpdfcommandrun.ml

RESULT = cpdf
ANNOTATE = true
PACKS = camlpdf

CFLAGS = -fPIC
OCAMLFLAGS = -bin-annot
OCAMLNCFLAGS = -g -safe-string -w -3
OCAMLBCFLAGS = -g -safe-string -w -3
OCAMLLDFLAGS = -g

all : native-code native-code-library byte-code-library top htdoc

clean ::
	rm -rf doc foo foo2 out.pdf out2.pdf *.cmt *.cmti *.json

LIBINSTALL_FILES = cpdf.a cpdf.cma cpdf.cmxa \
$(foreach x,$(MODS),$x.mli) $(foreach x,$(MODS),$x.cmi) \
$(foreach x,$(MODS),$x.cmx) $(foreach x,$(MODS),$x.cmti)

install : libinstall

-include OCamlMakefile
