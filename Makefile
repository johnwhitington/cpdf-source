# Build the cpdf command line tools and top level
MODS = cpdfstrftime cpdf cpdfcommand

SOURCES = cst2constr.h cst2constr.c unixsupport.h unixsupport.c gmtime.c gettimeofday.c cpdfunix.ml cpdfunix.mli $(foreach x,$(MODS),$(x).ml $(x).mli) cpdfcommandrun.ml

RESULT = cpdf
ANNOTATE = true
PACKS = camlpdf

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : native-code native-code-library byte-code-library top htdoc

LIBINSTALL_FILES = cpdf.a cpdf.cma cpdf.cmxa libcpdf_stubs.a \
dllcpdf_stubs.* $(foreach x,$(MODS),$x.mli) $(foreach x,$(MODS),$x.cmi)

install : libinstall

-include OCamlMakefile

