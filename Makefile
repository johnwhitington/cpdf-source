# Build the cpdf command line tools
NONDOC = cpdfyojson cpdfxmlm

DOC = cpdfutil cpdfunicodedata cpdferror cpdfdebug cpdfjson cpdfstrftime \
      cpdfcoord cpdfattach cpdfpagespec cpdfposition cpdfpresent cpdfmetadata \
      cpdfbookmarks cpdfpage cpdftruetype cpdfremovetext cpdfextracttext \
      cpdfembed cpdffont cpdftype cpdfaddtext cpdfpad cpdfocg cpdfsqueeze \
      cpdfdraft cpdfspot cpdfpagelabels cpdfcreate cpdfannot cpdfxobject \
      cpdfimpose cpdfchop cpdftweak cpdfprinttree cpdfua cpdftexttopdf \
      cpdftoc cpdfjpeg cpdfjpeg2000 cpdfpng cpdfimage cpdfdraw \
      cpdfcomposition cpdfshape cpdfcolours cpdfdrawcontrol cpdfform cpdfjs \
      cpdfcommand

MODS = $(NONDOC) $(DOC)

SOURCES = $(foreach x,$(MODS),$(x).ml $(x).mli) cpdfcommandrun.ml

RESULT = cpdf
ANNOTATE = true
PACKS = camlpdf

CFLAGS = -fPIC -g
OCAMLFLAGS = -bin-annot
OCAMLNCFLAGS = -g -safe-string
OCAMLBCFLAGS = -g -safe-string
OCAMLLDFLAGS = -g

TARGETS := byte-code byte-code-library htdoc

LIBINSTALL_FILES = cpdf.cma \
$(foreach x,$(MODS),$x.mli) $(foreach x,$(MODS),$x.cmi) \
$(foreach x,$(MODS),$x.cmti)

ifneq ($(shell ocamlopt -version),)
  TARGETS += native-code native-code-library
  LIBINSTALL_FILES += cpdf.a cpdf.cmxa $(foreach x,$(MODS),$x.cmx)
endif

all : $(TARGETS)

clean ::
	rm -rf doc foo foo2 out.pdf out2.pdf out3.pdf foo.pdf decomp.pdf *.cmt \
	*.cmti *.json test/*.pdf *.ps *.aux *.idx *.log *.out *.toc *.cut \
	*.ttf *.ttx out.png cpdf.dSYM

DOC_FILES = $(foreach x,$(DOC),$(x).mli)

install : libinstall

-include OCamlMakefile
