# Build the cpdf command line tools and top level
NONDOC = cpdfyojson cpdfxmlm cpdfutil

DOC = cpdfunicodedata cpdferror cpdfdebug cpdfjson cpdfstrftime cpdfcoord \
      cpdfattach cpdfpagespec cpdfposition cpdfpresent cpdfmetadata \
      cpdfbookmarks cpdfpage cpdftruetype cpdfremovetext cpdfextracttext \
      cpdfembed cpdfaddtext cpdfimage cpdffont cpdftype cpdfpad cpdfocg \
      cpdfsqueeze cpdfdraft cpdfspot cpdfpagelabels cpdfcreate cpdfannot \
      cpdfxobject cpdfimpose cpdftweak cpdftexttopdf cpdftoc cpdfjpeg \
      cpdfdraw cpdfcommand

MODS = $(NONDOC) $(DOC)

SOURCES = $(foreach x,$(MODS),$(x).ml $(x).mli) cpdfcommandrun.ml

RESULT = cpdf
ANNOTATE = true
PACKS = camlpdf

CFLAGS = -fPIC
OCAMLFLAGS = -bin-annot
OCAMLNCFLAGS = -g -safe-string
OCAMLBCFLAGS = -g -safe-string
OCAMLLDFLAGS = -g

TARGETS := byte-code-library top htdoc

LIBINSTALL_FILES = cpdf.cma \
$(foreach x,$(MODS),$x.mli) $(foreach x,$(MODS),$x.cmi) \
$(foreach x,$(MODS),$x.cmti)

ifneq ($(shell ocamlopt -version),)
  TARGETS += native-code native-code-library
  LIBINSTALL_FILES += cpdf.a cpdf.cmxa $(foreach x,$(MODS),$x.cmx)
endif

all : $(TARGETS)

clean ::
	rm -rf doc foo foo2 out.pdf out2.pdf foo.pdf decomp.pdf *.cmt *.cmti \
	*.json test/*.pdf debug/*.pdf *.ps *.aux *.idx *.log *.out *.toc *.cut \
	*.ttf *.ttx

DOC_FILES = $(foreach x,$(DOC),$(x).mli )

install : libinstall

-include OCamlMakefile
