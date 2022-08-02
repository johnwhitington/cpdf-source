# Build the cpdf command line tools and top level
MODS = cpdfyojson cpdfxmlm cpdfutil \
       cpdfunicodedata cpdferror cpdfdebug cpdfjson cpdfstrftime cpdfcoord \
       cpdfattach cpdfpagespec cpdfposition cpdfpresent cpdfmetadata \
       cpdfbookmarks cpdfpage cpdfaddtext cpdfimage cpdffont cpdftype \
       cpdftexttopdf cpdftoc cpdfpad cpdfocg cpdfsqueeze cpdfdraft cpdfspot \
       cpdfpagelabels cpdfcreate cpdfannot cpdfxobject cpdfimpose cpdftweak \
       cpdfcommand

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
  LIBINSTALL_FILES += cpdf.a cpdf.cmxa $(foreach x,$(PDFMODS),$x.cmx)
endif

all : $(TARGETS)

clean ::
	rm -rf doc foo foo2 out.pdf out2.pdf foo.pdf decomp.pdf *.cmt *.cmti \
	*.json test/*.pdf debug/*.pdf *.ps *.aux *.idx *.log *.out *.toc *.cut

DOC_FILES = cpdfunicodedata.mli cpdferror.mli cpdfdebug.mli cpdfjson.mli \
            cpdfstrftime.mli cpdfcoord.mli cpdfattach.mli cpdfpagespec.mli \
	    cpdfposition.mli cpdfpresent.mli cpdfmetadata.mli \
	    cpdfbookmarks.mli cpdfpage.mli cpdfaddtext.mli cpdfimage.mli \
	    cpdffont.mli cpdftype.mli cpdftexttopdf.mli cpdftoc.mli \
	    cpdfpad.mli cpdfocg.mli cpdfsqueeze.mli cpdfdraft.mli \
            cpdfspot.mli cpdfpagelabels.mli cpdfcreate.mli cpdfannot.mli \
	    cpdfxobject.mli cpdfimpose.mli cpdftweak.mli cpdfcommand.mli

install : libinstall

-include OCamlMakefile
