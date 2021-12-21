# Build the cpdf command line tools and top level
MODS = cpdfyojson cpdfxmlm \
       cpdfunicodedata cpdferror cpdfdebug cpdfjson cpdfstrftime cpdfcoord \
       cpdfattach cpdfpagespec cpdfposition cpdfpresent cpdfmetadata \
       cpdfbookmarks cpdfpage cpdfaddtext cpdf cpdfimage cpdffont cpdftype \
       cpdftexttopdf cpdftoc cpdfpad cpdfocg cpdfsqueeze cpdfspot \
       cpdfpagelabels cpdfcreate cpdfannot cpdfcommand

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
	rm -rf doc foo foo2 out.pdf out2.pdf foo.pdf decomp.pdf *.cmt *.cmti \
	*.json test/*.pdf debug/*.pdf *.ps

DOC_FILES = cpdferror.mli cpdfjson.mli cpdfstrftime.mli cpdfcoord.mli \
            cpdfattach.mli cpdfpagespec.mli cpdfposition.mli cpdf.mli \
	    cpdfcommand.mli

LIBINSTALL_FILES = cpdf.a cpdf.cma cpdf.cmxa \
$(foreach x,$(MODS),$x.mli) $(foreach x,$(MODS),$x.cmi) \
$(foreach x,$(MODS),$x.cmx) $(foreach x,$(MODS),$x.cmti)

install : libinstall

-include OCamlMakefile
