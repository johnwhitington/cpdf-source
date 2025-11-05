% CPDF(1)
% John Whitington
% November 2025

# NAME

cpdf - PDF command line tools

# SYNOPSIS

**cpdf** in.pdf \[operation] \[options] \[-o out.pdf]

FIXME mention AND here.

# DESCRIPTION

**cpdf** is an AGPL-licensed command line tool for processing PDF files. The
rest of this man page gives a brief description of each command line operation
and option. The file cpdfmanual.pdf which you should find installed or
otherwise at https://www.coherentpdf.com/cpdfmanual.pdf gives the full usage
details.

The sections in this man page follow the chapters of cpdfmanual.pdf.

# 1. BASIC USAGE

**-help, --help**

: Lists Cpdf's operations and options, very briefly.

**-version**

: Print the Cpdf version number

**-o**

: The output filename. Beware of writing back to the same file.

**-i**

: Cpdf automatically treats any filename ending with .pdf (any case) as an
input PDF. If your file does not end with .pdf, you can supply the file with
**-i** instead.

**-range**

: a

**-progress**

: a

**-keep-version**

: a

**-fast**

: a

**-idir <directory>**

: Add a whole directory of PDFs as inputs.

**-idir-only-pdfs**

: Restrict **-idir** to only files ending in .pdf (any case).

**-recrypt**

: a

**-decrypt-force**

: a

**-stdout**

: a

**-stdin**

: a

**-stdin-user <password>**

: a

**-stdin-owner <password>**

: a

**-producer <text>**

: a

**-creator <text>**

: a

**-change-id**

: a

**-l**

: a

**-cpdflin <filename>**

: a

**-keep-l**

: a

**-no-preserve-objstm**

: a

**-create-objstm**

: a

**-args <filename>**

: a

**-args-json <filename>**

: a

**-utf8**

: a

**-stripped**

: a

**-raw**

: a

**-gs**

: a

**-gs-malformed**

: a

**-gs-malformed-force**

: a

**-gs-quiet**

: a

**-error-on-malformed**

: a

# 2. MERGING AND SPLITTING

**cpdf -merge in1.pdf** \[\<range>] **in2.pdf** \[\<range>] \[\<more names/ranges>] \[-collate] \[-collate-n \<n>] \[-retain-numbering] \[-merge-add-bookmarks \[-merge-add-bookmarks-use-titles]] \[-remove-duplicate-fonts] \[-process-struct-trees] \[-subformat \<subformat>] **-o out.pdf**

: a

cpdf -portfolio in.pdf -pf \<filename> \[-pfd <string>] \[-pfr \<relationship>] \[-pf ...] -o out.pdf

: a

cpdf -split in.pdf \[-chunk \<chunksize>] \[-process-struct-trees] -o \<format>

: a

cpdf -split-bookmarks \<level> in.pdf \[-utf8] \[-process-struct-trees] -o \<format>

: a

cpdf -split-max \<file size> in.pdf \[-process-struct-trees] -o \<format>

: a

cpdf -spray in.pdf \[-process-struct-trees] -o a.pdf \[-o b.pdf \[-o ...]]

: a

# 3. PAGES

cpdf -scale-page "\<scale x> \<scale y>" \[-fast] \[<position>] in.pdf \[\<range>] -o out.pdf

: a

cpdf -scale-to-fit "\<x size> \<y size>" \[-fast] \[-prerotate] \[\<position>] \[-scale-to-fit-scale \<scale>] \[-scale-to-fit-rotate-clockwise] \[-scale-to-fit-rotate-anticlockwise] in.pdf \[\<range>] -o out.pdf

: a

cpdf -stretch "\<x size> \<y size>" \[-fast] in.pdf \[\<range>] -o out.pdf

: a

cpdf -center-to-fit "\<x size> \<y size>" in.pdf \[\<range>] -o out.pdf

: a

cpdf -scale-contents \[\<scale>] \[<position>] \[-fast] in.pdf \[\<range>] -o out.pdf

: a

cpdf -shift "\<shift x> \<shift y>" \[-fast] in.pdf \[\<range>] -o out.pdf

: a

cpdf -shift-boxes "\<shift x> \<shift y>" in.pdf \[\<range>] -o out.pdf

: a

cpdf -rotate \<angle> in.pdf \[\<range>] -o out.pdf

: a

cpdf -rotateby \<angle> in.pdf \[\<range>] -o out.pdf

: a

cpdf -upright \[-fast] in.pdf \[\<range>] -o out.pdf

: a

cpdf -rotate-contents \<angle> \[-fast] in.pdf \[\<range>] -o out.pdf

: a

cpdf -hflip \[-fast] in.pdf \[\<range>] -o out.pdf

: a

cpdf -vflip \[-fast] in.pdf \[\<range>] -o out.pdf

: a

cpdf -\[media|crop|art|trim|bleed]box \<boxspec> in.pdf \[\<range>] -o out.pdf

: a

cpdf -remove-\[crop|art|trim|bleed]box in.pdf \[\<range>] -o out.pdf

: a

cpdf -frombox \<boxname> -tobox \<boxname> \[-mediabox-if-missing] in.pdf \[\<range>] -o out.pdf

: a

cpdf -hard-box \<boxname> \[-fast] in.pdf \[\<range>] \[-mediabox-if-missing] -o out.pdf

: a

cpdf -show-boxes \[-fast] in.pdf \[\<range>] -o out.pdf

: a

cpdf -trim-marks \[-fast] in.pdf \[\<range>] -o out.pdf

: a

# 4. ENCRYPTION AND DECRYPTION

cpdf -encrypt \<method> \[-pw=]\<owner> \[-pw=]\<user> \[-no-encrypt-metadata] \<permissions> in.pdf -o out.pdf

: a

-no-edit
-no-print
-no-copy
-no-annot
-no-forms
-no-extract
-no-assemble
-no-hq-print

cpdf -decrypt \[-decrypt-force] in.pdf owner=\<owner password> -o out.pdf

: a

# 5. COMPRESSION

cpdf -decompress \[-just-content] in.pdf -o out.pdf

: a

-jbig2dec

: a

cpdf -compress in.pdf -o out.pdf

: a

cpdf -squeeze in.pdf \[-squeeze-log-to \<filename>] \[-squeeze-no-recompress] \[-squeeze-no-pagedata] -o out.pdf

: a

cpdf -remove-article-threads in.pdf -o out.pdf

: a

cpdf -remove-page-piece in.pdf -o out.pdf

: a

cpdf -remove-web-capture in.pdf -o out.pdf

: a

cpdf -remove-procsets in.pdf -o out.pdf

: a

cpdf -remove-output-intents in.pdf -o out.pdf

: a

# 6. BOOKMARKS

cpdf -list-bookmarks \[-utf8] in.pdf

: a

cpdf -list-bookmarks-json \[-preserve-actions] in.pdf

: a

cpdf -remove-bookmarks in.pdf -o out.pdf

: a

cpdf -add-bookmarks \<bookmark file> in.pdf -o out.pdf

: a

cpdf -add-bookmarks-json \<bookmark file> in.pdf -o out.pdf

: a

cpdf -bookmarks-open-to-level \<n> in.pdf -o out.pdf

: a

cpdf -table-of-contents \[-toc-title] \[-toc-no-bookmark] \[-toc-dot-leaders] \[-font <font>] \[-font-size <size>] \[-embed-std14 /path/to/fonts] \[-process-struct-trees] \[-subformat <subformat>] in.pdf -o out.pdf

: a

# 7. PRESENTATIONS

cpdf -presentation in.pdf \[\<range>] -o out.pdf \[-trans \<transition-name>] \[-duration \<float>] \[-vertical] \[-outward] \[-direction \<int>] \[-effect-duration \<float>]

: a

# 8. WATERMARKS AND STAMPS

cpdf -stamp-on source.pdf [-scale-stamp-to-fit] [<positioning command>] [-relative-to-cropbox] [-process-struct-trees] in.pdf [<range>] [-fast] -o out.pdf

: a

cpdf -stamp-under source.pdf [-scale-stamp-to-fit] [<positioning command>] [-relative-to-cropbox] [-process-struct-trees] in.pdf [<range>] [-fast] -o out.pdf

: a

cpdf -combine-pages over.pdf under.pdf [-fast] [-prerotate] [-no-warn-rotate] [-process-struct-trees] [-underneath] [-stamp-scale-to-fit] -o out.pdf

: a

cpdf (\[-add-text \<text-format> | -add-rectangle \<size>]) [-font \<fontname>] [-font-size \<size-in-points>] [-load-ttf \<name>=\<file>] [-embed-std14] [-color \<color>] [-line-spacing \<number>] [-outline] [-linewidth \<number>] [-underneath] [-relative-to-cropbox] [-prerotate] [-no-warn-rotate] [-bates \<number>] [-bates-at-range \<number>] [-bates-pad-to \<number>] [-opacity \<number>] [-midline] [-topline] [-fast] [-process-struct-trees] in.pdf \[\<range>] -o out.pdf

: a

Positioning commands for -add-text and -add-rectangle:

-pos-left

:a

-pos-center

:a

-pos-right

:a

-top 10

:a

-topleft 10

:a

-topleft "10 20"

:a

-topright 10

:a

-topright "10 20"

:a

-left 10

:a

-bottomleft 10

:a

-bottomleft "10 20"

:a

-bottom 10

:a

-bottomright 10

:a

-bottomright "10 20"

:a

-right 10

:a

-diagonal

:a

-reverse-diagonal

:a

-center

:a

-justify-left

:a

-justify-right

:a

-justify-center

:a

cpdf -remove-text in.pdf \[\<range>] -o out.pdf

: a

cpdf -prepend-content \<content> in.pdf \[\<range>] -o out.pdf

: a

cpdf -postpend-content \<content> in.pdf \[\<range>] -o out.pdf

: a

cpdf -stamp-as-xobject stamp.pdf in.pdf \[\<range>] -o out.pdf

: a

# 9. MULTIPAGE FACILITIES

cpdf -pad-before in.pdf \[\<range>] \[-pad-with pad.pdf] -o out.pdf

: a

cpdf -pad-after in.pdf \[\<range>] \[-pad-with pad.pdf] -o out.pdf

: a

cpdf -pad-every \[\<integer>] in.pdf \[-pad-with pad.pdf] -o out.pdf

: a

cpdf -pad-multiple \[\<integer>] in.pdf -o out.pdf

: a

cpdf -pad-multiple-before \[\<integer>] in.pdf -o out.pdf

: a

cpdf -redact \[-process-struct-trees] in.pdf \[\<range>] -o out.pdf

: a

cpdf \[-impose \<pagesize> | impose-xy "\<x> \<y>"] \[-impose-columns] \[-impose-rtl] \[-impose-btt] \[-impose-margin <margin>] \[-impose-spacing \<spacing>] \[-impose-linewidth \<width>] \[-fast] \[-process-struct-trees] in.pdf -o out.pdf

: a

cpdf -twoup-stack \[-fast] \[-process-struct-trees] in.pdf -o out.pdf

: a

cpdf -twoup \[-fast] \[-process-struct-trees] in.pdf -o out.pdf

: a

cpdf -chop "\<x> \<y>" \[-chop-columns] \[-chop-rtl] \[-chop-btt] in.pdf \[\<range>] -o out.pdf

: a

cpdf \[-chop-h \<y> | -chop-v \<x>] \[-chop-columns] in.pdf \[\<range>] -o out.pdf

# 10. ANNOTATIONS

cpdf -list-annotations in.pdf \[\<range>]

: a

cpdf -list-annotations-json in.pdf \[\<range>]

: a

cpdf -set-annotations \<filename> \[-underneath] in.pdf \[\<range>] -o out.pdf

: a

cpdf -copy-annotations from.pdf to.pdf \[\<range>] -o out.pdf

: a

cpdf -remove-annotations in.pdf \[\<range>] -o out.pdf

: a

# 11. DOCUMENT INFORMATION AND METADATA

cpdf -info\[-json] \[-utf8] \[-in | -cm | -mm] in.pdf

: a

cpdf -page-info\[-json] \[-in | -cm | -mm] in.pdf \[\<range>]

: a

cpdf -pages in.pdf

: a

cpdf -set-title \<title of document> \[-also-set-xmp] \[-just-set-xmp] in.pdf -o out.pdf

: a

cpdf -set-author \<author of document> \[-also-set-xmp] \[-just-set-xmp] in.pdf -o out.pdf

: a

cpdf -set-subject \<subject of document> \[-also-set-xmp] \[-just-set-xmp] in.pdf -o out.pdf

: a

cpdf -set-keywords \<keywords of document> \[-also-set-xmp] \[-just-set-xmp] in.pdf -o out.pdf

: a

cpdf -set-creator \<creator of document> \[-also-set-xmp] \[-just-set-xmp] in.pdf -o out.pdf

: a

cpdf -set-producer \<producer of document> \[-also-set-xmp] \[-just-set-xmp] in.pdf -o out.pdf

: a

cpdf -set-create \<date> \[-also-set-xmp] \[-just-set-xmp] in.pdf -o out.pdf

: a

cpdf -set-modify \<date> \[-also-set-xmp] \[-just-set-xmp] in.pdf -o out.pdf

: a

cpdf -set-trapped in.pdf -o out.pdf \[-also-set-xmp] \[-just-set-xmp] 

: a

cpdf -set-untrapped in.pdf -o out.pdf \[-also-set-xmp] \[-just-set-xmp]

: a

cpdf -set-page-layout \<layout> in.pdf -o out.pdf

: a

cpdf -set-page-mode \<mode> in.pdf -o out.pdf

: a

cpdf -set-non-full-screen-page-mode \<mode> in.pdf -o out.pdf

: a

cpdf -hide-toolbar \<true | false> in.pdf -o out.pdf

: a

cpdf -hide-menubar \<true | false> in.pdf -o out.pdf

: a

cpdf -hide-window-ui \<true | false> in.pdf -o out.pdf

: a

cpdf -fit-window \<true | false> in.pdf -o out.pdf

: a

cpdf -center-window \<true | false> in.pdf -o out.pdf

: a

cpdf -display-doc-title \<true | false> in.pdf -o out.pdf

: a

cpdf -open-at-page \<page number> in.pdf -o out.pdf

: a

cpdf -open-at-page-fit \<page number> in.pdf -o out.pdf

: a

cpdf -open-at-page-custom \<destination> in.pdf -o out.pdf

: a

cpdf -set-language \<language> in.pdf -o out.pdf

: a

cpdf -set-metadata \<metadata-file> in.pdf -o out.pdf

: a

cpdf -remove-metadata in.pdf -o out.pdf

: a

cpdf -remove-all-metadata in.pdf -o out.pdf

: a

cpdf -print-metadata in.pdf

: a

cpdf -extract-all-metadata in.pdf -o <directory>

: a

cpdf -create-metadata in.pdf -o out.pdf

: a

cpdf -set-metadata-date \<date> in.pdf -o out.pdf

: a

cpdf -add-page-labels in.pdf -o out.pdf \[-label-style \<style>] \[-label-prefix <string>] \[-label-startval <integer>] \[-labels-progress]

: a

cpdf -remove-page-labels in.pdf -o out.pdf

: a

cpdf -print-page-labels\[-json] in.pdf

: a

cpdf -composition\[-json] in.pdf

# 12. FILE ATTACHMENTS

cpdf -attach-file \<filename> \[-to-page \<page number>] \[-afd \<string>] \[-afr \<relationship>] \[-attach-file ...] in.pdf -o out.pdf

: a

cpdf -list-attached-files \[-json] \[-include-data] in.pdf

: a

cpdf -remove-files in.pdf -o out.pdf

: a

cpdf -dump-attachments in.pdf -o \<directory>

: a

# 13. IMAGES

cpdf -list-images\[-json] \[-inline] in.pdf \[\<range>]

: a

cpdf -image-resolution\[-json] \<n> \[-inline] in.pdf \[\<range>]

: a

cpdf -list-images-used\[-json] \[-inline] in.pdf \[\<range>]

: a

cpdf -extract-images in.pdf \[\<range>] \[-im <path>] \[-p2p <path>] \[-dedup | -dedup-perpage] \[-raw] \[-inline] \[-merge-masks] -o \<path>

: a

cpdf -extract-single-image \<object number> \[-im \<path>] \[-p2p <path>] \[-raw] \[-merge-masks] in.pdf -o \<filename>

: a

cpdf -process-images \[-process-images-info] in.pdf \[\<range>] \[-im \<filename>] [-jbig2enc \<filename>] [-lossless-resample[-dpi] \<n> | -lossless-to-jpeg \<n>] \[-jpeg-to-jpeg \<n>] \[-jpeg-to-jpeg-scale \<n>] \[-lossless-to-jpeg2000 \<n>] \[-jpeg2000-to-jpeg2000 \<n>] \[-jpeg-to-jpeg-dpi \<n>] \[-1bpp-method \<method>] \[-jbig2-lossy-threshold \<n>] \[-pixel-threshold \<n>] \[-length-threshold \<n>] \[-percentage-threshold \<n>] \[-dpi-threshold \<n>] \[-resample-interpolate] -o out.pdf

: a

cpdf -rasterize in.pdf \[\<range>] -o out.pdf \[-rasterize\[-gray|-1bpp|-jpeg|-jpeggray] \[-rasterize-res \<n>] \[-rasterize-jpeg-quality \<n>] \[-rasterize-no-antialias | -rasterize-downsample] \[-rasterize-annots] | \[-rasterize-alpha]

: a

cpdf -output-image in.pdf \[\<range>] -o \<format> \[-rasterize\[-gray|-1bpp|-jpeg|-jpeggray] \[-rasterize-res <n>] \[-rasterize-jpeg-quality <n>] \[-rasterize-no-antialias | -rasterize-downsample] \[-rasterize-annots] \[-rasterize-alpha] \[-tobox <BoxName>]

# 14. FONTS

cpdf -list-fonts\[-json] in.pdf

: a

cpdf -print-font-table \<font name> -print-font-table-page \<n> in.pdf

: a

cpdf -copy-font fromfile.pdf -copy-font-page \<int> -copy-font-name \<name> in.pdf \[\<range>] -o out.pdf

: a

cpdf -remove-fonts in.pdf -o out.pdf

: a

cpdf -missing-fonts in.pdf

: a

cpdf -embed-missing-fonts -gs \<path to gs> in.pdf -o out.pdf

: a

cpdf -extract-font \<page number>,\<pdf font name> in.pdf -o out.font

: a

# 15. PDF AND JSON

cpdf in.pdf -output-json -o out.json \[-output-json-parse-content-streams] \[-output-json-no-stream-data] \[-output-json-decompress-streams] \[-output-json-clean-strings] \[-utf8]

: a

cpdf -j in.json -o out.pdf

: a

# 16. OPTIONAL CONTENT GROUPS

cpdf -ocg-list in.pdf

: a

cpdf -ocg-rename -ocg-rename-from \<a> -ocg-rename-to \<b> in.pdf -o out.pdf

: a

cpdf -ocg-order-all in.pdf -o out.pdf

: a

cpdf -ocg-coalesce-on-name in.pdf -o out.pdf

# 17. CREATING NEW PDFS

cpdf -create-pdf \[-create-pdf-pages \<n>] \[-create-pdf-papersize \<paper size>] -o out.pdf

: a

cpdf -typeset \<text file> \[-create-pdf-papersize \<size>] \[-font \<font>] \[-font-size \<size>] \[-subformat \<subformat>] \[-title \<string>] -o out.pdf

: a

cpdf \[-subformat \<subformat>] \[-title \<string>] -jpeg \<filename> -o out.pdf

: a

cpdf \[-subformat \<subformat>] \[-title \<string>] -png \<filename> -o out.pdf

: a

cpdf \[-subformat \<subformat>] \[-title \<string>] -jpeg2000 \<filename> -o out.pdf

: a

cpdf \[-subformat \<subformat>] \[-title \<string>] \[-jbig2-global \<filename>] -jbig2 \<filename> \[-jbig2-global | -jbig2-global-clear] \[-jbig2 \<filename>] ...  -o out.pdf

: a

# 18. DRAWING ON PDFS

cpdf in.pdf [<range>] [-draw-struct-tree] [-underneath] -draw <draw operations> -o out.pdf

cpdf -text-width <text> [-font <font>] [-fontsize <fontsize>]

Building and showing paths

-rect

: Draw rectangle

-to

: Move to

-line

: Add line to path

-bez

: Add Bezier curve to path

-bez23

: Add Bezier curve to path

-bez13

: Add Bezier curve to path

-circle

: Add circle to path

-stroke

: Stroke path

-fill

: Fill path

-filleo

: Fill path, even odd

-strokefill

: Stroke and fill path

-strokefilleo

: Stroke and fill path, even odd

-close

: Close path

Clipping with paths

-clip

: Clip

-clipeo

: Clip, even odd

Path parameters

-strokecol

: Set stroke colour

-fillcol

: Set fill colour

-thick

: Set stroke thickness

-cap

: Set cap

-join

: Set join

-miter

: Set miter limit

-dash

: Set dash pattern

The graphics stack and matrices

-push

: Push graphics stack

-pop

: Pop graphics stack

-matrix

: Append to graphics matrix

-mtrans

: Translate the graphics matrix

-mrot

: Rotate the graphics matrix

-mscale

: Scale the graphics matrix

-mshearx

: Shear the graphics matrix in X

-msheary

: Shear the graphics matrix in Y

Re-use with XObjects

-xobj-bbox

: Specify the bounding box for xobjects

-xobj

: Begin saving a sequence of graphics operators

-end-xobj

: End saving a sequence of graphics operators

-use

: Use a saved sequence of graphics operators

Images

-draw-jpeg

: Load a JPEG from file and name it

-draw-png

: Load a PNG from file and name it

-draw-jpeg2000

: Load a JPEG2000 from file and name it

-image

: Draw an image which has already been loaded

Transparency

-fill-opacity

: Set opacity

-stroke-opacity

: Set stroke opacity

Text

-bt

: Begin text

-et

: End text

-text

: Draw text

-stext

: Draw text with %specials

-para

: Typeset a paragraph

-paras

: Typeset multiple paragraphs

-indent

: a

-leading

: Set leading

-charspace

: Set character spacing

-wordspace

: Set word space

-textscale

: Set text scale

-rendermode

: Set text rendering mode

-rise

: Set text rise

-nl

: New line

The next page

-newpage

: Move to a fresh page

Structure Information

-draw-struct-tree

: Add structure information

-tag

: Begin marked content

-end-tag

: End marked content

-stag

: Begin structure tree branch

-end-stag

: End structure tree branch

-auto-tags

: Automatically tag paragraphs and images

-no-auto-tags

: Refrain from automatically tagging paragraphs and images

-artifact

: Begin manual artifact

-end-artifact

: End manual artifact

-no-auto-artifacts

: Prevent automatic addition of artifacts during postprocessing

-namespace

: Set the namespace for future branches of the tree

-eltinfo

: Set element information

-end-eltinfo

: Erase element information

-rolemap

: Set role map

# 19. ACCESSIBLE PDFS WITH PDF/UA

cpdf -print-struct-tree in.pdf

: a

cpdf -extract-struct-tree in.pdf -o out.json

: a

cpdf -replace-struct-tree in.json in.pdf -o out.pdf

: a

cpdf -remove-struct-tree in.pdf -o out.pdf

: a

cpdf -mark-as-artifact in.pdf -o out.pdf

: a

cpdf -verify "PDF/UA-1(matterhorn)" \[-json] in.pdf

: a

cpdf -verify "PDF/UA-1(matterhorn)" -verify-single \<test> \[-json] in.pdf

: a

cpdf -mark-as \["PDF/UA-1" | "PDF/UA-2"] in.pdf -o out.pdf

: a

cpdf -remove-mark \["PDF/UA-1" | "PDF/UA-2"] in.pdf -o out.pdf

: a

cpdf -create-pdf-ua-\<1|2> \<title> \[-create-pdf-pages \<n>] \[-create-pdf-papersize \<paper size>] -o out.pdf

# 20. MISCELLANEOUS

cpdf -draft \[-boxes] \[-draft-remove-only \<n>] in.pdf \[\<range>] -o out.pdf

: a

cpdf -remove-all-text in.pdf \[\<range>] -o out.pdf

: a

cpdf -blacktext in.pdf \[\<range>] -o out.pdf

: a

cpdf -blacklines in.pdf \[\<range>] -o out.pdf

: a

cpdf -blackfills in.pdf \[\<range>] -o out.pdf

: a

cpdf -thinlines \<min thickness> in.pdf \[\<range>] -o out.pdf

: a

cpdf -clean in.pdf -o out.pdf

: a

cpdf -set-version \<version number> in.pdf -o out.pdf

: a

cpdf -copy-id-from source.pdf in.pdf -o out.pdf

: a

cpdf -remove-id in.pdf -o out.pdf

: a

cpdf -list-spot-colors in.pdf

: a

cpdf -print-dict-entry [-json] \<key> in.pdf

: a

cpdf -remove-dict-entry \<key> [-dict-entry-search \<term>] in.pdf -o out.pdf

: a

cpdf -replace-dict-entry \<key> -replace-dict-entry-value \<value> \[-dict-entry-search \<term>] in.pdf -o out.pdf

: a

cpdf -remove-clipping \[\<range>] in.pdf -o out.pdf

: a

cpdf -obj[-json] \<object specification> in.pdf

: a

cpdf -replace-obj \<object specification>=\<object> in.pdf -o out.pdf

: a

cpdf -remove-obj \<object number> in.pdf -o out.pdf

: a

cpdf -extract-stream\[-decompress] \<object specification> in.pdf \[-o out.dat | -stdout]

: a

cpdf -replace-stream \<object specification> -replace-stream-with \<filename> in.pdf -o out.pdf

: a

cpdf -contains-javascript in.pdf

: a

cpdf -remove-javascript in.pdf -o out.pdf

: a

# ENVIRONMENT

# BUGS

# AUTHOR

John Whitington <john at coherentgraphics dot co dot uk>
