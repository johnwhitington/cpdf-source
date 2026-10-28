# Creating New PDFs

    cpdf -create-pdf [-create-pdf-pages <n>]
         [-create-pdf-papersize <paper size>] -o out.pdf

    cpdf -typeset <text file> [-create-pdf-papersize <size>]
         [-font <font>] [-font-size <size>] [-color <color>] [-margin <n>]
         [-opacity <n>] [-line-spacing <n>] [-outline] [-linewidth <n>]
         [-subformat <subformat>] [-title <string>] -o out.pdf

    cpdf [-subformat <subformat>] [-title <string>] -jpeg <filename>
         -o out.pdf

    cpdf [-subformat <subformat>] [-title <string>] -png <filename>
         -o out.pdf

    cpdf [-subformat <subformat>] [-title <string>] -jpeg2000 <filename>
         -o out.pdf

    cpdf [-subformat <subformat>] [-title <string>]
         [-jbig2-global <filename>] -jbig2 <filename>
         [-jbig2-global | -jbig2-global-clear]
         [-jbig2 <filename>] ...
          -o out.pdf

## A new blank PDF

We can build a new PDF file, given a number of pages and a paper size.
The default is one page, A4 portrait.

`cpdf -create-pdf -create-pdf-pages 20 -create-pdf-papersize usletterportrait -o out.pdf`

The standard paper sizes are listed in Section FIXME, or you may specify the width and height
directly, as described in the same chapter. To build new PDF/UA files,
see `-create-pdf-ua-1` and `-create-pdf-ua-2` in chapter FIXME.

The `-create-pdf` operation may appear after `AND` too, naturally:

`cpdf one.pdf AND -create-pdf AND two.pdf -o out.pdf`

## Convert a text file to PDF

A basic text to PDF convertor is included in Cpdf. It takes a UTF8 text
file (ASCII is a subset of UTF8) and typesets it ragged-right, splitting
on whitespace. Both Windows and Unix line endings are allowed.

`cpdf -typeset file.txt -create-pdf-papersize a3portrait -font Courier -font-size 10 -o out.pdf`

The standard paper sizes are listed in Section FIXME, or you may specify the
width and height directly, as described in the same chapter. The margin may be
set with `-margin`. The default is 7.5% of the page width.

To produce a PDF in PDF/UA-1 or PDF/UA-2 format add, say,
`-subformat PDF/UA-2 -title "Thesis"` to the command line.

The font may be specified as described in Section FIXME. The default
font is Times-Roman and the default size is 12. See Chapter FIXME for
`-color`, `-opacity`, `-line-spacing`, `-outline`, and `-linewidth`.

## Make a PDF from a PNG, JPEG or JPEG2000 image

Simple facilities for making PDFs from PNG and JPEG images are included
in Cpdf. The resulting file can be written out, or used for further
operations.

For PNG files, the file must have no interlacing, and must not be
palletised:

`cpdf -png image.png -o out.pdf`

`cpdf image.png AND -add-text "My Image" -o out.pdf`

Notice that the `-png` can be omitted if your file has a standard file
extension. Almost any JPEG file may be used with `-jpeg` or again, with
a `-jpg` or `-jpeg` extension:

`cpdf -jpeg image.jpg -o out.pdf`

JPEG2000 images may be used similarly, with `-jpeg2000` or alone with a
`jp2`, `jpx` or `jpf` extension:

`cpdf -jpeg2000 image.jp2 -o out.pdf`

The output file will have one point of width or height for each pixel in
the input.

To produce a PDF in PDF/UA-1 or PDF/UA-2 format add, say,
`-subformat "PDF/UA-2" -title "Opus"` to the command line. NB this must
appear before the image file on the command line.

## Make a PDF from one or more JBIG2 images

Cpdf can build multi-pages files from one or more PDF-appropriate JBIG2
fragments, prepared by the `jbig2enc` program. In lossless mode, there
is one JBIG2 fragment for each page:

`cpdf -jbig2 1.jbig2 -jbig2 2.jbig2 -jbig2 3.jbig2 -o out.pdf`

This produces a PDF of three pages. In lossy mode, a JBIG2Globals stream
can be added, which contains shared data for several pages:

`cpdf -jbig2-global 0.jbig2globals -jbig2 1.jbig2 -jbig2 2.jbig2 -jbig2 3.jbig2 -o out.pdf`

The `-jbig2-global` option may be used to change the JBIG2Globals stream
in use. The `-jbig2-global-clear` option may be used to cease use of a
globals stream and return to lossless mode.

To produce a PDF in PDF/UA-1 or PDF/UA-2 format add, say,
`-subformat PDF/UA-2 -title "Opus"` to the command line.
