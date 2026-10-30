![logo](logo.png)

# Coherent PDF Command Line Tools

Version 2.9.3 (October 2026)

For bug reports, feature requests and comments, email [contact@coherentgraphics.co.uk](mailto:contact@coherentgraphics.co.uk)

The Coherent PDF tools provide a wide range of facilities for modifying
PDF files created by other means. There is a single command-line program
Cpdf (`cpdf.exe` under Microsoft Windows). The rest of this manual
describes the options that may be given to this program.

## Quickstart Examples

These examples demonstrate just a few of the facilities provided by the
Coherent PDF Command Line Tools. See each chapter for more commands and
full details.

### [Basic Usage](basic-usage.md)

`cpdf in.pdf 1-3,6 -o out.pdf`

> Read `in.pdf`, select pages 1, 2, 3 and 6, and write those pages to
`out.pdf`.

`cpdf in.pdf even -o out.pdf`

> Select the even pages (2, 4, 6...) from `in.pdf` and write those pages
to `out.pdf`.

`cpdf -merge in.pdf in2.pdf AND -add-text "Copyright 2026" -o out.pdf`

> Using `AND` to perform several operations in order, here merging two
files together and adding a copyright stamp to every page.

`cpdf -args args.txt`

> Read `args.txt` and use its contents as the command line arguments for
Cpdf.

### [Merging and Splitting](merging-and-splitting.md)

`cpdf -merge in.pdf in2.pdf -o out.pdf`

> Merge `in.pdf` and `in2.pdf` into one document, writing to `out.pdf`.

`cpdf -split in.pdf -o Chunk%%%.pdf -chunk 10`

> Split `in.pdf` into ten-page chunks, writing them to `Chunk001.pdf`,
`Chunk002.pdf` etc.

`cpdf -split-bookmarks 0 in.pdf -utf8 -o @B.pdf`

> Split `in.pdf` on bookmark boundaries, writing each to a file whose name
is the bookmark label.

`cpdf -split-max 1Mb in.pdf -o %%%.pdf`

> Split `in.pdf` into files of 1Mb or less

`cpdf -spray in.pdf -o a.pdf -o b.pdf -o c.pdf`

> Split `in.pdf`, writing pages 1,4,7... to `a.pdf`, 2,5,8... to `b.pdf`
and 3,6,9... to `c.pdf`.

### [Pages](pages.md)

`cpdf -scale-page "2 2" in.pdf -o out.pdf`

> Scale both the dimensions and contents of `in.pdf` by a factor of two in
x and y directions.

`cpdf -scale-to-fit usletterportrait in.pdf -o out.pdf`

> Scale the pages in `in.pdf` to fit the US Letter page size, writing to
`out.pdf`

`cpdf -shift "26pt 18mm" in.pdf -o out.pdf`

> Shift the contents of the page by 26 pts in the x direction, and 18
millimetres in the y direction, writing to `out.pdf`

`cpdf -rotate-contents 90 in.pdf -o out.pdf`

> Rotate the contents of the pages in `in.pdf` by ninety degrees and write
to `out.pdf`.

`cpdf -cropbox "0 0 600pt 400pt" in.pdf -o out.pdf`

> Crop the pages in `in.pdf` to a 600 pts by 400 pts rectangle.

### [Encryption and Decryption](encryption-and-decryption.md)

`cpdf -encrypt AES256ISO fred joe in.pdf -o out.pdf`

> Encrypt `in.pdf` using AES 256 PDF encryption using the owner password
`fred` and the user password `joe` and writing the encrypted file to
`out.pdf`

`cpdf -decrypt in.pdf owner=fred -o out.pdf`

> Decrypt `in.pdf` using the owner password, writing to `out.pdf`.

### [Compression](compression.md)

`cpdf -compress in.pdf -o out.pdf`

> Compress the data streams in `in.pdf`, writing the result to `out.pdf`.

`cpdf -decompress in.pdf -o out.pdf`

> Decompress the data streams in `in.pdf`, writing to `out.pdf`.

`cpdf -squeeze in.pdf -o out.pdf`

> Squeeze `in.pdf`, writing to `out.pdf`. Squeezing rearranges the
structure of the PDF file to save space.

### [Bookmarks](bookmarks.md)

`cpdf -list-bookmarks -utf8 in.pdf`

> List the bookmarks in `in.pdf`.

`cpdf -add-bookmarks bookmarks.txt in.pdf -o out.pdf`

> Add bookmarks in the same form from a prepared file `bookmarks.txt` to
`in.pdf`, writing to `out.pdf`. JSON alternatives are also available.

`cpdf -table-of-contents in.pdf -o out.pdf`

> Typeset a table of contents from existing bookmarks and prepend to the
document.

### [Presentations](presentations.md)

`cpdf -presentation in.pdf 2-end -trans Split -duration 10 -o out.pdf`

> Use the Split style to build a presentation from the PDF `in.pdf`, each
slide staying 10 seconds on screen unless manually advanced. The first
page, being a title does not move on automatically, and has no
transition effect.

### [Text and Stamps](text-and-stamps.md)

`cpdf -stamp-on watermark.pdf in.pdf -o out.pdf`

> Stamp the file `watermark.pdf` on to each page of `in.pdf`, writing the
result to `out.pdf`.

`cpdf -topleft 10 -font Courier -add-text "Page \%Page\nDate \%d-\%m-\%Y" in.pdf -o out.pdf`

> Add a page number and date to all the pages in `in.pdf` using the
Courier font, writing to `out.pdf`.

### [Multipage Facilities](multipage-facilities.md)

`cpdf -impose-xy "2 1" in.pdf -o out.pdf`

> Two up impose the file `in.pdf`, writing to `out.pdf`.

`cpdf -pad-after in.pdf 1,3,4 -o out.pdf`

> Add extra blank pages after pages one, three and four of a document.

`cpdf -chop "2 2" in.pdf -o out.pdf`

> Chop each page into four quarters, including each in the output.

### [Annotations](annotations.md)

`cpdf -list-annotations-json in.pdf > out.json`

> List the annotations in a file `in.pdf` to standard output, redirecting
to file `out.json`.

`cpdf -set-annotations out.json in.pdf -o out.pdf`

> Add the annotations from a JSON annotations file to `in.pdf`, writing to
`out.pdf`.

`cpdf -remove-annotations in.pdf -o out.pdf`

> Remove the annotations from `in.pdf`, writing to `out.pdf`.

### [Document Information and Metadata](document-information-and-metadata.md)

`cpdf -info -utf8 in.pdf`

> List document metadata for `in.pdf`.

`cpdf -set-title "The New Title" -also-set-xmp in.pdf -o out.pdf`

> Set the document title of `in.pdf`, writing to `out.pdf`.

`cpdf -hide-toolbar true in.pdf -o out.pdf`

> Set the document `in.pdf` to open with the PDF Viewer's toolbar hidden,
writing to `out.pdf`.

`cpdf -set-metadata metadata.xml in.pdf -o out.pdf`

> Set the metadata in a PDF `in.pdf` to the contents of the file
`metadata.xml`, and write the output to `out.pdf`.

`cpdf -set-page-layout TwoColumnRight in.pdf -o out.pdf`

> Set the document `in.pdf` to open in PDF Viewer showing two columns of
pages, starting on the right, putting the result in `out.pdf`.

`cpdf -set-page-mode FullScreen in.pdf -o out.pdf`

> Set the document `in.pdf` to open in PDF Viewer in full screen mode,
putting the result in `out.pdf`.

`cpdf -print-page-labels-json in.pdf`

> Show, in JSON format, the page labels in `in.pdf`.

`cpdf -composition in.pdf`

> Show how much data in `in.pdf` is used for images, fonts etc.

### [File Attachments](file-attachments.md)

`cpdf -attach-file sheet.xls in.pdf -o out.pdf`

> Attach the file `sheet.xls` to `in.pdf`, writing to `out.pdf`.

`cpdf -remove-files in.pdf -o out.pdf`

> Remove any attachments from `in.pdf`, writing to `out.pdf`.

`cpdf -dump-attachments in.pdf -o /home/fred/attachments`

> Dump attachments to file, given the directory to put them in.

### [Images](images.md)

`cpdf -image-resolution 600 in.pdf`

> Identify and list any image used at less than 600dpi.

`cpdf -extract-images in.pdf -im /usr/bin/magick -o output/%%%`

> Extract images from `in.pdf` to directory `output` (with the help of
imagemagick).

`cpdf -process-images -jpeg-to-jpeg 65 in.pdf -o out.pdf`

> Process JPEG images in `in.pdf` to 65% quality, writing the output to
`out.pdf`.

`cpdf -gs gs -rasterize in.pdf -o out.pdf`

> Rasterize PDF page content, creating new PDF.

`cpdf -gs gs -output-image in.pdf 10-end -o image%%%.png`

> Rasterize PDF pages to PNG files.

### [Fonts](fonts.md)

`cpdf -list-fonts in.pdf`

> List the fonts in use, and what pages they are used on.

`cpdf -missing-fonts in.pdf`

> List missing fonts.

### [PDF and JSON](PDF-and-JSON.md)

`cpdf in.pdf -output-json -utf8 -output-json-parse-content-streams -o out.json`

> Write the PDF in JSON format to the given file, parsing its content
streams into individual JSON objects too.

`cpdf -j in.json -o out.pdf`

> Load a PDF in JSON format, writing to an output PDF.

### [Optional Content Groups](optional-content-groups.md)

`cpdf -ocg-list in.pdf`

> List the optional content groups by name.

`cpdf -ocg-coalesce-on-name in.pdf -o out.pdf`

> Coalesce optional content groups after merging or stamping two files
with OCGs with like names.

### [Creating New PDFs](creating-new-PDFs.md)

`cpdf -create-pdf -create-pdf-pages 20 -create-pdf-papersize usletterportrait -o out.pdf`

> Create a US Letter PDF of twenty pages.

`cpdf -typeset file.txt -create-pdf-papersize a3portrait -font Courier -font-size 10 -o out.pdf`

> Typeset a text file as PDF on A3 paper with Courier 10 point font.

`cpdf -jpeg pic.jpeg -png pic.png -o out.pdf`

> Make a two-page PDF, the first from a JPEG and the second from a PNG.

### [Drawing on PDFs](drawing-on-PDFs.md)

`cpdf -create-pdf AND -draw -to "100 100" -line "400 400" -line "400 100" -close -fill -o out.pdf`

> Create a new PDF and draw a filled triangle on it.

`cpdf -create-pdf AND -draw -mtrans "100 200" -font-size 50 -leading 55 -bt -text "This is" -nl -text "on multiple" -nl -text "lines" -et -o out.pdf`

> Create a new PDF and draw three lines of text on it.

`cpdf -create-pdf AND -draw -bt -text "Page 1" -et -newpage -bt -text "Page 2" -et -o out.pdf`

> Create a new PDF and draw text on one page and then the next.

### [Accessible PDFs with PDF/UA](accessible-PDFs-with-PDFUA.md)

`cpdf -verify 'PDF/UA-1(matterhorn)' -json in.pdf`

> Verify `in.pdf` for conformance to PDF/UA-1 using the Matterhorn
protocol, returning results in JSON format.

### [Chapter 20: Redaction](redaction.md)

`cpdf -redact-shape "100 200 200 300" in.pdf 1 -o out.pdf`

> Redact text in the given rectangle on page one of `in.pdf`, writing to `out.pdf`.

`cpdf -redact-apply in.pdf -o out.pdf`

> Apply redaction annotations.

### [Chapter 21: Miscellaneous](miscellaneous.md)

`cpdf -blacktext in.pdf -o out.pdf`

> Blacken all the text in `in.pdf`, writing to `out.pdf`.

`cpdf -thinlines 2pt in.pdf -o out.pdf`

> Make sure all lines in `in.pdf` are at least 2pts wide, writing to
`out.pdf`.

`cpdf -print-dict-entry /URI in.pdf`

> List all URLs in annotation hyperlinks in `in.pdf`.

## Legal notices

© Coherent Graphics Limited. All rights reserved.

Adobe, Acrobat, and Adobe PDF are registered trademarks of Adobe Systems
Incorporated. Windows, Powerpoint and Excel are registered trademarks of
Microsoft Corporation. The verification descriptions for the Matterhorn
Protocol come from the PDF Association, and are licensed under the
Creative Commons Attribution 4.0 International license.
