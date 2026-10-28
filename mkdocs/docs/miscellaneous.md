# Miscellaneous

    cpdf -draft [-boxes] [-draft-remove-only <n>] in.pdf [<range>] -o out.pdf

    cpdf -remove-all-text in.pdf [<range>] -o out.pdf

    cpdf -blacktext in.pdf [<range>] -o out.pdf
    cpdf -blacklines in.pdf [<range>] -o out.pdf
    cpdf -blackfills in.pdf [<range>] -o out.pdf
    cpdf -reveal-text in.pdf [<range>] -o out.pdf

    cpdf -thinlines <min thickness> in.pdf [<range>] -o out.pdf

    cpdf -clean in.pdf -o out.pdf

    cpdf -set-version <version number> in.pdf -o out.pdf

    cpdf -copy-id-from source.pdf in.pdf -o out.pdf

    cpdf -remove-id in.pdf -o out.pdf

    cpdf -list-spot-colors in.pdf

    cpdf -print-dict-entry <key> [-json]  in.pdf

    cpdf -remove-dict-entry <key> [-dict-entry-search <term>]
          in.pdf -o out.pdf

    cpdf -replace-dict-entry <key> -replace-dict-entry-value <value>
         [-dict-entry-search <term>] in.pdf -o out.pdf

    cpdf -remove-clipping [<range>] in.pdf -o out.pdf

    cpdf -obj[-json] <object specification> in.pdf

    cpdf -replace-obj <object specification>=<object> in.pdf -o out.pdf
    cpdf -remove-obj <object number> in.pdf -o out.pdf

    cpdf -extract-stream[-decompress] <object specification>
         in.pdf [-o out.dat | -stdout]

    cpdf -replace-stream <object specification>
         -replace-stream-with <filename>
         in.pdf -o out.pdf

    cpdf -contains-javascript in.pdf
    cpdf -remove-javascript in.pdf -o out.pdf

    cpdf -show-bboxes [-light] in.pdf [<range>] -o out.pdf
    cpdf -show-bboxes-shape <boxspec> [-light] in.pdf [<range>] -o out.pdf

    cpdf -page-content [-pc-no-images] [-pc-no-graphics] [-pc-no-text]
          in.pdf [<range>] -o out.json

## Draft Documents

The `-draft` operation removes bitmap
(photographic) images from a file, so that it can be printed with less
ink. Optionally, the `-boxes` option can be added, filling the spaces
left blank with a crossed box denoting where the image was. This is not
guaranteed to be fully visible in all cases (the bitmap may be have been
partially covered by vector objects or clipped in the original). For
example:

`cpdf -draft -boxes in.pdf -o out.pdf`

To remove a single image only, specify `-draft-remove-only`, giving the
name of the image obtained by a call to `-list-images-used` as described
in Section FIXME
and giving the appropriate page or page range. For example:

`cpdf -draft -boxes -draft-remove-only "/Im1" in.pdf 7 -o out.pdf`

Note that this is only appropriate if the image is not used by pages
outside the range. Check this by cross-referencing object numbers in the
output of `-list-images[-used]`. To remove text instead of images, use
the `-remove-all-text` operation:

`cpdf -remove-all-text in.pdf -o out.pdf`

## Blackening Text, Lines and Fills

Sometimes PDF output from an application (for instance, a web browser)
has text in colors which would not print well on a grayscale printer.
The `-blacktext` operation blackens all text on the given pages so it
will be readable when printed.

This will not work on text which has been converted to outlines, nor on
text which is part of a form.

`cpdf -blacktext in.pdf -o out.pdf`

The `-blacklines` operation blackens all lines on the given pages.

`cpdf -blacklines in.pdf -o out.pdf`

The `-blackfills` operation blackens all fills on the given pages.

`cpdf -blackfills in.pdf -o out.pdf`

Contrary to their names, all these operations can use another color, if
specified with `-color`.

The operation `-reveal-text` will change the text rendering mode of all
text to 0 (stroke and fill). This should reveal hidden text. Note that
hidden text added by an OCR program may have no associated font glyph
data, and so will not re-appear:

`cpdf -reveal-text in.pdf -o out.pdf`

Using `-reveal-text` prior to `-blacktext` will reveal all possible
text.

## Hairline Removal

Quite often, applications will use very thin lines, or even the value of
0, which in PDF means "The thinnest possible line on the output
device". This might be fine for on-screen work, but when printed on a
high resolution device, such as by a commercial printer, they may be too
faint, or disappear altogether. The `-thinlines` operation prevents this
by changing all lines thinner than `<minimal thickness>` to the given
thickness. For example:

`cpdf -thinlines 0.2mm in.pdf [<range>] -o out.pdf`

Thicken all lines less than 0.2mm to that value.

A negative value will instead thin lines to the absolute of the value
given. By processing twice, with a positive then negative figure, it is
therefore possible to clamp thicknesses to a range or single value.

## Garbage Collection

Sometimes incremental updates to a file by an application, or bad
applications can leave data in a PDF file which is no longer used. This
function removes that unneeded data.

`cpdf -clean in.pdf -o out.pdf`

NB: This operation is deprecated. This work is now done by default upon
writing any file.

## Change PDF Version Number

To change the pdf version number, use the `-set-version` operation, giving the
part of the version number after the decimal point. For example:

`cpdf -set-version 4 in.pdf -o out.pdf`

> Change file to PDF 1.4.

This does not alter any of the actual data in the file --- just the
supposed version number. For PDF versions starting with 2 add ten to the
number. For example, for PDF version 2.0, use `-set-version 10`.

## Copy ID

The `-copy-id-from` operation copies the ID from the given file to the
input, writing to the output.

`cpdf -copy-id-from source.pdf in.pdf -o out.pdf`

> Copy the id from `source.pdf` to the contents of `in.pdf`, writing to
`out.pdf`.

If there is no ID in the source file, the existing ID is retained. You
cannot use `-recrypt` with `-copy-id-from`.

## Remove ID

The `-remove-id` operation removes the ID from a document.

`cpdf -remove-id in.pdf -o out.pdf`

> Remove the ID from `in.pdf`, writing to `out.pdf`.

You cannot use `-recrypt` with `-remove-id`.

## List Spot Colours

This operation lists the name of any "separation" color space in the
given PDF file.

`cpdf -list-spot-colors in.pdf`

> List the spot colors, one per line in `in.pdf`, writing to `stdout`.

## PDF Dictionary Entries

This is for editing data within the PDF's internal representation. Use
with caution. To print a dictionary entry:

`cpdf -print-dict-entry /URI in.pdf`

> Print all URLs in annotation hyperlinks in `in.pdf`, one per line.

To report data in JSON format, add `-json`. To remove a dictionary
entry:

`cpdf -remove-dict-entry /One in.pdf -o out.pdf`

> Remove the entry for `/One` in every dictionary `in.pdf`, writing to
`out.pdf`.

`cpdf -remove-dict-entry /One -dict-entry-search "\{I : 1\}"`\
`     in.pdf -o out.pdf`

> Replace the entry for `/One` in every dictionary `in.pdf` if the key's
value is the given CPDFJSON value, writing to `out.pdf`. Alternatively,
prefix `PDF` to give the value in PDF format.

To replace a dictionary entry, give the replacement value in JSON or
format:

`cpdf -replace-dict-entry /One -replace-dict-entry-value "PDF2"`\
`     in.pdf -o out.pdf`

> Remove the entry for `/One` in every dictionary `in.pdf`, writing to
`out.pdf`.

`cpdf -replace-dict-entry /One -dict-entry-search "\{I : 1\}"`\
`     -replace-dict-entry-value "\{I : 2\}" in.pdf -o out.pdf`

> Remove the entry for `/One` in every dictionary `in.pdf` if the key's
value is the given value, writing to `out.pdf`.

## Removing Clipping

The `-remove-clipping` operation removes any clipping paths on given
pages from the file.

`cpdf -remove-clipping in.pdf -o out.pdf`

> Remove clipping paths in `in.pdf`, writing to `out.pdf`.

## Exploring PDFs

The `-obj` operation prints an object to standard output, given the
object number. Number 0 is the trailer dictionary, so we begin there:

    $ cpdf -obj 0 in.pdf
    <</Root 1256 0 R/Length 588/ID[('\029\\t>\249\157\182F_\153V\175z[\234\196)
    ('\029\\t>\249\157\182F_\153V\175z[\234\196)]/Info 1351 0 R/Size 1406>>

    $ cpdf -obj 1256 in.pdf
    <</OpenAction 1238 0 R/PageLabels<</Nums[0<</S/r>>16<</S/D>>]>>/PageMode
    /UseOutlines/Names 924 0 R/Outlines 838 0 R/Pages 851 0 R/Type/Catalog>>

    $ cpdf -obj 1238 out.pdf
    <</D[1225 0 R/Fit]/S/GoTo>>

Alternatively, we may follow a chain of dictionary entries from the
trailer dictionary:

    $ ./cpdf -obj /Root/Pages/Count cpdfmanual.pdf
    133

Or, from a given object number:

    $ ./cpdf -obj 1256/PageLabels/Nums cpdfmanual.pdf
    [0<</S/r>>16<</S/D>>]

We may also begin at a numbered page instead of at the trailer
dictionary:

    ./cpdf -obj P20/Resources/Font/F58/BaseFont cpdfmanual.pdf
    /MCBERL+URWPalladioL-Roma

To output data in JSON format instead of PDF format, use `-obj-json`
instead:

    ./cpdf -obj-json 140/Prev/Prev/Prev cpdfmanual.pdf
    { "/Title": 129, "/A": 126, "/Parent": 112, "/Prev": 124, "/Next": 132 }

We can follow array entries as well as dictionary entries by giving the
index number into the array - here object 0 (the trailer dictionary),
dictionary entry `/ID`, index number 0:

    ./cpdf -obj 0/ID/[0] hello.pdf
    (\232\20625\030\179/\176q:O\202\135\176u\137)

A stream may be extracted with `-extract-stream` or
`-extract-stream-decompress`, which decompresses it first where
possible:

    $ cpdf -obj 0 hello.pdf
    <</Size 4/Root 4 0 R/ID[(\232\20625\030\179/\176q:O\202\135\176u\137)
    (\232\20625\030\179/\176q:O\202\135\176u\137)]>>

    $ cpdf -obj 4 hello.pdf
    <</Type/Catalog/Pages 1 0 R>>

    $ cpdf -obj 1 hello.pdf
    <</Type/Pages/Kids[3 0 R]/Count 1>>

    $ cpdf -obj 3 hello.pdf
    <</Type/Page/Parent 1 0 R/Resources<</Font<</F0<</Type/Font/Subtype/Type1
    /BaseFont/Times-Italic>>>>>>/MediaBox[0 0 595.275590551 841.88976378]
    /Rotate 0/Contents[2 0 R]>>

    $ cpdf -extract-stream-decompress 2 hello.pdf -stdout
    1 0 0 1 50 770 cm BT/F0 36 Tf(Hello, World!)Tj ET

By these mechanisms, ad-hoc exploration of PDF files is possible.

We may also edit dictionary entries with `-replace-obj` by giving an
object specification, and the new value in JSON or PDF format (prefix
with "PDF" to denote PDF format):

`cpdf -replace-obj /Root/MarkInfo/Marked=true in.pdf -o out.pdf`

> Replace or add dictionary entry in `in.pdf`, writing to `out.pdf`.

`cpdf -replace-obj '/Root/Info/Title=PDF(New title)' in.pdf -o out.pdf`

> Replace or add dictionary entry in `in.pdf`, writing to `out.pdf`.

Any part of the object specification not already present will be
fabricated using direct nested dictionaries. For example, if `/MarkInfo`
does not exist in the root dictionary, this command adds
`/MarkInfo <</Marked true>>` to the root dictionary.

An object may be remove with `-remove-obj`:

`cpdf -remove-obj 6 in.pdf -o out.pdf`

> Remove object 6 from `in.pdf`, writing to `out.pdf`.

Stream contents may be replaced with `-replace-stream`:

`cpdf -replace-stream 4 -replace-stream-with in.dat in.pdf -o out.pdf`

> Replace stream object 4's contents with the contents of `in.dat`.

The stream dictionary is unaffected, save for any correction to its
length entry.

## JavaScript and PDF

We can see if a PDF contains any JavaScript scripts or `javascript:`
URIs:

`cpdf -contains-javascript in.pdf -o out.pdf`

> Prints `true` if the PDF contains JavaScript, `false` otherwise.

We can remove any such from the PDF:

`cpdf -remove-javascript in.pdf -o out.pdf`

> Remove JavaScript from `in.pdf`, writing to `out.pdf`.

## Debugging tools

Some extra options are available to aid debugging of Cpdf and PDF
output:

`cpdf -debug in.pdf ...`

> Show debug information.

`cpdf -debug-crypt in.pdf ...`

> Show extra debug information to do with encryption.

`cpdf -debug-malformed in.pdf ...`

> Treat file as if malformed.

`cpdf -debug-stderr-to-stdout in.pdf ...`

> Intermingle `stderr` and `stdout` output on `stdout`.

`cpdf -debug-readable-ops in.pdf ...`

> Use newlines instead of spaces in page stream content.

`cpdf -stay-on-error in.pdf ...`

> Do not call `exit()` after an error.

`cpdf -remove-stream-content in.pdf -o out.pdf`

> Remove stream content when writing. This will create a document which is
malformed, but easier to navigate in a text editor.

## Showing object bounding boxes

We can show the content which makes up each page by showing the bounding
box of each path, glyph, image, inline image, shading, and clipping
region:

`cpdf -show-bboxes in.pdf -o out.pdf`

Here are the colours:

| Object |      Colour | 
| -------------- |----------------|
| Path           | Blue |
| Glyph          | Black |
| Image          | Green |
| Inline image   | Red | 
| Shading        | Cyan |
| Clip           | Black, dashed. |

The option `-light` may be specified to use lighter versions of these
colours if the PDF in question is dark.

To show only such boxes as lie within a given rectangle (for example,
those which may be redacted by a redaction annotation covering that
region), use `-show-bboxes-shape` instead:

`cpdf -show-bboxes-shape "50 50 200 300" in.pdf -o out.pdf`

## Extracting page content

The `-page-content` operation outputs JSON describing the graphic
objects, text, and images on the given pages of the given PDF:

`cpdf -page-content in.pdf -o out.json`

Since such output can be very large, the options `-pc-no-text`,
`-pc-no-graphics` and `-pc-no-images` may be used to suppress unwanted
elements.

In the future, this JSON format will contain enough information to
modify and round-trip page content.
