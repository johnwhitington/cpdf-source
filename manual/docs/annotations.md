# Annotations

    cpdf -list-annotations in.pdf [<range>]

    cpdf -list-annotations-json in.pdf [-annot-type <type>] [<range>]

    cpdf -set-annotations <filename> [-underneath] in.pdf [<range>] -o out.pdf

    cpdf -copy-annotations from.pdf to.pdf [<range>] -o out.pdf

    cpdf -remove-annotations in.pdf [<range>] -o out.pdf

    cpdf -flatten-annotations in.pdf [<range>]
         [-annot-type <subtypes> | -annot-no-type <subtypes>] -o out.pdf

## Listing Annotations

The `-list-annotations` operation prints the textual content of any
annotations on the selected pages to standard output. Each annotation is
preceded by the page number and followed by a newline. The output of
this operation is always UTF8.

`cpdf -list-annotations in.pdf > annots.txt`

> Print annotations from `in.pdf`, redirecting output to `annots.txt`.

More information can be obtained by listing annotations in JSON format:

`cpdf -list-annotations-json in.pdf > annots.json`

> Print annotations from `in.pdf` in JSON format, redirecting output to
`annots.json`.

This produces an array of (page number, object number, annotation)
triples giving the PDF structure of each annotation. Destination pages
for page links will have page numbers in place of internal PDF page
links, but the content is otherwise unaltered. Here is an example entry
for an annotation with object number 102 on page 10:

      [
      10, 102
      { "/H": { "N": "/I" },
        "/Border": [ { "I": 0 }, { "I": 0 }, { "I": 0 } ],
        "/Rect": [
          { "F": 89.88023 }, { "F": 409.98401 }, { "F": 323.90561 }, {
            "F": 423.32059 } ],
        "/Subtype": { "N": "/Link" },
        "/Type": { "N": "/Annot" },
        "/A": {
          "/S": { "N": "/URI" },
          "/URI": { "U" : "http://www.google.com/" },
        "/StructParent": { "I": 10 } }
      ]

Extra objects required for annotations, but which are not annotations
themselves are also extracted. They omit the page number, being just a
pair of the object number and object. The CPDFJSON format is described
on page . There is an additional object, -1, which gives the Cpdf
annotation format version, currently 1.

To list only one or more certain types of annotation, add `-annot-type`:

`cpdf -list-annotations-json in.pdf -annot-type /Link/Highlight > annots.json`

> Print link and highlight annotations from `in.pdf` in JSON format,
redirecting output to `annots.json`.

To list all but one or more certain types of annotation, add
`-annot-no-type`:

`cpdf -list-annotations-json in.pdf -annot-no-type /Link/Highlight > annots.json`

> Print all but link and highlight annotations from `in.pdf` in JSON
format, redirecting output to `annots.json`.

Note that Popup annotations related to the chosen annotation type will
not be included in these cases.

## Setting annotations

We can also set annotations from a JSON file, either modified from the
output of`-list-annotations-json` or produced manually:

`cpdf -set-annotations annots.json in.pdf -o out.pdf `

> Add the annotations in `annots.json` on top of any already present in
`in.pdf`, writing to `out.pdf`.

If replacing rather than adding annotations, use `-remove-annotations`
first to clear the existing ones.

## Copying Annotations

The `-copy-annotations` operation copies the annotations in the given
page range from one file (the file specified immediately after the
option) to another pre-existing PDF. The range is specified after this
pre-existing PDF. The result is then written an output file, specified
in the usual way.

`cpdf -copy-annotations from.pdf to.pdf 1-10 -o result.pdf `

> Copy annotations from the first ten pages of `from.pdf` onto the PDF
file `to.pdf`, writing the result to `results.pdf`.

It exists for historical reasons, and is no different from listing and
setting the annotations using `-list-annotations-json` and
`-set-annotations`.

## Removing Annotations

The `-remove-annotations` operation removes all annotations from the
given page range.

`cpdf -remove-annotations in.pdf 1 -o out.pdf`

> Remove annotations from the first page of a file only.

## Flattening Annotations

The `-flatten-annotations` operation removes all annotations from the
given page range, flattening their appearances onto the page as ordinary
graphics operations.

`cpdf -flatten-annotations in.pdf -o out.pdf`

> Flatten annotations.

We can use `-annot-type` or `-annot-no-type` to select annotations to
flatten, or to refrain from flattening. For example:

`cpdf -flatten-annotations in.pdf -annot-no-type /Link/Highlight -o out.pdf`

> Flatten annotations, except link and highlight annotations.
