# Redaction

cpdf -redact
     [-redact-structure remove,all] [-redact-bookmarks remove,page]
     [-redact-links remove,page] in.pdf [<range>] -o out.pdf
cpdf -redact-shape <rectangle> [application options]
     in.pdf [<range>] -o out.pdf

cpdf -redact-apply [application options] in.pdf -o out.pdf
cpdf -redact-apply-type <type> [application options] in.pdf -o out.pdf

cpdf -annotate <rectangle> [annotation options] in.pdf -o out.pdf

cpdf -remove-search-index in.pdf -o out.pdf
cpdf -remove-marked-content in.pdf [<range>] -o out.pdf
cpdf -remove-acroform in.pdf -o out.pdf
cpdf -remove-struct-tree in.pdf -o out.pdf
cpdf -ocg-remove in.pdf -o out.pdf

Redaction application options:

-color <color> *(see Chapter 8 for color specifications)*
-opacity <n> *(see Chapter 8)*
-linewidth <n> *(see Chapter 8)*
-underneath *(see Chapter 8)*
-outline *(see Chapter 8)*
-jbig2dec <path> *when chopping images only*
-jbig2enc <path> *when chopping images only*
-magick <path> *when chopping images only*
-redact-no-show
-redact-text [remove|leave],[touches|encloses]
-redact-images [remove|chop|leave],[touches|encloses|covers<n>]
-redact-inline-images [remove|leave],[touches|encloses| covers<n>]
-redact-vectors [remove|leave],[touches|encloses|covers<n>]
-redact-annotations [remove|leave],[touches|encloses|covers<n>]
-redact-bookmarks [remove|leave],[page|touches]
-redact-links [remove|leave],[page|touches]
-redact-structure [remove|leave],[all|innermost|containing]
-redact-apply-appearance
-redact-invert

Redaction annotation creation options:

-redact-color <color> *(see Chapter 8 for color specifications)*
-redact-outline-color <color> *(see Chapter 8)*
-redact-text-color <color> *(see Chapter 8)*
-redact-opacity *Opacity for redaction fill and outline (see below for
text opacity)*
-redact-overlay <text>
-redact-repeat
-redact-auto-size
-font <font>
-fontsize <n>
-opacity <n>
-linespace <n>
-outline <n>
-linewidth <n>

Redaction removes content from a PDF, erasing it entirely rather than
merely covering it up.

## Redacting whole pages

To redact one or more whole pages we give the page range we use
`-redact`. Such pages will be emptied of content, and any annotations
and page resources removed. The page dimensions remain the same.

`cpdf -redact in.pdf 1,2,19-end -o out.pdf`

Redact pages 1,2 and 19-end of `in.pdf`, writing to `out.pdf`

If `-redact-structure remove,all` is added to the command, the
document's structure tree will be shorn of any parts which are marked as
relating to the now-redacted pages. We can add
`-redact-bookmarks remove,page` to remove bookmarks linking to the page
range, and `-redact-links remove,page` to remove any hyperlink in the
whole PDF pointing to any redacted page.

## Redacting content selectively

The `-redact-shape` operation allows a shape (presently just a
rectangle) to be redacted in the given page range. For example:

`cpdf -redact-shape "100 100 200 300" in.pdf 2 -o out.pdf`

Redact rectangle on page 2 of `in.pdf`, writing to `out.pdf`

The rectangle is given as \"minx miny width height\" as usual. What
content is removed, and how it is detected and processed is defined by a
number of options:

The specification is made of two parts -- the first required, the second
optional. For the first, the `remove` specifier will remove matching
content, `leave` will leave the content unredacted, and `chop` -- for
images only -- will cut out the part of the image to be redacted. For
the second, the `touches` specifier will consider any content
intersecting with the redaction rectangle, whereas `encloses` will
consider only such content as is fully enclosed within the redaction
rectangle. The `covers` specifier insists the redaction rectangle cover
at least the given percentage of the content (e.g `covers80` for 80%).
The default is `touches`. NB: Intersecting and enclosed and covers here
refer to operations on the rectangular bounding boxes of glyphs, images,
paths etc. The exact shapes are not used.

The appearance of the redaction is governed by the options `-color`
(default black, format described in Chapter 8), `-outline` (default
filled), `-linewidth`, `-underneath`, `-opacity`, and in the alternative
`-redact-no-show` (omit the redaction appearance entirely).

The options `-jbig2enc`, `-jbig2dec`, and `magick` may be required when
`-redact-image` is in `chop` mode. The `magick` program is always
required; `-jbig2dec` and `-jbig2enc` only where the file contains JBIG2
images.

## Applying redaction annotations

Annotations denoting future redactions can be added to a PDF in Adobe
Acrobat or other interactive PDF software. Cpdf can apply these
annotations to perform the redaction. To apply existing annotation
redactions:

`cpdf -redact-apply in.pdf 2 out.pdf`

Apply redaction annotations on page two of `in.pdf`, writing to
`out.pdf`

The redaction application appearance options described above may be used
here too. Or, if we supply `-redact-show-appearance` the redaction
annotation's own appearance will be used instead.

If the interactive software you are using to demarcate the annotations
does not know about redaction annotations, you can use a different kind
of annotation and Cpdf will use that annotation's bounding box as the
redaction rectangle:

`cpdf -redact-apply-type /Square in.pdf 2 out.pdf`

Apply Square (rectangle) annotations as if they were redaction
annotations in `in.pdf`, writing to `out.pdf`

As above, the redaction itself is controlled through the options as
`-redact-text`, `-redact-images` and so on.

## Creating redaction annotations

To add new redaction annotations, we may write

`cpdf -annotate "100 100 200 300" in.pdf 1 out.pdf`

Add a redaction annotation to page 1 of `in.pdf`, writing to `out.pdf`

Recall that this does not redact content, merely marks it for future
redaction.

By default, the redaction shows as a black filled rectangle with a dark
red outline. We can change this with `-redact-color` and
`-redact-outline-color` and `-redact-opacity`.

We can add text to, for example, show which rule required the redaction,
with `-redact-overlay`:

`cpdf -annotate "100 100 200 300" -redact-overlay "(b) (1) (A)"`
`     in.pdf 1 out.pdf`

Add a description redaction annotation to page 1 of `in.pdf`, writing to
`out.pdf`

The text colour may be changed with `-redact-text-color`. Another use of
overlay text is to add repeated text to show where text has been
redacted. We can do this by adding `-redact-repeat`. The text will be
repeated, interspersed with spaces. To auto-size the text to the width
of the redaction annotation, we can add `-redact-auto-size`. Where both
`-redact-repeat` and `-redact-auto-size` are present, the given text is
interspersed with newlines instead, each repetition scaled to fit one
line.

The following options from Chapter 8 may be used to style the text:
`-font`, `-fontsize`, `-opacity`, `-linespace`, `-outline`,
`-linewidth`.

## Removing other metadata

Even when we have removed the required page content, it may leak in
other places. For example, bookmarks or links which point to the
redacted content or in PDF/UA logical structure. Cpdf provides options
to be added to all redaction commands (such as `-redact-shape`) to
redact such data:

For example, to remove bookmarks which link to the redacted areas, use
`-redact-bookmarks remove,touches`. To remove links from other pages
which point to anywhere on the page where content has been redacted, use
`-redact-links remove,page`.

To erase structure content (which may include the same text as text on
the page content itself) we use `-redact-structure`. The `remove,all`
specification erases all structure item text on the page. The
`remove,containing` removes all structure item text enclosing the
redacted content. The `remove,innermost` removes just the innermost
enclosing structure. The structure is left intact, so PDF/UA compliance
is preserved, but any text which has been redacted will be removed also
from the structure.

Three other options can help to remove metadata from a file where it
might contain information we do not wish to release. To remove the
search index:

`cpdf -remove-search-index in.pdf -o out.pdf`

Remove the proprietary Adobe search index from a document

To remove marked content, proprietary or otherwise from all content
streams in the document (NB: will void PDF/UA status).

`cpdf -remove-marked-content in.pdf -o out.pdf`

Remove all marked content from PDF streams.

To remove the structure tree wholesale (NB: will void PDF/UA status):

`cpdf -remove-struct-tree in.pdf -o out.pdf`

Remove all marked content from PDF streams.

To remove any AcroForm from the document:

`cpdf -remove-acroform in.pdf -o out.pdf`

Remove AcroForm.

And one more from elsewhere in this manual:

`cpdf -ocg-remove in.pdf -o out.pdf`

Remove the optional content structure from a document. See Chapter 16
for details.

## Evidence of redaction

There are several ways to check that redaction has proceeded as we would
have wished:

Inversion

: Adding `-redact-invert` and writing to another PDF will keep all
  objects redaction would lose, and lose all objects redaction would
  keep. Each object (image, vector, glyph) ends up only the redacted or
  inverted output (note that images are never chopped with
  `-redact-invert` the specifier `chop` is considered the same as
  `remove` in this context).

Page content JSON

: Page content can be extracted with `-page-content` as described in
  Section FIXME for the redacted and unredacted file, and a
  comparison made.

Full JSON

: To check metadata as well, we can use `-output-json` from Chapter 15
  and compare the output on redacted and unredacted files.

Text extraction

: Whilst Cpdf does not yet have text extraction facilities, you can use
  another text extraction program to extract the text from the original
  file and the redacted file and check the differences.

Image extraction

: Cpdf's image listing and extraction facilities (see Chapter 13) may be
  used to check that images have been redacted when in `chop` mode, and
  that no hidden images or image masks have survived.

Structure tree extraction

: Cpdf's `-extract-struct-tree` (see Chapter 19) may be used to verify
  the erasure of text in structure tree nodes referring to redacted
  content.
