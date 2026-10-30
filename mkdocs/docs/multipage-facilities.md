# Multipage Facilities

    cpdf -pad-before in.pdf [<range>] [-pad-with pad.pdf] -o out.pdf

    cpdf -pad-after in.pdf [<range>] [-pad-with pad.pdf] -o out.pdf

    cpdf -pad-every [<integer>] in.pdf [-pad-with pad.pdf] -o out.pdf

    cpdf -pad-multiple [<integer>] in.pdf -o out.pdf

    cpdf -pad-multiple-before [<integer>] in.pdf -o out.pdf

    cpdf [-impose <pagesize> | impose-xy "<x> <y>"]  
         [-impose-columns] [-impose-rtl] [-impose-btt]  
         [-impose-margin <margin>] [-impose-spacing <spacing>]  
         [-impose-linewidth <width>] [-fast]  
         [-process-struct-trees]  
         in.pdf -o out.pdf

    cpdf -twoup-stack [-fast] [-process-struct-trees] in.pdf -o out.pdf

    cpdf -twoup [-fast] [-process-struct-trees] in.pdf -o out.pdf

    cpdf -chop "<x> <y>" [-chop-columns] [-chop-rtl] [-chop-btt]  
         in.pdf [<range>] -o out.pdf

    cpdf [-chop-h <y> | -chop-v <x>] [-chop-columns]  
         in.pdf [<range>] -o out.pdf

## Inserting Blank Pages

Sometimes, for instance to get a printing arrangement right, it's useful
to be able to insert blank pages into a PDF file. Cpdf can add blank
pages before a given page or pages, or after. The pages in question are
specified by a range in the usual way:

`cpdf -pad-before in.pdf 1 -o out.pdf`

> Add a blank page before page 1 (i.e. at the beginning of the document.)

`cpdf -pad-after in.pdf 2,16,38,84,121,147 -o out.pdf`

> Add a blank page after pages 2, 16, 38, 84, 121 and 147 (for instance,
to add a clean page between chapters of a document.)

The dimensions of the padded page are derived from the boxes (media box,
crop box etc.) of the page after or before which the padding is to be
applied.

The `-pad-every n` operation places a blank page after every n pages,
excluding any last one. For example on a 9 page document this command
adds a blank page after pages 3 and 6:

`cpdf -pad-every 3 in.pdf -o out.pdf`

> Add a blank page after every three pages

In all three of these operations, one may specify `-pad-with` providing
a (usually one-page) PDF file to be used instead of a blank page. For
example, a page saying "This page left intentionally blank".

The `-pad-multiple n` operation adds blank pages so the document has a
multiple of `n` pages. For example:

`cpdf -pad-multiple 8 in.pdf -o out.pdf`

> Add blank pages to `in.pdf` so it has a multiple of 8 pages.

The `-pad-multiple-before n` operation adds the padding pages at the
beginning of the file instead.

## Imposition

Imposition is the act of putting two or more pages of an input document
onto each page of the output document. There are two operations provided
by Cpdf:

- the `-impose` operation which, given a page size fits multiple pages
  into it; and

- the `-impose-xy` operation which, given an x and y value, builds
  an output page which fits x input pages horizontally and y input
  pages vertically.

`cpdf -impose a0landscape in.pdf -o out.pdf`

> Impose as many pages as will fit on to new A0 landscape pages.

`cpdf -impose-xy "3 4" in.pdf -o out.pdf`

> Impose 3 across and 4 down on to new pages of 3 times the width and 4
times the height of the input ones.

The x-value for `-impose-xy` may be set to zero to indicate an
infinitely-wide page; the y-value to indicate an infinitely-long one.
In both cases, the pages in the input file are assumed to be of the same
dimensions.

The following options may be used to modify the output:

- `-impose-columns` Lay the pages out in columns rather than rows.

- `-impose-rtl` Lay the pages out right-to-left.

- `-impose-btt` Lay the pages out bottom-to-top.

- `-impose-margin <margin>` Add a margin around the edge of the page of
  the given width. When using `-impose-xy` the page size increases; with
  `-impose` the pages are scaled.

- `-impose-spacing <spacing>` Add spacing between each row and column.
  When using `-impose-xy` the page size increases; with `-impose` the
  pages are scaled.

- `-impose-linewidth <width>` Add a border around each input page. With
  `-impose` the pages are scaled after the border is added, so you must
  account for this yourself.

To impose with rotated pages, for example to put two A4 portrait pages
two-up on an A3 landscape page, rotate them prior to imposition.

Two other ways of putting multiple pages on a single page remain from
earlier versions of Cpdf which lacked a general imposition operation.
The `-twoup-stack` operation puts two logical pages on each physical
page, rotating them 90 degrees to do so. The new mediabox is thus
larger. The `-twoup` operation does the same, but scales the new sides
down so that the media box is unchanged.

`cpdf -twoup in.pdf -o out.pdf`

> Impose a document two-up, keeping the existing page size.

`cpdf -twoup-stack in.pdf -o out.pdf`

> Impose a document two-up on a larger page by rotation.

NB: For all imposition options, see also discussion of `-fast` in
[Malformed Files](basic-usage.md#malformed-files). The
option `-process-struct-trees` will mark the file's content as an
artifact for the purpose of imposition.

## Chopping up pages

The `-chop` operation cuts up a page into multiple pages, according to
the chosen grid, and those pages replace the originals in the PDF. It is
a sort of de-imposition. For example:

`cpdf -chop "2 3" in.pdf -o out.pdf`

> Chop each page into six.

The crop box is used if present; if not, the media box. By default, the
pieces are arranged in the output file row by row, and from left to
right on each row. To alter this, add one or more of `-chop-columns`,
`-chop-rtl` (right to left), and `-chop-btt` (bottom to top).

As an alternative, pages can be chopped into two at a given position,
horizontally with `-chop-h` or vertically with `-chop-v`:

`cpdf -chop-h 400 in.pdf -o out.pdf`

> Chop each page into two, top and bottom, at 400pt mark.

To reverse the order of pages in the output, specify `-chop-columns` in
addition.
