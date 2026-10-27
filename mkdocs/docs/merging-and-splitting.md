# Merging and Splitting

    cpdf -merge in1.pdf [<range>] in2.pdf [<range>] [<more names/ranges>]  
         [-collate] [-collate-n <n>] [-retain-numbering]  
         [-merge-add-bookmarks [-merge-add-bookmarks-use-titles]]  
         [-remove-duplicate-fonts] [-process-struct-trees]  
         [-subformat <subformat>]  
         -o out.pdf

    cpdf -portfolio in.pdf -pf <filename> [-pfd <string>]  
         [-pfr <relationship>] [-pf ...] -o out.pdf

    cpdf -split in.pdf [-chunk <chunksize>] [-process-struct-trees]  
         [-utf8] -o <format>

    cpdf -split-bookmarks <level> in.pdf [-utf8] [-process-struct-trees]  
         -o <format>

    cpdf -split-max <file size> in.pdf [-utf8] [-process-struct-trees]  
         -o <format>

    cpdf -spray in.pdf [-utf8] [-process-struct-trees]  
         -o a.pdf [-o b.pdf [-o ...]]

## Merging

The `-merge` operation allows the merging of several files into one.
Ranges can be used to select only a subset of pages from each input file
in the output. The output file consists of the concatenation of all the
input pages in the order specified on the command line. Actually, the
`-merge` can be omitted, since this is the default operation of Cpdf.

`cpdf -merge a.pdf 1 b.pdf 2-end -o out.pdf`

Take page one of `a.pdf` and all but the first page of `b.pdf`, merge
them and produce `out.pdf`.

`cpdf -merge -idir files -o out.pdf`

Merge all files from directory `files`, producing `out.pdf`.

Merge maintains and merges bookmarks, named destinations, annotations,
tagged PDF information, and so on. PDF features which cannot be merged
are retained if they are from the document which first exhibits that
feature.

The `-collate` option collates pages: that is to say, it takes the first
page from the first document and its range, then the first page from the
second document and its range and so on. When all first pages have been
taken, it begins on the second from each range, and so on. To collate in
chunks use, for example, `-collate-n 2`.

The `-retain-numbering` option keeps the PDF page numbering labels of
each document intact, rather than renumbering the output pages from 1.

The `-remove-duplicate-fonts` option ensures that fonts used in more
than one of the inputs only appear once in the output.

The `-merge-add-bookmarks` option adds a top-level bookmark for each
file, using the filename. Any existing bookmarks are retained. The
`-merge­-add­-bookmarks­-use­-titles`, when used in conjunction with
`-merge-add-bookmarks`, will use the title from each PDF's metadata
instead of the filename.

The `-process-struct-trees` option will merge structure trees (the data
which forms the logical structure of the PDF). In its absence, the
structure tree from the first PDF only is preserved. When merging two or
more PDF/UA files, we can add `-subformat PDF/UA-2` to tell Cpdf to add
a top-level Document structure tree element, to conform to the PDF/UA-2
standard.

## PDF Portfolios

A PDF portfolio is a special kind of PDF with contains other documents
(PDF and otherwise) within it. Support is mostly limited to Adobe
products at time of writing. To create a portfolio from a PDF file and
and a JPEG:

`cpdf -create-pdf AND -portfolio -pf one.pdf -pf two.jpeg -o out.pdf`

Build a PDF portfolio with two files: `one.pdf` and `two.jpeg`, writing
to `out.pdf`.

When opened in a supporting PDF viewer, such as Adobe Reader, a
navigation pane is shown, allowing each file to be viewed. There are two
columns of interest: the Description and the Relationship. By default,
Cpdf uses the file name for the description, and \"Unspecified\" for the
Relationship. We can specify the description by adding
`-pfd "The description"` after the relevant `-pf`, and the relationship
by adding `-pfr RelationshipName`.

## Splitting

The `-split` operation splits a PDF file into a number of parts which
are written to file, their names being generated from a *format*. The
optional `-chunk` option allows the number of pages written to each
output file to be set.

`cpdf -split a.pdf -o out%%%.pdf`

Split `a.pdf` to the files `out001.pdf`, `out002.pdf` etc.

`cpdf a.pdf even AND -split -chunk 10 -o dir/out%%%.pdf`

Split the even pages of `a.pdf` to the files `out001.pdf`, `out002.pdf`
etc. with at most ten pages in each file. The directory (folder) `dir`
must exist.

If the output format does not provide enough numbers for the files
generated, the result is unspecified. The following format operators may
be used:

    `%, %%, %%% etc.`  Sequence number padded to the number of percent signs
    `@F`               Original filename without extension
    `@N`               Sequence number without padding zeroes
    `@S`               Start page of this chunk
    `@E`               End page of this chunk
    `@B`               Bookmark name at this page, if any.
    `@b<w>@`           Bookmark name at this page, if any, truncated to `<w>` characters.

Numbers padded to a fixed width field by zeroes may be obtained for `@S`
and `@E` by following them with more `@` signs e.g `@E@@@` for a fixed
width of three.

## Splitting on Bookmarks

The `-split-bookmarks <level>` operation splits a PDF file into a number
of parts, according to the page ranges implied by the document's
bookmarks. These parts are then written to file with names generated
from the given format.

Level 0 denotes the top-level bookmarks, level 1 the next level
(sub-bookmarks) and so on. So `-split-bookmarks 1` creates breaks on
level 0 and level 1 boundaries.

`cpdf -split-bookmarks 0 a.pdf -o out%%%.pdf`

Split `a.pdf` to the files `out001.pdf`, `out002.pdf` on bookmark
boundaries.

There may be many bookmarks on a single page (for instance, if
paragraphs are bookmarked or there are two subsections on one page). The
splits calculated by `-split-bookmarks` ensure that each page appears in
only one of the output files. It is possible to use the `@` operators
above, including operator `@B` which expands to the text of the
bookmark:

`cpdf -split-bookmarks 0 a.pdf -o @B.pdf`

Split `a.pdf` on bookmark boundaries, using the bookmark text as the
filename.

The bookmark text used for a name has the following characters are
removed, in addition to any character with ASCII code less than 32 or
equal to 126. In addition, names beginning with `.` are not produced.

` / ? < > \ : * | " ^ + =`

The bookmark may be truncated by using the `@b` variant:

`cpdf -split-bookmarks 0 a.pdf -o @b10@.pdf`

Split `a.pdf` on bookmark boundaries, using the first 10 characters of
bookmark text as the filename.

## Splitting to Maximum Size

The `-split-max` operation splits a file into chunks of no more than the
given size, starting at the beginning. The suffixes kB, KiB, MB, MiB,
GB, and GiB may be used to give the size. For example:

`cpdf -split-max 100kB in.pdf -o out%%%.pdf`

Split `in.pdf` into parts of no more than 100kB, if possible.

Should the operation not be possible for the given size, an error
message is printed and no output (not even partial output) is produced.

## Spraying

Spraying is a sort of de-collation. It takes one input file, and writes
pages in turn to one or more outputs:

`cpdf -spray in.pdf -o a.pdf -o b.pdf`

Place odd pages of the input file in one file, and the even in another.

This is the only time more than one `-o` is allowed.

## Encrypting with Split operations

The encryption parameters described in Chapter
FIXME may be added to the command line to encrypt each
split PDF. Similarly, the `-recrypt` switch described in Chapter
FIXME may by given to re-encrypt each file with the
existing encryption of the source PDF.

## Splitting and structure trees

The `-process-struct-trees` option used in conjunction with any
splitting command will trim the structure tree (the data which forms the
logical structure of the PDF) for each output file. In its absence, the
structure tree is preserved wholesale in each output file. Its use can
be important when, for example, producing PDF/UA files.
