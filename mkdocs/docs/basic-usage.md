# Basic Usage

    -help / --help          -summary                 -version
    -o                      -i                       -idir <directory>
    -idir-only-pdfs         -range                   -recrypt                
    -decrypt-force          -stdout                  -stdin
    -stdin-user <password>  -stdin-owner <password>  -producer <text>        
    -creator <text>         -keep-version            -change-id
    -l                      -cpdflin <filename>      -keep-l
    -no-preserve-objstm     -create-objstm           -args <filename>
    -args-json <filename>   -utf8                    -stripped
    -raw                    -gs                      -gs-malformed
    -gs-malformed-force     -gs-quiet                -error-on-malformed
    -progress               -revision                -revisions
    -update

The Coherent PDF tools provide a wide range of facilities for modifying
PDF files created by other means. There is a single command-line program
Cpdf (`cpdf.exe` under Microsoft Windows). The rest of this manual
describes the options that may be given to this program.

## Documentation

The operation `-help / --help` gives pointers to sources of
documentation. The operation `-summary` prints each operation and option
together with a short description. The operation `-version` prints the
Cpdf version string.

## Input and Output Files

The typical pattern for usage is

`cpdf [<operation>] <input file(s)> -o <output file>`

and the simplest concrete example, assuming the existence of a file
`in.pdf` is:

`cpdf in.pdf -o out.pdf`

This copies `in.pdf` to `out.pdf`. Of course, we should like to do more
interesting things to the PDF file than that!

Files on the command line are distinguished from other input by their
containing a period. If an input file does not contain a period, it
should be preceded by `-i`. For example:

`cpdf -i in -o out.pdf`

A whole directory of files may be added (where a command supports
multiple files) by using the `-idir` option:

`cpdf -merge -idir myfiles -o out.pdf`

The files in the directory `myfiles` are considered in alphabetical
order. They must all be PDF files. If the names of the files are
numeric, leading zeroes will be required for the order to be correct
(e.g `001.pdf`, `002.pdf` etc).

To restrict cpdf to files ending in `.pdf` (in upper or lower or mixed
case) add the option `-idir-only-pdfs` *before* `-idir`:

`cpdf -merge -idir-only-pdfs -idir myfiles -o out.pdf`

## Input Ranges

An *input range* may be specified after each input file. This is treated
differently by each operation. For instance

`cpdf in.pdf 2-5 -o out.pdf`

extracts pages two, three, four and five from `in.pdf`, writing the
result to `out.pdf`, assuming that `in.pdf` contains at least five
pages. Here are the rules for building input ranges:

- A number represents a page number

- A page label may be used in place of a number e.g `[iii]` represents
  the first page found which is labelled `iii`.

- A tilde (`~`) defines a page number counting from the end of the
  document rather than the beginning. Page `~1` is the last page, `~2`
  the penultimate page etc.

- A dash (`-`) defines ranges, e.g. `1-5` or `6-3`.

- A comma (`,`) allows one to specify several ranges, e.g. `1-2,4-5`.

- The word `end` represents the last page.

- The words `odd` and `even` can be used in place of or at the end of a
  page range to restrict to just the odd or even pages.

- The words `portrait` and `landscape` can be used in place of or at the
  end of a page range to restrict to just those pages which are portrait
  or landscape. Note that the meaning of "portrait" and "landscape" does
  not take account of any viewing rotation in place (use `-upright` from
  chapter 3 first, if required). A page with equal width and height is
  considered neither portrait nor landscape.

- The word `reverse` is the same as `end-1`.

- The word `all` is the same as `1-end`.

- A range must contain no spaces.

- Prepending `NOT` to a whole page range inverts it.

- Prepending `<n>``DUP` to a whole page range duplicates each page of
  the range `<n>` times.

- The range `annotated` includes in the output just such pages as
  contain one or more annotations.

- The empty range is represented by `empty`.

For example:

`cpdf in.pdf 1,2,7-end -o out.pdf`

Remove pages three, four, five and six from a document.

`cpdf in.pdf 1-16odd -o out.pdf`

Extract the odd pages 1,3,\...,13,15.

`cpdf in.pdf landscape -rotate 90 -o out.pdf`

Rotate all landscape pages by ninety degrees.

`cpdf in.pdf 1,all -o out.pdf`

Duplicate the front page of a document, perhaps as a fax cover sheet.

`cpdf in.pdf ~3-~1 -o out.pdf`

Extract the last three pages of a document, in order.

`cpdf in.pdf 2DUP1-10 -o out.pdf`

Produce the pages 1,1,2,2,\....10,10.

Page ranges may refer to non-existent pages, except in the case where a
PDF file with no pages would be produced - such a file is invalid.

If the file has a structure tree (a.k.a Tagged PDF), it will be
preserved whole. To trim the structure tree to only include the output
pages, and so save space, add `-process-struct-trees` to the command
line.

## Working with Encrypted Documents

In order to perform many operations, encrypted input PDF files must be
decrypted. Some require the owner password, some either the user or
owner passwords. Either password is supplied by writing
`user=<password>` or `owner=<password>` following each input file
requiring it (before or after any range). The document will *not* be
re-encrypted upon writing. For example:

`cpdf in.pdf user=charles -info`\
`cpdf in.pdf owner=fred reverse -o out.pdf`

To re-encrypt the file with its existing encryption upon writing, which
is required if only the user password was supplied, but allowed in any
case, add the `-recrypt` option:

`cpdf in.pdf user=charles reverse -recrypt -o out.pdf`

The password required (owner or user) depends upon the operation being
performed. Separate facilities are provided to decrypt and encrypt files
(See Section FIXME).

When appropriate passwords are not available, the option
`-decrypt-force` may be added to the command line to process the file
regardless.

For decryption with AES256, passwords may be Unicode. However the
password, should it contain non-ASCII characters, must be normalized by
applying the SASLPrep profile (RFC 4013) of the stringprep algorithm
(RFC 3454) using the Normalize and BiDi options. It must then be
converted to UTF8 and truncated to 127 bytes. Cpdf does not perform this
pre-processing -- it takes its passwords from the command line without
processing.

## Standard Input and Standard Output

Thus far, we have assumed that the input PDF will be read from a file on
disk, and the output written similarly. Often it's useful to be able to
read input from `stdin` (Standard Input) or write output to `stdout`
(Standard Output) instead. The typical use is to join several programs
together into a *pipe*, passing data from one to the next without the
use of intermediate files. Use `-stdin` to read from standard input, and
`-stdout` to write to standard input, either to pipe data between
multiple programs, or multiple invocations of the same program. For
example, this sequence of commands (all typed on one line)

    cpdf in.pdf reverse -stdout |
    cpdf -stdin 1-5 -stdout |
    cpdf -stdin reverse -o out.pdf

extracts the last five pages of `in.pdf` in the correct order, writing
them to `out.pdf`. It does this by reversing the input, taking the first
five pages and then reversing the result.

To supply passwords for a file from `-stdin`, use
`-stdin-owner <password>` and/or `-stdin-user <password>`.

Using `-stdout` on the final command in the pipeline to output the PDF
to screen is not recommended, since PDF files often contain compressed
sections which are not screen-readable.

Several Cpdf operations write to standard output by default (for
example, listing fonts). A useful feature of the command line (not
specific to Cpdf) is the ability to redirect this output to a file. This
is achieved with the `>` operator:

`cpdf -info in.pdf > file.txt`

Use the `-info` operation (See Section FIXME), redirecting the
output to `file.txt`.

## Doing Several Things at Once with AND

The keyword `AND` can be used to string together several commands in
one. The advantage compared with using pipes is that the file need not
be repeatedly parsed and written out, saving time.

To use `AND`, simply leave off the output specifier (e.g `-o`) of one
command, and the input specifier (e.g filename) of the next. For
instance:

`cpdf -merge in.pdf in2.pdf AND -add-text "Label"`\
`     AND -merge in3.pdf -o out.pdf`

Merge `in.pdf` and `in2.pdf` together, add text to both pages, append
`in3.pdf` and write to `out.pdf`.

To specify the range for each section, use `-range`:

`cpdf -merge in.pdf in2.pdf AND -range 2-4 -add-text "Label"`
`     AND -merge in3.pdf -o out.pdf`

## Units

When measurements are given to Cpdf, they are in points (1 point = 1/72
inch). They may optionally be followed by some letters to change the
measurement. The following are supported:

  ------ -------------------------------------------
    `pt` Points (72 points per inch). The default.
    `cm` Centimeters
    `mm` Millimeters
    `in` Inches
  ------ -------------------------------------------

For example, one may write `14mm` or `21.6in`. In addition, the
following letters stand for various page dimensions:

  --------- --------------------------------
       `PW` Page width
       `PH` Page height
    `PMINX` Page minimum x coordinate
    `PMINY` Page minimum y coordinate
    `PMAXX` Page maximum x coordinate
    `PMAXY` Page maximum y coordinate
       `CW` Crop box width
       `CH` Crop box height
    `CMINX` Crop box minimum x coordinate
    `CMINY` Crop box minimum y coordinate
    `CMAXX` Crop box maximum x coordinate
    `CMAXY` Crop box maximum y coordinate
       `AW` Art box width
       `AH` Art box height
    `AMINX` Art box minimum x coordinate
    `AMINY` Art box minimum y coordinate
    `AMAXX` Art box maximum x coordinate
    `AMAXY` Art box maximum y coordinate
       `TW` Trim box width
       `TH` Trim box height
    `TMINX` Trim box minimum x coordinate
    `TMINY` Trim box minimum y coordinate
    `TMAXX` Trim box maximum x coordinate
    `TMAXY` Trim box maximum y coordinate
       `BW` Bleed box width
       `BH` Bleed box height
    `BMINX` Bleed box minimum x coordinate
    `BMINY` Bleed box minimum y coordinate
    `BMAXX` Bleed box maximum x coordinate
    `BMAXY` Bleed box maximum y coordinate
  --------- --------------------------------

For example, we may write `PMINX` `PMINY` to stand for the coordinate of
the lower left corner of the page.

Simple arithmetic may be performed using the words `add`, `sub`, `mul`
and `div` to stand for addition, subtraction, multiplication and
division. For example, one may write `14in sub 30pt` or `PMINX mul 2`

## Setting the Producer and Creator

The `-producer` and `-creator` options may be added to any Cpdf command
line to set the producer and/or creator of the PDF file. If the file was
converted from another format, the *creator* is the program producing
the original, the *producer* the program converting it to PDF.

`cpdf -merge in.pdf in2.pdf -producer MyMerger -o out.pdf`

Merge `in.pdf` and `in2.pdf`, setting the producer to `MyMerger` and
writing the output to `out.pdf`.

## PDF Version Numbers

When an operation which uses a part of the PDF standard which was
introduced in a later version than that of the input file, the PDF
version in the output file is set to the later version (most PDF viewers
will try to load any PDF file, even if it is marked with a later version
number). However, this automatic version changing may be suppressed with
the `-keep-version` option. If you wish to manually alter the PDF
version of a file, use the `-set-version` operation described in Section FIXME.

## File IDs

PDF files contain an ID (consisting of two parts), used by some workflow
systems to uniquely identify a file. To change the ID, behavior, use the
`-change-id` operation. This will create a new ID for the output file.

`cpdf -change-id in.pdf -o out.pdf`

Write `in.pdf` to `out.pdf`, changing the ID.

Setting the environment variable `CAMLPDF_REPRODUCIBLE_IDS` to `true`
will use a standard value instead of generating one.

## Linearization

Linearized PDF is a version of the PDF format in which the data is held
in a special manner to allow content to be fetched only when needed.
This means viewing a multipage PDF over a slow connection is more
responsive. By default, Cpdf does not linearize output files. To make it
do so, add the `-l` option to the command line, in addition to any other
command being used. For example:

`cpdf -l in.pdf -o out.pdf`

Linearize the file `in.pdf`, writing to `out.pdf`.

This requires the existence of an external linearize such as `qpdf`. If
you are unable to install the linearizer, you must use `-cpdflin` to let
Cpdf know where to find it:

`cpdf.exe -cpdflin "C:\\qpdf.exe" -l in.pdf -o out.pdf`

Linearize the file `in.pdf`, writing to `out.pdf`.

In extremis, you may place the linearizer and its resources in the
current working directory, though this is not recommended.

To keep the existing linearization status of a file (produce linearized
output if the input is linearized and the reverse), use `-keep-l`
instead of `-l`.

## Object Streams

PDF 1.5 introduced a new mechanism for storing objects to save space:
object streams. by default, Cpdf will preserve object streams in input
files, creating no more. To prevent the retention of existing object
streams, use `-no-preserve-objstm`:

`cpdf -no-preserve-objstm in.pdf -o out.pdf`

Write the file `in.pdf` to `out.pdf`, removing any object streams.

To create new object streams if none exist, or augment the existing
ones, use `-create-objstm`:

`cpdf -create-objstm in.pdf -o out.pdf`

Write the file `in.pdf` to `out.pdf`, preserving any existing object
streams, and creating any new ones for new objects which have been
added.

To create wholly new object streams, use both options together:

`cpdf -create-objstm -no-preserve-objstm in.pdf -o out.pdf`

Write the file `in.pdf` to `out.pdf` with wholly new object streams.

Files written with object streams will be set to PDF 1.5 or higher,
unless `-keep-version` is used (see above).

## Malformed Files

There are many malformed PDF files in existence,
including many produced by otherwise-reputable applications. Cpdf
attempts to correct these problems silently.

Grossly malformed files will be reconstructed. The reconstruction
progress is shown on `stderr` (Standard Error):

`$cpdf in.pdf -o out.pdf`\
`couldn't lex object number`\
`Attempting to reconstruct the malformed pdf in.pdf...`\
`Read 5530 objects`\
`Malformed PDF reconstruction succeeded!`

In the unlikely event that Cpdf cannot reconstruct a malformed file, it
is able to use the `gs` program to try to reconstruct the PDF file, if
you have it installed. For example, if `gs` is installed and in your
path, we might try:

`cpdf -gs gs -gs-malformed in.pdf -o out.pdf`

To suppress the output of `gs` use the `-gs-quiet` option. If the
malformity lies inside an individual page of the PDF, rather than in its
gross structure, Cpdf may appear to succeed in reconstruction, only to
fail when processing a page (e.g when adding text). To force the use of
`gs` to pre-process such files so cpdf cannot fail on them, use
`-gs­-malformed­-force`:

`cpdf in.pdf -gs gs -gs-malformed-force -o out.pdf [-gs-quiet]`

The command line for `-gs-malformed-force` must be of *precisely* this
form. Sometimes, on the other hand, we might wish Cpdf to fail
immediately on any malformed file, rather than try its own
reconstruction process. The option `-error-on-malformed` achieves this.

*Note: Use of these commands with `-gs` is a last resort; they may strip
some metadata from PDF files.*

Sometimes old, pre-ISO standardisation files can be technically
well-formed but use inefficient PDF constructs. If you are sure the
input files you are using are modern ISO-compliant PDFs, the `-fast`
option may be added to the command line (or, if using `AND`, to each
section of the command line). This will use certain shortcuts which
speed up processing, but would fail on a minority of pre-ISO files. The
`-fast` option may be used with:

Chapter FIXME

    -rotate-contents -upright -vflip -hflip
    -shift -scale-page -scale-to-fit -scale-contents -center-to-fit -stretch
    -show-boxes -hard-box -trim-marks

Chapter FIXME

    -add-text -add-rectangle
    -stamp-on -stamp-under -combine-pages

Chapter FIXME

    -impose -impose-xy -twoup -twoup-stack

If problems occur, refrain from using `-fast`.

## Error Handling

When Cpdf encounters an error, it exits with code 2. An error message is
displayed on `stderr` (Standard Error). In normal usage, this means it
is displayed on the screen. When a bad or inappropriate password is
given, the exit code is 1.

## Control Files

`cpdf -args <filename>`\
`cpdf -args-json <filename>`

Some operating systems have a limit on the length of a command line. To
circumvent this, or simply for reasons of flexibility, a control file
may be specified from which arguments are drawn.

Using `-args` or will perform direct textual substitution of the file
into the command line, prior to any other processing.

Using `-args-json` will read arguments from a JSON file consisting of a
single array of strings. For example:

    ["-merge",
     "hello.pdf",
     "cpdfmanual.pdf",
     //Cpdf's JSON parser allows C-style comments
     "-o",
     /* The output file name: */
     "out.pdf"]

## String Arguments

Command lines are handled differently on each operating system. Some
characters are reserved with special meanings, even when they occur
inside quoted string arguments. To avoid this problem, Cpdf performs
processing on string arguments as they are read.

A backslash is used to indicate that a character which would otherwise
be treated specially by the command line interpreter is to be treated
literally. For example, Unix-like systems attribute a special meaning to
the exclamation mark, so the command line

`cpdf -add-text "Hello!" in.pdf -o out.pdf`

would fail. We must escape the exclamation mark with a backslash:

`cpdf -add-text "Hello\!" in.pdf -o out.pdf`

It follows that backslashes intended to be taken literally must
themselves be escaped (i.e. written `\\`).

## Text Encodings

Some Cpdf commands write text to standard output, or read text from the
command line or configuration files. These are:

`-info`\
`-list-bookmarks`\
`-set-author` et al.\
`-list-annotations`\
`-dump-attachments`

There are three options to control how the text is interpreted:

`-utf8`\
`-stripped`\
`-raw`

Add `-utf8` to use Unicode UTF8, `-stripped` to convert to 7 bit ASCII
by dropping any high characters, or `-raw` to perform no processing. The
default unless specified in the documentation for an individual
operation is `-stripped`.

In modern usage, `-utf8` is almost always the sensible option. But for
historical reasons it would be the default.

## Line Endings

For historical reasons, Cpdf uses the Unix line ending character (LF)
when writing text files on Microsoft Windows. For example, bookmark
files.

## Showing Progress

Adding `-progress` to any Cpdf command will show progress on Standard
Error:

    $ cpdf -add-text Private in.pdf -o out.pdf -progress
    *** Operation AddText
    <<< Reading cpdfmanual.pdf
    1.2.3.4.5.6.7.8.9.10.11.12.13.14.15.16.17.18.19.20.21
    >>> Writing out.pdf

For page-by-page operations the page number is printed when the
operation begins, the period when it ends. The `-progress` option, when
combined with `AND`, shows progress for all operations after it is
specified.

## Incremental update

A PDF can contain multiple revisions. These are created when, for
example, an annotation is added in Adobe Reader, or when some other
programs edit files. They are also an important part of the PDF
mechanisms for digital signatures. We can find out how many revisions
are in a PDF:

    $ cpdf -revisions in.pdf
    3

We can retrieve any revision we like. The most recent is revision 1, the
next back revision 2, and so on:

`cpdf in.pdf -revision 2 -o out.pdf`

Note that this extracts exactly revision 2, not a PDF with revisions
2..n. Note also that `-revision` must come after the filename.

We can incrementally update a PDF by adding `-update` to a Cpdf command:

`cpdf in.pdf -rotate 90 -update -o in.pdf`

Note the filename is specified twice.
