% CPDF(1)
% Coherent Graphics Ltd
% February 2026

# NAME

cpdf - PDF command line tools.

# SYNOPSIS

Simple operation:

**cpdf** [in.pdf] \[operation] \[options] \[-o out.pdf]

Operation on password-protected file:

**cpdf** in.pdf \[user=\<password>] \[owner=\<password>] ... \[-o out.pdf]

Multiple operations, one after another:

**cpdf** [in.pdf] \[operation] \[options] **AND** \[operation] \[options] **AND**
... \[-o out.pdf]

# DESCRIPTION

**Cpdf** is an AGPL-licensed command line tool for processing PDF files. The
rest of this document gives a brief description of each command line operation
and option. The file cpdfmanual.pdf which you should find installed on your
system or otherwise at https://www.coherentpdf.com/cpdfmanual.pdf gives the
full usage details.

The sections in this document follow the chapters of cpdfmanual.pdf, for easy
cross-referencing.

# 1. BASIC USAGE

**-version**

: Print the Cpdf version number.

**-help, --help**

: Gives links to sources of help.

**-summary**

: Lists and describes very briefly each command line option.

**-o** \<filename>

: The output filename. Beware of writing back to the input file.

**-i** \<filename>

: Cpdf automatically treats any filename ending with .pdf (any case) as an
input PDF. If your file does not end with .pdf, you can supply the file with
**-i** instead.

**-range** \<range>

: Gives the range of pages to be affected by an operation. By default, all
pages are affected. The range may be specified without **-range** simply by
writing it directly after the filename it relates to. Example ranges: "2" "1-5"
"1,2,3-end" "odd" "NOT1-5". See cpdfmanual.pdf for a full description.

**-progress**

: Show which operations and on which pages they are operating, as it happens,
on standard error.

**-keep-version**

: Keep the PDF version of the input document even if features are used which
would otherwise make it increase.

**-fast**

: Presume ISO-compliant content streams when processing page content. This is
faster, because it does not involve re-parsing whole streams to add content.

**-idir** \<path>

: Add a whole directory of PDFs as inputs.

**-idir-only-pdfs**

: Restrict **-idir** to only files ending in .pdf (any case). Must appear
before **-idir**.

**-recrypt**

: Re-encrypt output files using the same encryption parameters (if any) as the
input file. 

**-decrypt-force**

: Allow the output file to be written decrypted even if permissions would
otherwise prevent it.

**-stdout**

: Write the output file to standard output instead of to a file with **-o**.

**-stdin**

: Read the input PDF from standard input instead of from a named file.

**-stdin-user** \<password>

: Supply the user password for the PDF which is from standard input. 

**-stdin-owner** \<password>

: Supply the owner password the the PDF which is from standard input.

**-producer** \<string>

: Set the producer of the output file.

**-creator** \<string>

: Set the creator of the output file.

**-change-id**

: Change the PDF's ID field when writing the output file.

**-l**

: Linearize the PDF when writing. Requires a linearizer to be supplied with
**-cpdflin**.

**-cpdflin** \<path>

: Give the path of an external linearizer. For example, Qpdf.

**-keep-l**

: Keep the linearization status (either linearized or not) of the input file
upon output. Requires a linearizer to be supplied with **-cpdflin**.

**-no-preserve-objstm**

: Do not preserve existing object streams when writing the output file. Note
that **-create-objstm** and **-no-preserve-objstm** may be used together - the
effect is then to redo all object streams.

**-create-objstm**

: Create new object streams when writing the output file. Note that
**-create-objstm** and **-no-preserve-objstm** may be used together - the
effect is then to redo all object streams.

**-args** \<filename>

: Read command line arguments from the given file by direct textual
substitution into the command line, prior to any other processing. 

**-args-json** \<filename>

: Read command line arguments from a JSON file consisting of a single array of
strings.

**-utf8**

: Read and write string data as UTF8. Almost always the sensible option, and
will become the default in a future version.

**-stripped**

: Convert string output to 7 bit ASCII by dropping any high characters. To be
used with caution.

**-raw**

: Perform no processing on string outputs.

**-gs** \<path>

: A very few of Cpdf's functions rely upon the **gs** command. Its path may be
supplied with **-gs**.

**-gs-malformed**

: This option is used to allow Cpdf to call out to the **gs** command to
pre-process badly malformed files as a last resort.

**-gs-malformed-force**

: See cpdfmanual.pdf for details of this fragile command.

**-gs-quiet**

: Do not show the output of **gs** when used.

**-error-on-malformed**

: Do not attempt to reconstruct malformed files by any method, but exit with an
error.

# 2. MERGING AND SPLITTING

## Merging

**cpdf -merge in1.pdf** \[\<range>] **in2.pdf** \[\<range>] \[\<more
names/ranges>] \[-collate] \[-collate-n \<n>] \[-retain-numbering]
\[-merge-add-bookmarks \[-merge-add-bookmarks-use-titles]]
\[-remove-duplicate-fonts] \[-process-struct-trees] \[-subformat \<subformat>]
**-o out.pdf**

The **-merge** operation allows the merging of several files into one.
Ranges can be used to select only a subset of pages from each
input file in the output. The output file consists of the concatenation of all
the input pages in the order specified on the command line. Actually, the
**-merge** can be omitted, since this is the default operation of Cpdf.

**-collate**

: Instead of ordinary operation, take the first page from the first document,
then the first from the second and so on. Then the second page from the first
document and on until all pages are exhausted.

**-collate-n** \<n>

: Like **-collate**, but in chunks of more than one page.

**-retain-numbering**

: Keep the page numbering of each input file intact, rather than renumbering
the pages in the output document beginning at 1.

**-merge-add-bookmarks \[-merge-add-bookmarks-use-titles]**

: Add a top-level bookmark for each file, using the filename. The option
**-merge-add-bookmarks-use-titles**, when used in conjunction with
**-merge-add-bookmarks**, uses the titles from document metadata instead.

**-remove-duplicate-fonts**

: Ensures that fonts used in more than one input appear only once in the output.

**-process-struct-trees**

: Merge input structure trees in the output.

**-subformat** \<subformat>

: If **-subformat** "PDF/UA-2" is given, together with
**-process-struct-trees**, Cpdf will add a top-level Document structure tree
element.

## Portfolios

A PDF portfolio is a special kind of PDF which contains other documents (PDF and
otherwise) within it. Support is mostly limited to Adobe products at time of
writing.

**cpdf** **-portfolio** **in.pdf** -pf \<filename> \[-pfd \<string>] \[-pfr \<relationship>]
\[-pf ...] **-o out.pdf**

The input **in.pdf** here is the main file. You can build a blank one with **-create-pdf**.

**-pf** \<filename>

: The filename for each file to include in the portfolio.

**-pfd** \<description>

: The description for the file (must appear after **-pf**).

**-pfr** \<relationship>

: The so-called relationship for the file (must appear after **-pf**).

## Splitting

We can split an input PDF into its constituent pages, and output one PDF for
each page or each chunk of pages.

**cpdf -split in.pdf** \[-chunk \<n>] \[-process-struct-trees] \[-utf8] **-o
\<format>**

The output format has many options (see cpdfmanual.pdf for details). But the
simplest is just to number the outputs in sequence. For example **cpdf in.pdf
-o out%%%.pdf** will produce out001.pdf, out002.pdf and so on.

**-chunk** \<n>

: Choose a chunk size other than 1.

**-process-struct-trees**

: Split the input document's structure tree into the output documents.

**-utf8**

: This option may be required in the case of some output formats. See
cpdfmanual.pdf for details.

## Splitting on bookmarks

**cpdf -split-bookmarks \<level> in.pdf** \[-process-struct-trees] \[-utf8] **-o \<format>**

Split on bookmark boundaries at a given level, instead of splitting on each
page. Level 0 is top level, level 1 next, and so on. See above for format
details.

**-process-struct-trees**

: Split the input document's structure tree into the output documents.

**-utf8**

: This option may be required in the case of some output formats. See
cpdfmanual.pdf for details.

## Splitting to a given size


**cpdf -split-max \<n> in.pdf** \[-process-struct-trees] \[-utf8] **-o \<format>
**

Split the file, if possible, to a maximum file size (in bytes) for each output
PDF. See above for format details.

**-process-struct-trees**

: Split the input document's structure tree into the output documents.

**-utf8**

: This option may be required in the case of some output formats. See
cpdfmanual.pdf for details.

## Interleaved splitting

We can use **-spray** to write the split pages to more than one named output
file. When Cpdf runs out of output files, it adds the next page to the first
output file, and so on until all input pages are exhausted.

**cpdf -spray in.pdf** \[-process-struct-trees] \[-utf8] **-o a.pdf** \[-o b.pdf \[-o ...]]

**-process-struct-trees**

: Split the input document's structure tree into the output documents.

**-utf8**

: This option may be required in the case of some output formats. See
cpdfmanual.pdf for details.

# 3. PAGES

**cpdf -scale-page "\<x scale> \<y scale>"** **in.pdf** \[\<range>] \[-fast]
\[\<position>] **-o out.pdf**

Scale pages in the given range by the given factor e.g "2 2". See Chapter 8 for
information on positions.

**cpdf -scale-to-fit "\<x size> \<y size>"** **in.pdf** \[\<range>] \[-fast]
\[-prerotate] \[\<position>] \[-scale-to-fit-scale \<scale>]
\[-scale-to-fit-rotate-clockwise] \[-scale-to-fit-rotate-anticlockwise] **-o
out.pdf**

Scale pages in the given range to fit the given size e.g "a4paper" or "10in
7in", without altering the aspect ratio. By default the content will be
centered on the new page. See Chapter 8 for information on positions.

**-scale-to-fit-scale** \<n>

: Scale to a proportion of the available area, instead of filling it. For
example 0.9 for 90 percent.

**-scale-to-fit-rotate-clockwise -scale-to-fit-rotate-anticlockwise**

: Automatically rotate page to maximise use of area.

**-prerotate**

: Remove any viewing rotation before beginning.

**cpdf -stretch "\<x size> \<y size>"** **in.pdf** \[\<range>] \[-fast] **-o out.pdf**

Scale pages without regard to aspect ratio.

**cpdf -center-to-fit "\<x size> \<y size>"** **in.pdf** \[\<range>] \[-fast] **-o
out.pdf**

Center each page on a new page size, without scaling it.

**cpdf -scale-contents \<scale>** **in.pdf** \[\<range>] \[\<position>] \[-fast] **-o
out.pdf**

Scale the content of pages by a given factor, without changing the size
of the page. See the end of this chapter for position.

**cpdf -shift "\<shift x> \<shift y>"** **in.pdf** \[\<range>] \[-fast] **-o out.pdf**

Shift the content of pages by a given displacement.

**cpdf -shift-boxes "\<shift x> \<shift y>"** **in.pdf** \[\<range>] **-o out.pdf**

Shift the boxes of a page by a given displacement, without moving the content.

**cpdf -rotate \<angle> in.pdf** \[\<range>] **-o out.pdf**

Change the PDF viewing rotation of pages to 0, 90, 180 or 270 degrees clockwise. 

**cpdf -rotateby \<angle> in.pdf** \[\<range>] **-o out.pdf**

Change the PDF viewing rotation of pages by 0, 90, 180 or 270 degrees clockwise. 

**cpdf -upright** **in.pdf** \[\<range>] \[-fast] **-o out.pdf**

The **-upright** operation does whatever combination of **-rotate** and
**-rotate-contents** is required to change the rotation of the document to zero
without altering its appearance. In addition, it makes sure the media box has
its origin at (0,0), changing other boxes to compensate.

**cpdf -rotate-contents \<angle>** **in.pdf** \[\<range>] \[-fast] **-o out.pdf**

Rotates the content of the page around its center point by the given angle.

**cpdf -hflip** **in.pdf** \[\<range>] \[-fast] **-o out.pdf**

Flip page content horizontally.

**cpdf -vflip** **in.pdf** \[\<range>] \[-fast] **-o out.pdf**

Flip page content vertically.

**cpdf -\[media | crop | art | trim | bleed]box \<boxspec> in.pdf** \[\<range>] **-o out.pdf**

Set the media, crop, art, trim or bleed box. For example **-cropbox "50 50 300
200"** sets minx 50, miny 50, width 300, height 200. To use absolute numbers
instead of width and height, prefix with a question mark, writing **"?50 50 350
250"** instead.

**cpdf -remove-\[crop | art | trim | bleed]box in.pdf** \[\<range>] **-o out.pdf**

Remove a crop, art, trim or bleed box from pages.

**cpdf -frombox \<boxname> -tobox \<boxname>** **in.pdf** \[\<range>] \[-mediabox-if-missing] **-o out.pdf**

Copy a box to another. For example from **/TrimBox** to **/CropBox**.

**-mediabox-if-missing**

: Use media box, rather than failing, if the **-frombox** is missing.

**cpdf -hard-box \<boxname>** **in.pdf** \[\<range>] \[-fast] \[-mediabox-if-missing] **-o out.pdf**

Create a hard box for a given box name - that is to say, one which clips its content.

**cpdf -show-boxes** **in.pdf** \[\<range>] \[-fast] **-o out.pdf**

Show the media, crop, art, trim, and bleed boxes in Red, Green, Blue, Orange
and Pink respectively.

**cpdf -trim-marks** **in.pdf** \[\<range>] \[-fast] **-o out.pdf**

Add trim marks to a PDF. The trim box must be present.

# 4. ENCRYPTION AND DECRYPTION

**cpdf -encrypt \<method> \[-pw=]\<owner password> \[-pw=]\<user password>** **in.pdf** \[-no-encrypt-metadata]
\<permissions> **-o out.pdf**

Encrypt a document given the method (e.g **AES256ISO** for modern usage), owner
and user passwords, and optionally permissions. E.g **cpdf -encrypt AES256ISO
secret "" in.pdf -o out.pdf**.

**-no-encrypt-metadata**

: Do not encrypt metadata (AES encryption only).

**-pw=**\<password>

: Useful if a password may begin with a dash.

Permissions:

**-no-edit**

: Cannot change the document.

**-no-print**

: Cannot print the document.

**-no-copy**

: Cannot select or copy text or graphics.

**-no-annot**

: Cannot add or change form fields or annotations.

**-no-forms**

: Cannot edit form fields.

**-no-extract**

: Cannot extract text or graphics.

**-no-assemble**

: Cannot, for example, merge files.

**-no-hq-print**

: Cannot print high-quality.

Note: Adobe Acrobat and Adobe Reader may show slightly different permissions in info dialogues – this
is a result of policy changes and not a bug in Cpdf. You may need to experiment.

**cpdf -decrypt** **in.pdf owner=\<owner password>** \[-decrypt-force] **-o out.pdf**

: Decrypt a document, given its owner password.

**-decrypt-force**

: Decrypt even without the owner password, using just the user password. The
user password is often blank.

# 5. COMPRESSION

**cpdf -decompress** **in.pdf** \[-just-content] \[-jbig2dec \<path>] **-o out.pdf**

Decompress the streams in a PDF file, for instance to manually inspect it.

**-just-content**

: Only decompress page content streams.

**-jbig2dec** \<path>

: Give the path to the jbig2dec program which Cpdf uses, if available, to
decompress JBIG2 streams.

**cpdf -compress in.pdf -o out.pdf**

Compress any streams which are uncompressed using the FlateDecode method,
with the exception of metadata streams.

**cpdf -squeeze in.pdf** \[-squeeze-log-to \<filename>] \[-squeeze-no-pagedata] **-o out.pdf**

Squeeze a file by coalescing like objects, and various other methods.

**-squeeze-log-to** \<filename>

: Write the squeeze log to file rather than standard output.

**-squeeze-no-pagedata**

: Avoid processing page data, making the squeeze process much faster at the cost of a little compression.

**cpdf -remove-article-threads in.pdf -o out.pdf**

Remove article threads.

**cpdf -remove-page-piece in.pdf -o out.pdf**

Remove page piece information.

**cpdf -remove-web-capture in.pdf -o out.pdf**

Remove web capture data.

**cpdf -remove-procsets in.pdf -o out.pdf**

Remove ProcSets, a now-irrelevant data structure from early PDFs.

**cpdf -remove-output-intents in.pdf -o out.pdf**

Remove output intents, a colour-matching system for documents intended to be printed.

# 6. BOOKMARKS

**cpdf -list-bookmarks** **in.pdf** \[-utf8]

Print bookmark data to standard output. The data includes level, title,
page number linked to, and full link data. Use **-utf8** always. For example:

```
0 "Part 1" 1 open
1 "Part 1A" 2 "[2 /XYZ 200 400 null]"
1 "Part 1B" 3
0 "Part 2" 4
1 "Part 2a" 5
```

**cpdf -list-bookmarks-json** **in.pdf** \[-preserve-actions]

Print bookmark data to standard output in JSON format instead. Here is a single
entry in the JSON array of bookmarks:

```
{ "level": 0,
  "text": "1 Basic Usage",
  "page": 17,
  "open": false,
  "target":
    [ { "I": 17 },
      { "N": "/XYZ" },
      { "F": 85.039 },
      { "F": 609.307 },
      null ]
  "colour": [ 0.0, 0.0, 0.0 ],
  "italic": false,
  "bold": false
}
```

**-preserve-actions**

: Instead of resolving complex destinations types to simple ones, keep the originals.

**cpdf -remove-bookmarks in.pdf -o out.pdf**

Remove all bookmarks from a PDF.

**cpdf -add-bookmarks \<filename> in.pdf -o out.pdf**

Add bookmarks, given an old-style bookmark file.

**cpdf -add-bookmarks-json \<filename> in.pdf -o out.pdf**

Add bookmarks, given a new-style JSON bookmark file.

**cpdf -bookmarks-open-to-level \<n> in.pdf -o out.pdf**

Set all bookmarks up to and including a given level to be open.

**cpdf -table-of-contents** **in.pdf** \[-toc-title \<string>] \[-toc-no-bookmark] \[-toc-dot-leaders]
\[-font \<font>] \[-font-size \<n>] \[-embed-std14 \<path>]
\[-process-struct-trees] \[-subformat \<subformat>] **-o out.pdf**

Generate a typeset table of contents from existing bookmarks, adding it to
the beginning of the document.

**-toc-title** \<string>

: Title (default is "Table of Contents").

**-toc-no-bookmark**

: Do not add an entry for the new table of contents in the document's bookmarks.

**-toc-dot-leaders**

: Add dot leaders.

**-font** \<font>

: Give the font (default Times Roman).

**-font-size** \<n>

: Give the font size (default 12pt).

**-embed-std14**

: Embed standard 14 fonts given their path (see cpdfmanual.pdf for details).

**-process-struct-trees**

: Create a structure tree for the new table of contents and merge it with the
document's.

**-subformat** \<subformat>

: Add **-subformat "PDF/UA-2"** when adding a table of contents to a PDF/UA-2
file to keep compatibility.

# 7. PRESENTATIONS

**cpdf -presentation in.pdf** \[\<range>] **-o out.pdf** \[-trans \<transition-name>]
\[-duration \<float>] \[-vertical] \[-outward] \[-direction \<n>]
\[-effect-duration \<float>]

Make a slide-show presentation from a PDF.

**-trans** \<transition>

The transition style, one of **Split**, **Blinds**, **Box**, **Wipe**,
**Dissolve**, **Glitter**.

**-duration** \<n>

: Time in seconds before presentation advances (default: no automatic
advancement).

**-vertical**

: Select vertical blinds for transition type Blinds.

**-outward**

: Select outward sweep for transition type Box.

**-direction** \<n>

: Direction for Wipe and Glitter styles. See cpdfmanual.pdf for full
information.

**-effect-duration** \<n>

: Length in seconds of the transition itself.

# 8. TEXT AND STAMPS

**cpdf \[-stamp-on | -stamp-under] stamp.pdf** **in.pdf** \[\<range>]
\[-scale-stamp-to-fit] \[\<positioning command>] \[-relative-to-cropbox]
\[-process-struct-trees] \[-fast] **-o out.pdf**

Stamp a one-page PDF over or under each page in the given range. The
positioning commands described later in this chapter may be used to choose
where to stamp it (default, bottom left of media box).

**-scale-stamp-to-fit**

: Scale the stamp to fit the page before applying it.

**-relative-to-cropbox**

: Take the positioning command relative to the crop box rather than the media box.

**-process-struct-trees**

: Maintain tagged PDF. The main file will keep its structure; the stamp will be
marked as an artifact.

**cpdf -combine-pages over.pdf under.pdf** [-fast] [-prerotate] [-no-warn-rotate]
[-process-struct-trees] [-underneath] [-stamp-scale-to-fit] **-o out.pdf**

Combine the pages of two PDFs, page 1 of the first with page 1 of the second
and so on.

**-prerotate**

: Remove any rotation differences between the files before combining.

**-no-warn-rotate**

: Do not warn of unresolved rotation differences.

**-underneath**

: Reverse the order of "over" and "under" files.

**-process-struct-trees**

: Maintain tagged PDF. The "under" file will keep its structure; the "over"
file will be marked as an artifact.

**cpdf \[-add-text \<string> | -add-rectangle "\<x size> \<y size>"]** [-font \<fontname>]
[-font-size \<n>] [-load-ttf \<name>=\<filename>] [-embed-std14 \<path>]
[-color \<color>] [-line-spacing \<n>] [-outline] [-linewidth \<n>]
[-underneath] [-relative-to-cropbox] [-prerotate] [-no-warn-rotate] [-bates
\<n>] [-bates-at-range \<n>] [-bates-pad-to \<n>] [-opacity
\<n>] [-midline] [-topline] [-fast] [-process-struct-trees] **in.pdf**
\[\<range>] **-o out.pdf**
 
Add text to a PDF. Various special codes for page numbers or time and date
may be used. For example:

```
%Page          Page number in arabic notation (1, 2, 3. . . )
%PageDiv2      Page number in arabic notation divided by two
%roman         Page number in lower-case roman notation (i, ii, iii. . . )
%Roman         Page number in upper-case roman notation (I, II, III. . . )
%EndPage       Last page of document in arabic notation
%Label         The page label of the page
%EndLabel      The page label of the last page
%filename      The full file name of the input document
%URL[text|URL] Add text, which links to URL (does not work for diagonal text)
%Bookmark<n>   Bookmark text at level n (0, 1, 2, 3, 4)
%Bates         Bates number
```

And date and time formats:

```
%a Abbreviated weekday name (Sun, Mon etc.)
%A Full weekday name (Sunday, Monday etc.)
%b Abbreviated month name (Jan, Feb etc.)
%B Full month name (January, February etc.)
%d Day of the month (01-31)
%e Day of the month (1-31)
%H Hour in 24-hour clock (00-23)
%I Hour in 12-hour clock (01-12)
%j Day of the year (001-366)
%m Month of the year (01-12)
%M Minute of the hour (00-59)
%p "a.m" or "p.m"
%S Second of the minute (00-61)
%T Same as %H:%M:%S
%u Weekday (1-7, 1 = Sunday)
%w Weekday (0-6, 0 = Sunday)
%Y Year (0000-9999)
%% The % character
```

\\n may be used to demarcate multiple lines.

**-font** \<font>

: Give the font (default Times Roman). Options are:

```
Times-Roman
Times-Bold
Times-Italic
Times-BoldItalic
Helvetica
Helvetica-Bold
Helvetica-Oblique
Helvetica-BoldOblique
Courier
Courier-Bold
Courier-Oblique
Courier-BoldOblique
```

**-font-size** \<n>

: Give the font size (default 12pt).

**-load-ttf** \<name>=\<filename>

: Load a truetype font, and give it name which may be used with -font. For
example **-load-ttf A=NotoSans-Black.ttf**.

**-embed-std14** \<path>

: Embed the standard 14 fonts given the path to the URW Base35 free fonts.

**-color** \<color>

: Choose the text colour using one (Grey), three (RGB), or four (CMYK) numbers
from 0-1. E.g **-color "0.5 0.4 0.5"**.

**-line-spacing** \<n>

: Set the spacing for multi-line text (default 1).

**-outline**

: Use outline text.

**-linewidth** \<n>

: Line width for outline text.

**-underneath**

: Put the text underneath the page instead of on top of the page.

**-relative-to-cropbox**

: Positions are relative to the crop box, rather than the media box.

**-prerotate**

: Remove any viewing rotation before adding text.

**-no-warn-rotate**

: Do not warn of unresolved viewing rotation.

**-bates** \<n>

: Set the bates number for use with **%Bates**.

**-bates-at-range** \<n>

: Set the bates number for the first page in the range.

**-bates-pad-to** \<n>

: Pad bates numbers to a given number of leading zeros.

**-opacity** \<n>

: Set text opacity. Wholly opaque is 1, wholly transparent is 0.

**-midline**

: Position is relative to the midline of text rather than the baseline.

**-topline**

: Position is relative to the topline of text rather than the baseline.

**-process-struct-trees**

: Maintain tagged PDF, for example with PDF/UA. The main file will keep its
structure; the stamped text will be marked as an artifact.

Positioning commands:

**-pos-left** "\<x> \<y>"

: Position the left of the baseline of the text at (x, y).

**-pos-center** "\<x> \<y>"

: Position the center of the baseline of the text at (x, y).

**-pos-right** "\<x> \<y>"

: Position the right of the baseline of the text at (x, y).

**-top** \<n>

: Position the baseline of the text n pts from the top middle of the page.

**-topleft** \<n>

: Position the left of the baseline of the text n pts below and right of the
top left of the page.

**-topleft** "\<x> \<y>"

: Position the left of the baseline of the text y pts below and x pts right
of the top left of the page.

**-topright** \<n>

: Position the right of the baseline of the text n pts below and left of the
top right of the page.

**-topright** "\<x> \<y>"

: Position the right of the baseline of the text y pts below and x pts left
of the top right of the page.

**-left** \<n>

: Position the left of the baseline of the text n pts right of the left middle
of the page.

**-bottomleft** \<n>

: Position the left of the baseline of the text n pts up and right of the
bottom left of the page.

**-bottomleft** "\<x> \<y>"

: Position the left of the baseline of the text y pts up and x pts right of the
bottom left of the page.

**-bottom** \<n>

: Position the center of the baseline of the text n pts up from the bottom
middle of the page.

**-bottomright** \<n>

: Position the right of the baseline of the text n pts up and left from the
bottom right of the page.

**-bottomright** "\<x> \<y>"

: Position the right of the baseline of the text y pts up and x pts left from
the bottom right of the page.

**-right** \<n>

: Position the right of the baseline of the text n pts left of the center right
of the page.

**-diagonal**

: Position text diagonally, bottom left to top right.

**-reverse-diagonal**

: Position text diagonally, top left to bottom right.

**-center**

: Position text centered on the page.

**-justify-left**

: Set left justification for multi-line text. Default depends upon position.

**-justify-right**

: Set right justification for multi-line text. Default depends upon position.

**-justify-center**

: Set center justification for multi-line text. Default depends upon position.

**cpdf -remove-text in.pdf** \[\<range>] **-o out.pdf**

Remove text previously added by Cpdf.

**cpdf -prepend-content \<content> in.pdf** \[\<range>] **-o out.pdf**

A low-level operation to prepend raw content to the beginning of page streams.

**cpdf -postpend-content \<content> in.pdf** \[\<range>] **-o out.pdf**

A low-level operation to postpend raw content to the end of page streams.

**cpdf -stamp-as-xobject stamp.pdf in.pdf** \[\<range>] **-o out.pdf**

A low-level operation to add stamp.pdf as a Form XObject in the given pages
of a PDF and write to standard output its name. 

# 9. MULTIPAGE FACILITIES

**cpdf [-pad-before | -pad-after] in.pdf** \[\<range>] \[-pad-with pad.pdf] **-o out.pdf**

Add blank pages before or after each page in the given range.

**-pad-with** \<filename>

: Use a separate PDF to act as the padding.

**cpdf -pad-every \<n> in.pdf** \[-pad-with pad.pdf] **-o out.pdf**

: Add a blank page after every n pages.

**-pad-with** \<filename>

: Use a separate PDF to act as the padding.

**cpdf [-pad-multiple | -pad-multiple-before]** \[\<n>] **in.pdf -o out.pdf**

Add as many blank pages as are required before or after the original pages to
make the file's length a multiple of the given number of pages

**cpdf -redact** **in.pdf** \[\<range>] \[-process-struct-trees] **-o out.pdf**

Remove the content of the pages in the given range entirely, including
annotations and any resources. 

**-process-struct-trees**

: Process the document's structure tree to remove any parts which are marked as
relating to the now-redacted pages.

**cpdf \[-impose \<pagesize> | impose-xy "\<x> \<y>"]** **in.pdf**
\[-impose-columns] \[-impose-rtl] \[-impose-btt] \[-impose-margin \<n>]
\[-impose-spacing \<n>] \[-impose-linewidth \<n>] \[-fast]
\[-process-struct-trees] **-o out.pdf**

Cpdf has two imposition operations:

**-impose** fits multiple pages onto a given page size e.g **-impose
a4portrait** or **-impose "8in 12in"**

**-impose-xy** builds an output page with the x pages horizontally and y pages
vertically e.g **-impose-xy "2 3"**

**-impose-columns**

: Lay the pages out in columns rather than rows.

**-impose-rtl**

: Lay the pages out right to left.

**-impose-btt**

: Lay the pages out bottom to top.

**-impose-margin** \<n>

: Add a margin around the edge of each page. When using **-impose-xy** the page
size increases; with **-impose** the pages are scaled.

**-impose-spacing** \<n>

: Add spacing between rows and columns. When using **-impose-xy** the page size
increases; with **-impose** the pages are scaled.

**-impose-linewidth** \<n>

: Add a border around each input page. With **-impose** the pages are scaled after
the border is added, so you must account for this yourself.

**-process-struct-trees**

: Mark the file's content as an artifact for the purposes of imposition.

**cpdf -twoup-stack** **in.pdf** \[-fast] \[-process-struct-trees] **-o out.pdf**

**cpdf -twoup** **in.pdf** \[-fast] \[-process-struct-trees] **-o out.pdf**

Two old imposition functions which can now both be done with **-impose** /
**-impose-xy**. The **-twoup-stack** operation puts two logical pages on each
physical page, rotating them 90 degrees to do so. The new mediabox is thus
larger.  Whereas the **-twoup** operation does the same, but scales the new
sides down so that the media box is unchanged.

**-process-struct-trees**

: Mark the file's content as an artifact for the purposes of imposition.

**cpdf -chop "\<x> \<y>"** **in.pdf** \[\<range>] \[-chop-columns] \[-chop-rtl] \[-chop-btt] **-o out.pdf**

The **-chop** operation cuts up a page into multiple pages, according to the
chosen grid.

**-chop-columns**

: Arrange by columns instead of rows.

**-chop-rtl**

: Arrange right to left.

**-chop-btt**

: Arrange bottom to top.

**cpdf \[-chop-h \<y> | -chop-v \<x>]** **in.pdf** \[\<range>] \[-chop-columns] **-o out.pdf**

Chop each page into two, vertically or horizontally, at the given position.
E.g **-chop-h 200pt**.

**-chop-columns**

: Reverse the order of pages in the output.

# 10. ANNOTATIONS

**cpdf -list-annotations in.pdf** \[\<range>]

List textual content of annotations to standard output. Each annotation is
preceded by the page number and followed by a newline.

**cpdf -list-annotations-json in.pdf** \[\<range>]

List annotation objects in JSON format. This includes all ancillary objects,
to support round-tripping. See cpdfmanual.pdf for full format details.

**cpdf -set-annotations \<filename>** **in.pdf** \[\<range>] \[-underneath] **-o out.pdf**

Set annotations from a JSON file. They will be added on top of any
annotations already present.

**-underneath**

: Put the annotations underneath instead of on top of existing ones.

**cpdf -copy-annotations from.pdf to.pdf** \[\<range>] **-o out.pdf**

Copy annotations from **from.pdf** to **to.pdf**.

**cpdf -remove-annotations in.pdf** \[\<range>] **-o out.pdf**

Remove annotations from the document.

# 11. DOCUMENT INFORMATION AND METADATA

**cpdf -info\[-json]** **in.pdf** \[-utf8] \[-in | -cm | -mm]

Print info about a document in plain text (**-info**) or JSON (**-info-json**). For example:

```
cpdf -info pdf_reference.pdf
Encryption: Not encrypted
Permissions: 
Linearized: true
Object streams: true
ID: <0b1f990718e2a92c0c112fbf08b233fb> <b2f1dbee369e11d9b951000393c97fd8>
Version: 1.5
Pages: 1236
Title: PDF Reference, version 1.6
Author: Adobe Systems Incorporated
Subject: Adobe Portable Document Format (PDF)
Keywords: 
Creator: FrameMaker 7.0
Producer: Acrobat Distiller 6.0.1 for Macintosh
Created: D:20041114084116Z
Modified: D:20041114163850-08'00'
Trapped: False
PageMode: UseOutlines
PageLayout: 
OpenAction: [1/XYZ -32768 -32768 1]
HideToolbar: 
HideMenubar: 
HideWindowUI: 
FitWindow: 
CenterWindow: 
DisplayDocTitle: True
NonFullScreenPageMode: 
AcroForm: False
XFA: False
Marked: False
UserProperties: False
Suspects: False
MediaBox: 0.000000 0.000000 612.000000 792.000000
CropBox: 41.000000 63.000000 572.000000 729.000000
BleedBox: 
TrimBox: various
ArtBox: various
Subformats: 
Language: en-us
XMP dc:title: PDF Reference, version 1.6
XMP dc:creator: Adobe Systems Incorporated
XMP dc:description: Adobe Portable Document Format (PDF)
```

**-utf8**

: Output in UTF8 format (plain text only)

**-in** / **-mm** / **-cm**

: Output dimensions in inches, millimetres or centimetres instead of points.

**cpdf -page-info\[-json]** **in.pdf** \[\<range>] \[-in | -cm | -mm]

List page information in plain (**-page-info**) or JSON (**-page-info-json**) format.

```
cpdf -page-info 14psfonts.pdf
Page 1:
Label: i
MediaBox: 0.000000 0.000000 600.000000 450.000000
CropBox: 200.000000 200.000000 500.000000 500.000000
BleedBox:
TrimBox:
ArtBox:
Rotation: 0
Annotations: 0
```

**-in** / **-mm** / **-cm**

: Output dimensions in inches, millimetres or centimetres instead of points.

**cpdf -pages in.pdf**

Print the number of pages in the file.

**cpdf -set-title \<string>** **in.pdf** \[-also-set-xmp] \[-just-set-xmp] **-o out.pdf**

**cpdf -set-author \<string>** **in.pdf** \[-also-set-xmp] \[-just-set-xmp] **-o out.pdf**

**cpdf -set-subject \<string>** **in.pdf** \[-also-set-xmp] \[-just-set-xmp] **-o out.pdf**

**cpdf -set-keywords \<string>** **in.pdf** \[-also-set-xmp] \[-just-set-xmp] **-o out.pdf**

**cpdf -set-creator \<string>** **in.pdf** \[-also-set-xmp] \[-just-set-xmp] **-o out.pdf**

**cpdf -set-producer \<string>** **in.pdf** \[-also-set-xmp] \[-just-set-xmp] **-o out.pdf**

Set metadata.

**cpdf -set-create \<date>** **in.pdf** \[-also-set-xmp] \[-just-set-xmp] **-o out.pdf**

**cpdf -set-modify \<date>** **in.pdf** \[-also-set-xmp] \[-just-set-xmp] **-o out.pdf**

Set creation or modification dates. See Appendix A of cpdfmanual.pdf for details of the PDF date format.

**cpdf -set-trapped in.pdf** \[-also-set-xmp] \[-just-set-xmp] **-o out.pdf**

**cpdf -set-untrapped in.pdf** \[-also-set-xmp] \[-just-set-xmp] **-o out.pdf**

Set the trapped status of a PDF.

**-also-set-xmp**

: Set the XMP metadata if the field is present, in addition to setting the
old-style PDF metadata.

**-just-set-xmp**

: Just set the XMP metadata, if the field is present, in addition to the old-style PDF metadata.

**cpdf -set-page-layout \<layout> in.pdf -o out.pdf**

Set the page layout: one of **SinglePage**, **OneColumn**, **TwoColumnLeft**,
**TwoColumnRight**, **TwoPageLeft**, **TwoPageRight**

**cpdf -set-page-mode \<mode> in.pdf -o out.pdf**

Set the page mode: one of **UseNone**, **UseOutlines**, **UseThumbs**,
**FullScreen**, **UseOC**, **UseAttachments**.

**cpdf -set-non-full-screen-page-mode \<mode> in.pdf -o out.pdf**

Set the non full screen mode for a PDF with FullScreen page mode: one of
**UseNone**, **UseOutlines**, **UseThumbs**, **UseAttachments**.

**cpdf -hide-toolbar \<true | false> in.pdf -o out.pdf**

Hide or reveal the viewer's toolbar.

**cpdf -hide-menubar \<true | false> in.pdf -o out.pdf**

Hide or reveal the viewer's menubar.

**cpdf -hide-window-ui \<true | false> in.pdf -o out.pdf**

Hide or reveal the window's scroll bars.

**cpdf -fit-window \<true | false> in.pdf -o out.pdf**

Resize the document's windows to fit the size of the first page.

**cpdf -center-window \<true | false> in.pdf -o out.pdf**

Position the document window in the center of the screen.

**cpdf -display-doc-title \<true | false> in.pdf -o out.pdf**

Display the document title instead of the file name in the title bar.

**cpdf -open-at-page \<n> in.pdf -o out.pdf**

Set the document to open at the given page number.

**cpdf -open-at-page-fit \<n> in.pdf -o out.pdf**

Set the document to open at the given page number scaled to fit the window.

**cpdf -open-at-page-custom \<destination> in.pdf -o out.pdf**

Set the document to open at a custom destination. See cpdfmanual.pdf for details.

**cpdf -set-language \<language> in.pdf -o out.pdf**

Set the document's global language, for example "en-US"

**cpdf -set-metadata \<filename> in.pdf -o out.pdf**

Replace or add XMP metadata.

**cpdf -remove-metadata in.pdf -o out.pdf**

Remove the main XMP metadata stream.

**cpdf -remove-all-metadata in.pdf -o out.pdf**

Remove all XMP metadata streams.

**cpdf -print-metadata in.pdf**

Print the XMP metadata stream to standard output.

**cpdf -extract-all-metadata in.pdf -o \<path>**

Extract XMP metadata streams to the given directory.

**cpdf -create-metadata in.pdf -o out.pdf**

Create or replace XMP metadata from old-style metadata.

**cpdf -set-metadata-date \<date> in.pdf -o out.pdf**

Set the XMP metadata date. See Appendix A in cpdfmanual.pdf for date format details.

**cpdf -add-page-labels in.pdf** \[\<range>] \[-label-style \<style>]
\[-label-prefix \<string>] \[-label-startval \<n>] \[-labels-progress] **-o out.pdf** 

Add page labels to a PDF. Multiple calls may be used to add multiple ranges of page labels.

**-label-style**

: One of **DecimalArabic**, **LowercaseRoman**, **UppercaseRoman**, **LowercaseLetters**,
**UppercaseLetters**, **NoLabelPrefixOnly**.

**-label-prefix**

: The textual prefix for these labels.

**-label-startval**

: By default the labels begin at page number 1 for each range. To override
this, we can use **-label-startval**.

**-labels-progress**

: make sure the start value progresses between sub-ranges when the page range
specified is disjoint, e.g "1-9, 30-40" or "odd".

**cpdf -remove-page-labels in.pdf -o out.pdf**

Remove the page labels.

**cpdf -print-page-labels\[-json] in.pdf**

Print the page labels in plain text (**-print-page-labels**) or JSON
(**-print-page-labels-json**) format.

**cpdf -composition\[-json] in.pdf**

Print the composition of a PDF, showing how much space is taken by images, fonts etc.

```
cpdf -composition cpdfmanual.pdf
Images: 0 bytes (0.00%)
Fonts: 144731 bytes (46.72%)
Content streams: 132767 bytes (42.85%)
Structure Info: 0 bytes (0.00%)
Attached Files: 0 bytes (0.00%)
XRef Table: 21082 bytes (6.80%)
Piece Info: 0 bytes (0.00%)
Unclassified: 11229 bytes (3.62%)
```

# 12. FILE ATTACHMENTS

**cpdf -attach-file \<filename>** **in.pdf** \[-to-page \<n>] \[-afd \<string>]
\[-afr \<relationship>] \[-attach-file ...] **-o out.pdf**

Attach a file to a PDF, given its filename.

**-to-page** \<n>

: Instead of attaching globally, add it as an attachment on a single page.

**-afd** \<string>

: Supply the textual description for this attachment. 

**-afr** \<relationship>

: Supply the so-called Relationship for this attachment.

**cpdf -list-attached-files** **in.pdf** \[-json] \[-include-data]

List attached files.

**-json**

: List attached files in JSON format. See cpdfmanual.pdf for full details.

**-include-data**

: Include the attachment file data when listing in JSON format.

**cpdf -remove-files in.pdf -o out.pdf**

Remove attached files from a PDF.

**cpdf -dump-attachments in.pdf -o \<path>**

Extract the attachments to a given directory.

# 13. IMAGES

**cpdf -list-images\[-json]** **in.pdf** \[\<range>] \[-inline]

List the images in a file together with their object number, page numbers,
image name, width, height, size in bytes, bits per pixel, colour space,
compression method, mask type and mask object number. Either in plain text
(**-list-images**) or JSON (**-list-images-json**).

**-inline**

: Also list inline images.

**cpdf -list-images-used\[-json]** **in.pdf** \[\<range>] \[-inline] 

**cpdf -image-resolution\[-json] \<n>** **in.pdf** \[\<range>] \[-inline] 

The **-list-images-used** and **-list-images-used-json** operations list the images
at point of use with their page number, image name, width in pixels, height in
pixels, x resolution, y resolution and object number. Using **-image-resolution**
or **-image-resolution-json** instead reports only such images as do not reach a
minimum resolution.

**-inline**

: Also list inline images.

**cpdf -extract-images in.pdf** \[\<range>] \[-im \<path>] \[-p2p \<path>] \[-dedup |
-dedup-perpage] \[-raw] \[-inline] \[-merge-masks] **-o \<path>**

Extract images to a given path e.g "output/%%%", which would create
output/001.jpg, output/002.png and so on. JPEG, JPEG2000 and lossless JBIG2
images are extracted directly. Lossy JBIG2 images are extracted in sections.
Other images are written as PNGs, processed with ImageMagick.

**-im** \<path>

: Provide path to ImageMagick.

**-p2p** \<path>

: Provide path to pnmtopng if ImageMagick not available.

**-dedup**

: Deduplicate images, extracting only once even if multiply-included.

**-dedup-per-page**

: Deduplicate images, but only per-page.

**-raw**

: Output .pnm files instead of .png.

**-inline**

: Also extract inline images.

**-merge-masks**

: Merge soft masks with their images when extracting.

**cpdf -extract-single-image \<object number>** **in.pdf** \[-im \<path>] \[-p2p \<path>]
\[-raw] \[-merge-masks] **-o \<filename>**

Extract a single image, given its object number. Other flags as above.

**cpdf -process-images** **in.pdf** \[\<range>] \[-process-images-info]
\[-process-images-force] \[-im \<filename>] [-jbig2enc \<filename>] \[-jbig2dec
\<filename>] [-lossless-resample[-dpi] \<n> | -lossless-to-jpeg \<n>]
\[-jpeg-to-jpeg \<n>] \[-jpeg-to-jpeg-scale \<n>] \[-lossless-to-jpeg2000 \<n>]
\[-jpeg2000-to-jpeg2000 \<n>] \[-jpeg-to-jpeg-dpi \<n>] \[-1bpp-method
\<method>] \[-jbig2-lossy-threshold \<n>] \[-pixel-threshold \<n>]
\[-length-threshold \<n>] \[-percentage-threshold \<n>] \[-dpi-threshold \<n>]
\[-resample-interpolate] **-o out.pdf**

Re-process existing images within the PDF, typically to reduce size. For a
full commentary, see cpdfmanual.pdf.

**-process-images-info**

: Show the work being done.

**-process-images-force**

: Process image even when size would increase.

**-im** \<path>

: Supply path to ImageMagick.

**-jbig2enc** \<path>

: Supply path to jbig2enc.

**-jbig2dec** \<path>

: Supply path to jbig2dec.

**-lossless-resample** \<n>

: Resample lossless images to the given percentage scale.

**-lossless-resample-dpi** \<n>

: Resample lossless images to a given DPI resolution.

**-lossless-to-jpeg** \<n>

: Convert lossless images to JPEG at the given quality level.

**-jpeg-to-jpeg** \<n>

: Reprocess JPEG images to the given quality level.

**-jpeg-to-jpeg-scale** \<n>

: Reprocess JPEG images to the given percentage scale.

**-jpeg-to-jpeg-dpi** \<n>

: Reprocess JPEG images to the given DPI resolution.

**-lossless-to-jpeg2000** \<n>

: Convert lossless images to JPEG 2000 at the given quality level.

**-jpeg2000-to-jpeg2000** \<n>

: Reprocess JPEG 2000 images to the given quality level.

**-1bpp-method** \<1bppmethod>

: Specify method for 1bpp images. One of **JBIG2Lossy**, **JBIG2Lossless**, **CCITT**, **CCITTG4**.

**-jbig2-lossy-threshold** \<n>

: Set the threshold for lossy JBIG2 compression.

**-pixel-threshold** \<n>

: Images below this number of pixels not processed (default 25).

**-length-threshold** \<n>

: Images with less than this number of bytes not processed (default 100).

**-percentage-threshold** \<n>

: Results not below this percentage of original size discarded (default 90).

**-dpi-threshold** \<n>

: Only image above this threshold at all use points processed (default no dpi threshold).

**-resample-interpolate**

: Use interpolation when resampling.

**cpdf -rasterize in.pdf** \[\<range>] \[-gs \<path>]
\[-rasterize\[-gray | -1bpp | -jpeg | -jpeggray]] \[-rasterize-res \<n>]
\[-rasterize-jpeg-quality \<n>] \[-rasterize-no-antialias |
-rasterize-downsample] \[-rasterize-annots] \[-rasterize-alpha] **-o out.pdf** 

Rasterize each page, replacing the page content with the rasterized version
in the output PDF.

**-gs** \<path>

: Supply path to gs.

**-rasterize-gray**

: Use grayscale instead of colour.

**-rasterize-1bpp**

: Use monochrome instead of colour.

**-rasterize-jpeg**

: Use JPEG instead of lossless compression.

**-rasterize-jpeggray**

: Use grayscale JPEG instead of lossless compression.

**-rasterize-res** \<n>

: Set the resolution (default 144dpi).

**-rasterize-jpeg-quality** \<n>

: Set JPEG image quality (0..100).

**-rasterize-no-antialias**

: Turn off anti-aliasing.

**-rasterize-downsample**

: Use better but slower anti-aliasing.

**-rasterize-annots**

: Rasterize annotations instead of retaining.

**-rasterize-alpha**

: Produce an alpha channel (lossless only)

**cpdf -output-image in.pdf** \[\<range>] \[-gs \<path>]
\[-rasterize\[-gray | -1bpp | -jpeg | -jpeggray] \[-rasterize-res \<n>]
\[-rasterize-jpeg-quality \<n>] \[-rasterize-no-antialias |
-rasterize-downsample] \[-rasterize-annots] \[-rasterize-alpha] \[-tobox
\<boxname>] **-o \<format>** 

Output rasterized images for each page to the given folder and format e.g
"dir/%%%.png" would create dir/001.png and so on. Options as above and:

**-tobox** \<boxname>

: Specify the box to rasterize (default **/MediaBox**).

# 14. FONTS

**cpdf -list-fonts\[-json] in.pdf**

List the fonts on each page in a file together with their types. Either in
plain text (**-list-fonts**) or JSON (**-list-fonts-json**) format.

**cpdf -print-font-table \<font name>** **in.pdf** [-print-font-table-page \<n>]

Print a font table for a given font name and page, e.g:

```
cpdf -print-font-table /XYPLPB+NimbusSanL-Bold -print-font-table-page 2 cpdfmanual.pdf
67 = U+0043 (C - LATIN CAPITAL LETTER C) = /C
68 = U+0044 (D - LATIN CAPITAL LETTER D) = /D
70 = U+0046 (F - LATIN CAPITAL LETTER F) = /F
71 = U+0047 (G - LATIN CAPITAL LETTER G) = /G
...
```

**cpdf -copy-font fromfile.pdf** **in.pdf** \[\<range>] -copy-font-page \<n> -copy-font-name \<name> **-o out.pdf**

A low-level operation to copy a font from one file to another.

**-copy-font-page** \<n>

: The page from which to copy the font.

**-copy-font-name** \<n>

: The (PDF) name of the font. 

**cpdf -remove-fonts in.pdf -o out.pdf**

Remove fonts from a file.

**cpdf -missing-fonts in.pdf**

Report missing fonts - i.e those which are unembedded.

**cpdf -embed-missing-fonts -gs \<path to gs> in.pdf -o out.pdf**

Use gs to embed missing fonts into a PDF. Note: putting a PDF file through gs
in this manner may not be lossless: some metadata may not be preserved.

**cpdf -extract-font \<n>,\<pdf font name> in.pdf -o out.font**

Extract a font file from a PDF given a page number, font name pair e.g:

cpdf -extract-font 5,/F50 in.pdf -o out.ttf

# 15. PDF AND JSON

**cpdf in.pdf -output-json** \[-output-json-parse-content-streams]
\[-output-json-no-stream-data] \[-output-json-decompress-streams]
\[-output-json-clean-strings] \[-utf8] **-o out.json**

Convert PDF to a JSON format which may be inspected, or edited and
round-tripped back into PDF.

**-output-json-parse-content-streams**

: Parse the content streams into JSON too.

**-output-json-no-stream-data**

: Elide stream data. This makes a smaller file, but bars round-tripping.

**-output-json-decompress-streams**

: Keep the streams intact, and decompress them.

**-output-json-clean-strings**

: Deprecated in favour of **-utf8** below.

**-utf8**

: Use UTF8 rather than PDFDocEncoding for strings, to make them more easily editable.

**cpdf -j in.json -o out.pdf**

Convert a JSON file to a PDF file.

# 16. OPTIONAL CONTENT GROUPS

**cpdf -ocg-list in.pdf**

List the optional content groups in the PDF, one per line, to standard output.

**cpdf -ocg-rename in.pdf -ocg-rename-from \<a> -ocg-rename-to \<b> -o out.pdf**

Rename an optional content group given the old and new names.

**cpdf -ocg-order-all in.pdf -o out.pdf**

Ensure that every optional content group appears in the order list.

**cpdf -ocg-coalesce-on-name in.pdf -o out.pdf**

Coalesce optional content groups. For example, if we merge or stamp two files
both with an OCG called "Layer 1", we will have two different optional content
groups. This command will merge them into a single optional content group.

# 17. CREATING NEW PDFS

**cpdf -create-pdf** \[-create-pdf-pages \<n>] \[-create-pdf-papersize \<papersize>] **-o out.pdf**

Create a new PDF (default: one page, A4 portrait).

**-create-pdf-pages** \<n>

: Give the number of pages.

**-create-pdf-papersize** \<papersize>

: Give the paper size, e.g "a3landscape" or "200pt 600pt"

**cpdf -typeset \<filename>** \[-create-pdf-papersize \<papersize>] \[-font \<font>]
\[-font-size \<n>] \[-subformat \<subformat>] \[-title \<string>] **-o out.pdf**

Typeset a text file into a PDF.

**-create-pdf-papersize** \<papersize>

: Give the paper size.

**-font** \<font>

: The font (default Times-Roman)

**-font-size** \<n>

: The font size (default 12pt)

**-subformat** \<subformat>

: Specify PDF/UA-1 or PDF/UA-2 to make a conforming file.

**-title** \<string>

: Specify a title when using **-subformat** for a PDF/UA file.

**cpdf  -jpeg \<filename>** \[-subformat \<subformat>] \[-title \<string>] **-o
out.pdf**

**cpdf -png \<filename>** \[-subformat \<subformat>] \[-title \<string>] **-o out.pdf**

**cpdf -jpeg2000 \<filename>** \[-subformat \<subformat>] \[-title \<string>] **-o
out.pdf**

Build a PDF from an image file (JPEG, PNG or JPEG2000).

**-subformat** \<subformat>

: Specify PDF/UA-1 or PDF/UA-2 to make a conforming file.

**-title** \<string>

: Specify a title when using **-subformat** for a PDF/UA file.

**cpdf** \[-jbig2-global \<filename>] **-jbig2 \<filename>** \[-jbig2-global |
-jbig2-global-clear] \[-jbig2 \<filename>] ... \[-subformat \<subformat>]
\[-title \<string>] **-o out.pdf**

Make a multi-page PDF from one or more PDF-appropriate JBIG2 fragments
prepared by the jbig2enc program. For example, in lossless mode:

**cpdf -jbig2 1.jbig2 -jbig2 2.jbig2 -jbig2 3.jbig2 -o out.pdf**

In lossy mode, jbig2globals segments may be specified:

**cpdf -jbig2-global 0.jbig2globals -jbig2 1.jbig2 -jbig2 2.jbig2 -jbig2 3.jbig2 -o out.pdf**

**-jbig2-global** \<filename>

: Change the jbig2global segment in use.

**-jbig2-global-clear**

: Cease use of jbig2global segment, and return to lossless mode.

**-subformat** \<subformat>

: Specify PDF/UA-1 or PDF/UA-2 to make a conforming file.

**-title** \<string>

: Specify a title when using **-subformat** for a PDF/UA file.

# 18. DRAWING ON PDFS

**cpdf in.pdf** \[\<range>] [-draw-struct-tree] **-draw \<draw operations>** [-underneath] **-o out.pdf**

Draw on a PDF. See cpdfmanual.pdf for a tutorial, and full information.

**-draw-struct-tree**

: Produce structure information.

**-underneath**

: Draw underneath instead of on top of any existing page content.

**cpdf -text-width \<string>** \[-font \<font>] \[-fontsize \<n>]

Return the width of a string of text in a one of the standard 14 fonts. Used
for calculations of text position (e.g right-alignment).

**-font** \<font>

: Specify the font.

**-fontsize** \<n>

: Specify the font size.

**Building and showing paths**

**-rect** "x y w h"

: Draw rectangle.

**-to** "x y"

: Move to.

**-line** "x y"

: Add line to path.

**-bez** "x1 y1 x2 y2 x3 y3"

: Add Bezier curve to path.

**-bez23** "x2 y2 x3 y3"

: Add Bezier curve to path.

**-bez13** "x1 y1 x3 y3"

: Add Bezier curve to path.

**-circle** "x y r"

: Add circle to path.

**-stroke**

: Stroke path.

**-fill**

: Fill path.

**-filleo**

: Fill path, even odd.

**-strokefill**

: Stroke and fill path.

**-strokefilleo**

: Stroke and fill path, even odd.

**-close**

: Close path.

**Clipping with paths**

**-clip**

: Clip.

**-clipeo**

: Clip, even odd.

**Path parameters**

**-strokecol** "g" | "r g b" | "c y m k" | \<namedcolour>

: Set stroke colour.

**-fillcol** "g" | "r g b" | "c y m k" | \<namedcolour>

: Set fill colour.

**-thick** \<n>

: Set stroke thickness.

**-cap** \<butt | round | square>

: Set cap.

**-join** \<miter | round | bevel>

: Set join.

**-miter** \<n>

: Set miter limit.

**-dash** \<pattern>

: Set dash pattern.

**The graphics stack and matrices**

**-push**

: Push graphics stack.

**-pop**

: Pop graphics stack.

**-matrix** "a b c d e f"

: Append to graphics matrix.

**-mtrans** "tx ty"

: Translate the graphics matrix.

**-mrot** "x y a"

: Rotate the graphics matrix.

**-mscale** "x y sx sy"

: Scale the graphics matrix.

**-mshearx** "x y a"

: Shear the graphics matrix in X.

**-msheary** "x y a"

: Shear the graphics matrix in Y.

**Re-use with XObjects**

**-xobj-bbox** "x y w h"

: Specify the bounding box for xobjects.

**-xobj** \<name>

: Begin saving a sequence of graphics operators.

**-end-xobj**

: End saving a sequence of graphics operators.

**-use** \<name>

: Use a saved sequence of graphics operators.

**Images**

**-draw-jpeg** \<name>=\<filename>

: Load a JPEG from file and name it.

**-draw-png** \<name>=\<filename>

: Load a PNG from file and name it.

**-draw-jpeg2000** \<name>=\<filename>

: Load a JPEG2000 from file and name it.

**-image** \<name>

: Draw an image which has already been loaded.

**Transparency**

**-fill-opacity** \<n>

: Set opacity.

**-stroke-opacity** \<n>

: Set stroke opacity.

**Text**

**-bt**

: Begin text.

**-et**

: End text.

**-text** \<string>

: Draw text.

**-stext** \<string>

: Draw text with %specials.

**-para** \<paragraph>

: Typeset a paragraph.

**-paras** \<paragraph>

: Typeset multiple paragraphs.

**-indent** \<n>

: Set paragraph indent.

**-leading** \<n>

: Set leading.

**-charspace** \<n>

: Set character spacing.

**-wordspace** \<n>

: Set word space.

**-textscale** \<n>

: Set text scale.

**-rendermode** \<n>

: Set text rendering mode.

**-rise** \<n>

: Set text rise.

**-nl**

: New line.

**The Next Page**

**-newpage**

: Move to a fresh page.

**Structure Information**

**-draw-struct-tree**

: Add structure information. Must precede **-draw** on command line.

**-tag** \<name>

: Begin marked content.

**-end-tag**

: End marked content.

**-stag** \<name>

: Begin structure tree branch.

**-end-stag**

: End structure tree branch.

**-auto-tags**

: Automatically tag paragraphs and images.

**-no-auto-tags**

: Refrain from automatically tagging paragraphs and images.

**-artifact**

: Begin manual artifact.

**-end-artifact**

: End manual artifact.

**-no-auto-artifacts**

: Prevent automatic addition of artifacts during post-processing.

**-namespace** \<namespace>

: Set the namespace for future branches of the tree.

**-eltinfo** \<elt=string>

: Set element information.

**-end-eltinfo**

: Erase element information.

**-rolemap** \<rolemap>

: Set role map.

# 19. ACCESSIBLE PDFS WITH PDF/UA

**cpdf -print-struct-tree in.pdf**

Print a PDF's structure tree to standard output for inspection.

**cpdf -extract-struct-tree in.pdf -o out.json**

Extract the whole structure tree in JSON format. See cpdfmanual.pdf for details.

**cpdf -replace-struct-tree in.json in.pdf -o out.pdf**

Replace a PDF's structure tree with one from a JSON file.

**cpdf -remove-struct-tree in.pdf -o out.pdf**

Remove the structure tree from a file.

**cpdf -mark-as-artifact in.pdf -o out.pdf**

Mark all the content of the file as being an artifact.

**cpdf -verify "PDF/UA-1(matterhorn)"** **in.pdf** \[-verify-single \<test>] \[-json]

Verify conformance with PDF/UA-1.

**-json**

: Output conformance failures in JSON rather than plain text format.

**-verify-single** \<test>

: Verify just one test. See cpdfmanual.pdf for list of tests.

**cpdf -mark-as \["PDF/UA-1" | "PDF/UA-2"] in.pdf -o out.pdf**

Mark a PDF as conforming to PDF/UA-1 or PDF/UA-2

**cpdf -remove-mark \["PDF/UA-1" | "PDF/UA-2"] in.pdf -o out.pdf**

Remove conformance marker from a PDF.

**cpdf -create-pdf-ua-\<1 | 2> \<string>** \[-create-pdf-pages \<n>] \[-create-pdf-papersize \<papersize>] **-o out.pdf**

Create a new, blank, PDF/UA-1 or PDF/UA-2 file with the given title.

**-create-pdf-pages** \<n>

: Give the number of pages.

**-create-pdf-papersize** \<papersize>

: Give the paper size, e.g "a3landscape" or "200pt 600pt"

# 20. MISCELLANEOUS

**cpdf -draft** **in.pdf** \[\<range>] \[-boxes] \[-draft-remove-only \<n>] **-o out.pdf**

Remove bit-mapped (photographic) images from a PDF.

**-boxes**

: Replace each image with a crossed box to show where it was.

**-draft-remove-only**

: Remove only one image, with the given name. The name can be obtained from,
for example, **-list-images-used**.

**cpdf -remove-all-text in.pdf** \[\<range>] **-o out.pdf**

Remove all text from a PDF.

**cpdf -blacktext in.pdf** \[\<range>] **-o out.pdf**

Make all text black.

**cpdf -blacklines in.pdf** \[\<range>] **-o out.pdf**

Make all lines black.

**cpdf -blackfills in.pdf** \[\<range>] **-o out.pdf**

Make all fills black.

**cpdf -thinlines \<min thickness> in.pdf** \[\<range>] **-o out.pdf**

Make each line a minimum thickness. A negative value will instead thin lines
to the absolute of the value given. By processing twice, with a positive then
negative figure, it is therefore possible to clamp thicknesses to a range or
single value.

**cpdf -clean in.pdf -o out.pdf**

Deprecated. This work is now done by default upon writing any file.

**cpdf -set-version \<version number> in.pdf -o out.pdf**

Set the version of a PDF. E.g for version 1.4, use **-set-version 4**. For PDF
versions starting with 2, add ten to the number. E.g for PDF 2.0, use
**-set-version 10**.

**cpdf -copy-id-from source.pdf in.pdf -o out.pdf**

Copy the ID from one PDF to another.

**cpdf -remove-id in.pdf -o out.pdf**

Remove the ID from a PDF.

**cpdf -list-spot-colors in.pdf**

List the names of any separation colour spaces in a PDF.

**cpdf -print-dict-entry** **\<key> in.pdf** [-json]

**cpdf -remove-dict-entry \<key>** **in.pdf** [-dict-entry-search \<term>] **-o out.pdf**

**cpdf -replace-dict-entry \<key> -replace-dict-entry-value \<value>** **in.pdf** \[-dict-entry-search \<term>] **-o out.pdf**

Low level operations to edit PDF objects directly. See cpdfmanual.pdf for details.

**cpdf -remove-clipping in.pdf** \[\<range>] **-o out.pdf**

Remove any clipping paths from a PDF.

**cpdf -obj[-json] \<object specification> in.pdf**

**cpdf -replace-obj \<object specification>=\<object> in.pdf -o out.pdf**

**cpdf -remove-obj \<object number> in.pdf -o out.pdf**

**cpdf -extract-stream\[-decompress] \<object specification> in.pdf -o out.dat**

**cpdf -replace-stream \<object specification> -replace-stream-with \<filename> in.pdf -o out.pdf**

Low level operations to explore and edit PDF files object-by-object. See cpdfmanual.pdf for details.

**cpdf -contains-javascript in.pdf**

Prints true if the PDF contains JavaScript, false otherwise.

**cpdf -remove-javascript in.pdf -o out.pdf**

Remove JavaScript from a PDF.

# ENVIRONMENT

**CPDF_SHOW_EXT**

: Show command line of external commands as they are called if equal to "true".

**CPDF_DEBUG**

: Same as adding **-debug** to command line, if equal to "true".

**CPDF_REPRODUCIBLE_DATES**

: Expand data "now" to the same, false value each time, if equal to "true".

**CAMLPDF_REPRODUCIBLE_IDS**

: Produce the same file ID each time, if equal to "true".

# BUGS

See https://github.com/johnwhitington/cpdf-source/issues or email
contact@coherentgraphics.co.uk
