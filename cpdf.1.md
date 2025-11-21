% CPDF(1)
% John Whitington
% November 2025

# NAME

cpdf - PDF command line tools

# SYNOPSIS

Simple operation:

**cpdf** in.pdf \[operation] \[options] \[-o out.pdf]

Operation on password-protected file:

**cpdf** in.pdf \[user=\<password>] \[owner=\<password>] ....

Multiple operations, one after another:

**cpdf** in.pdf \[operation] \[options] **AND** \[operation] \[options] **AND**
... \[-o out.pdf]

# DESCRIPTION

**cpdf** is an AGPL-licensed command line tool for processing PDF files. The
rest of this man page gives a brief description of each command line operation
and option. The file cpdfmanual.pdf which you should find installed on your
system or otherwise at https://www.coherentpdf.com/cpdfmanual.pdf gives the
full usage details.

The sections in this man page follow the chapters of cpdfmanual.pdf.

# 1. BASIC USAGE

**-version**

: Print the Cpdf version number

**-help, --help**

: Gives links to sources of help

**-summary**

: Lists and describes very briefly each command line option

**-o**

: The output filename. Beware of writing back to the same file.

**-i**

: Cpdf automatically treats any filename ending with .pdf (any case) as an
input PDF. If your file does not end with .pdf, you can supply the file with
**-i** instead.

**-range**

: Gives the range of pages to be affected by a the operation. By default, all
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

: Presume ISO-compliant content streams when processing page contents. This is
faster, because it does not involve re-parsing whole streams to add content.

**-idir <directory>**

: Add a whole directory of PDFs as inputs.

**-idir-only-pdfs**

: Restrict **-idir** to only files ending in .pdf (any case).

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

**-stdin-user <password>**

: Supply the user password for the PDF which is from standard input. 

**-stdin-owner <password>**

: Supply the owner password the the PDF which is from standard input.

**-producer <text>**

: Set the producer of the output file.

**-creator <text>**

: Set the creator of the output file.

**-change-id**

: Change the PDF's ID field when writing the output file.

**-l**

: Linearize the PDF when writing. Requires a linearizer to be supplied with
**-cpdflin**.

**-cpdflin <filename>**

: Give the path of an external linearizer. For example, Qpdf.

**-keep-l**

: Keep the linearization status (either linearized or not) of the input file
upon output. Requires a linearizer to be supplied with **-cpdflin**.

**-no-preserve-objstm**

: Do not preseve existing object streams when writing the output file. Note
that **-create-objstm** and **-no-preserve-objstm** may be used together - the
effect is then to redo all object streams.

**-create-objstm**

: Create new object streams when writing the output file. Note that
**-create-objstm** and **-no-preserve-objstm** may be used together - the
effect is then to redo all object streams.

**-args <filename>**

: Read command line arguments from the given file by direct textual
substitution into the command line, prior to any other processing. 

**-args-json <filename>**

: Read command line arguments from a JSON file consisting of a single array of
strings.

**-utf8**

: Read and write string data as UTF8. Almost always the sensible option, and
will become the default in a future version. See section 1.17 of cpdfmanual.pdf.

**-stripped**

: Convert string output to 7 bit ASCII by dropping any high characters. To be
used with caution. See section 1.17 of cpdfmanual.pdf.

**-raw**

: Perform no processing on string outputs. See section 1.17 of cpdfmanual.pdf.

**-gs**

: A very few of Cpdf's functions rely upon the **gs** command. Its path may be
supplied with **-gs**.

**-gs-malformed**

: This option is used to allow Cpdf to call out the **gs** command to
pre-process badly malformed files as a last resort. See section 1.13 of
cpdfmanual.pdf for details.

**-gs-malformed-force**

: This option is used to allow Cpdf to call out to the **gs** command to
pre-process badly malformed files as a last resort. See section 1.13 of
cpdfmanual.pdf for details.

**-gs-quiet**

: Do not show the output of **gs** when used.

**-error-on-malformed**

: Do not attemp to reconstruct malformed files by any method, but exit with an
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

**-collate-n**

: Like collate, but in chunks of more than one page.

**-retain-numbering**

: Keep the page numbering of each input file intact, rather than renumbering
the pages in the output document beginning at 1.

**-merge-add-bookmarks \[-merge-add-bookmarks-use-titles]**

: Add a top-level bookmark for each file, using the filename.
**-merge-add-bookmarks-use-titles**, when used in conjunction with
**-merge-add-bookmarks** uses the titles from document metadata instead.

**-remove-duplicate-fonts**

: Ensures that fonts used in more than one input appear only once in the output.

**-process-struct-trees**

: Merge input structure trees in the output.

**-subformat**

: If **-subformat** "PDF/UA-2" is given, together with
**-process-struct-trees** Cpdf will add a top-level Document structure tree
element.

## Portfolios

A PDF portfolio is a special kind of PDF with contains other documents (PDF and
otherwise) within it. Support is mostly limited to Adobe products at time of
writing.

cpdf -portfolio in.pdf -pf \<filename> \[-pfd <string>] \[-pfr \<relationship>]
\[-pf ...] -o out.pdf

The input in.pdf here is the main file. You can build a blank one with **-create-pdf**.

**-pf**

: The filename for each file to include in the portfolio.

**-pfd**

: The description for the file (must appear after **-pf**).

**-pfr**

: The so-called relationship for the file (must appear after **-pf**).

## Splitting

We can split an input PDF into its constituent pages, and output one PDF for
each page.

cpdf -split in.pdf \[-chunk \<chunksize>] \[-process-struct-trees] \[-utf8] -o
\<format>

The output format has many options (see cpdfmanual.pdf for details). But the
simplest is just to number the outputs in sequence. For example cpdf -split
in.pdf -o out%%%.pdf will produce out001.pdf, out002.pdf and so on.

**-chunk**

: Choose a chunk size other than 1.

**-process-struct-trees**

: Split the input document's structure tree into the output documents.

**-utf8**

: This option may be required in the case of some output formats. See
cpdfmanual.pdf for details.

## Splitting on bookmarks

We can split on bookmark boundaries, at a given level, instead of splitting on each page:

cpdf -split-bookmarks \<level> in.pdf \[-process-struct-trees] \[-utf8] -o \<format>

**-process-struct-trees**

: Split the input document's structure tree into the output documents.

**-utf8**

: This option may be required in the case of some output formats. See
cpdfmanual.pdf for details.

## Splitting to a given size

We can split the file, if possible, to a maximum filesize for each output PDF:

cpdf -split-max \<file size> in.pdf \[-process-struct-trees] \[-utf8] -o \<format>

**-process-struct-trees**

: Split the input document's structure tree into the output documents.

**-utf8**

: This option may be required in the case of some output formats. See
cpdfmanual.pdf for details.

## Interleaved splitting

We can use **-spray** to write the split pages to more than one named output
file. When Cpdf runs out of output files, it adds the next page to the first
output file, and so on until all input pages are exhausted.

cpdf -spray in.pdf \[-process-struct-trees] \[-utf8] -o a.pdf \[-o b.pdf \[-o ...]]

**-process-struct-trees**

: Split the input document's structure tree into the output documents.

**-utf8**

: This option may be required in the case of some output formats. See
cpdfmanual.pdf for details.

# 3. PAGES

cpdf -scale-page "\<scale x> \<scale y>" \[-fast] \[<position>] in.pdf
\[\<range>] -o out.pdf

: Scale pages in the given range by the given factor e.g "2 2". See the end of
this chapter for -fast and -position.

cpdf -scale-to-fit "\<x size> \<y size>" \[-fast] \[-prerotate] \[\<position>]
\[-scale-to-fit-scale \<scale>] \[-scale-to-fit-rotate-clockwise]
\[-scale-to-fit-rotate-anticlockwise] in.pdf \[\<range>] -o out.pdf

: Scale pages in the given range to fit the given size e.g "a4paper" or "10in
7in", without altering the aspect ratio. By default the content will be
centered on the new page. See the end of this chapter for -position
and -prerotate.

**-scale-to-fit-scale**

: Scale to a proportion of the available area, instead of filling it. For
example 0.9 for 90 percent.

**-scale-to-fit-rotate-clockwise -scale-to-fit-rotate-anticlockwise**

: Automatically rotate page to maximise use of area.

cpdf -stretch "\<x size> \<y size>" \[-fast] in.pdf \[\<range>] -o out.pdf

: Scale pages without regard to aspect ratio.

cpdf -center-to-fit "\<x size> \<y size>" \[-fast] in.pdf \[\<range>] -o
out.pdf

: Center each page on a new page size, without scaling it.

cpdf -scale-contents \[\<scale>] \[<position>] \[-fast] in.pdf \[\<range>] -o
out.pdf

: Scale the content of pages by a given factor, without changing the size
of the page. See the end of this chapter for -position.

cpdf -shift "\<shift x> \<shift y>" \[-fast] in.pdf \[\<range>] -o out.pdf

: Shift the content of pages by a given displacement.

cpdf -shift-boxes "\<shift x> \<shift y>" in.pdf \[\<range>] -o out.pdf

: Shift the boxes of a page by a given displacement, without moving the content.

cpdf -rotate \<angle> in.pdf \[\<range>] -o out.pdf

: Change the PDF viewing rotation of pages to 0, 90, 180 or 270 degrees clockwise. 

cpdf -rotateby \<angle> in.pdf \[\<range>] -o out.pdf

: Change the PDF viewing rotation of pages by 0, 90, 180 or 270 degrees clockwise. 

cpdf -upright \[-fast] in.pdf \[\<range>] -o out.pdf

: The -upright operation does whatever combination of -rotate and
-rotate-contents is required to change the rotation of the document to zero
without altering its appearance. In addition, it makes sure the media box has
its origin at (0,0), changing other boxes to compensate.

cpdf -rotate-contents \<angle> \[-fast] in.pdf \[\<range>] -o out.pdf

: Rotates the contents of the page around its center point by the given angle.

cpdf -hflip \[-fast] in.pdf \[\<range>] -o out.pdf

: Flip page content horizontally.

cpdf -vflip \[-fast] in.pdf \[\<range>] -o out.pdf

: Flip page content vertically.

cpdf -\[media|crop|art|trim|bleed]box \<boxspec> in.pdf \[\<range>] -o out.pdf

: Set the media, crop, art, trim or bleed box. For example -cropbox "50 50 300
200" sets minx 50, miny 50, width 300, height 200. To use absolute numbers
instead of width and height, prefix with a question mark, writing "?50 50 350
250" instead.

cpdf -remove-\[crop|art|trim|bleed]box in.pdf \[\<range>] -o out.pdf

: Remove a crop, art, trim or bleed box from pages.

cpdf -frombox \<boxname> -tobox \<boxname> \[-mediabox-if-missing] in.pdf \[\<range>] -o out.pdf

: Copy a box to another. For example from /TrimBox to /CropBox.

**-mediabox-if-missing**

: Use media box, rather than failing, if the -frombox is missing.

cpdf -hard-box \<boxname> \[-fast] in.pdf \[\<range>] \[-mediabox-if-missing] -o out.pdf

: Create a hard box for a given box name - that is to say, one which clips its contents.

cpdf -show-boxes \[-fast] in.pdf \[\<range>] -o out.pdf

: Show the media, crop, art, trim, and bleed boxes in Red, Green, Blue, Orange
and Pink respectively.

cpdf -trim-marks \[-fast] in.pdf \[\<range>] -o out.pdf

: Add trim marks to a PDF. The trim box must be present.

FIXME: POSITIONS
FIXME: FAST

# 4. ENCRYPTION AND DECRYPTION

cpdf -encrypt \<method> \[-pw=]\<owner> \[-pw=]\<user> \[-no-encrypt-metadata]
\<permissions> in.pdf -o out.pdf

: Encrypt a document given the method (e.g AES256ISO for modern usage), owner
and user passwords, and optionally permissions. E.g cpdf -encrypt AES256ISO
secret "" in.pdf -o out.pdf

**-no-encrypt-metadata**

: Do not encrypt metadata (AES encryption only)

**-pw=**

: Useful if a password may begin with a dash.

Permissions:

**-no-edit**

: Cannot change the document

**-no-print**

: Cannot print the document

**-no-copy**

: Cannot select or copy text or graphics

**-no-annot**

: Cannot add or change form fields or annotations

**-no-forms**

: Cannot edit form fields

**-no-extract**

: Cannot extract text or graphics

**-no-assemble**

: Cannot merge files etc.

**-no-hq-print**

: Cannot print high-quality

cpdf -decrypt \[-decrypt-force] in.pdf owner=\<owner password> -o out.pdf

Note: Adobe Acrobat and Adobe Reader may show slightly different permissions in info dialogues – this
is a result of policy changes and not a bug in Cpdf. You may need to experiment.

: Decrypt a document, given its owner password.

**-decrypt-force**

: Decrypt even without the owner password, using just the user password. The user password is often blank.

# 5. COMPRESSION

cpdf -decompress \[-just-content] in.pdf -o out.pdf

: Decompress the streams in a PDF file, for instance to manually inspect it.

**-just-content**

: Only decompress page content streams

-jbig2dec

: Give the path to the jbig2dec program which Cpdf uses, if available, to decompress JBIG2 streams.

cpdf -compress in.pdf -o out.pdf

: Compress any streams which are uncompressed using the FlateDecode method, with the exception of metadata streams.

cpdf -squeeze in.pdf \[-squeeze-log-to \<filename>] \[-squeeze-no-pagedata] -o out.pdf

: Squeeze a file by coalescing like objects, and various other maneuvers.

**-squeeze-log-to**

: Write the squeeze log to file rather than standard output.

**-squeeze-no-pagedata**

: Avoid processing page data, making the squeeze process much faster at the cost of a little compression.

cpdf -remove-article-threads in.pdf -o out.pdf

: Remove article threads.

cpdf -remove-page-piece in.pdf -o out.pdf

: Remove page piece information.

cpdf -remove-web-capture in.pdf -o out.pdf

: Remove web capture data.

cpdf -remove-procsets in.pdf -o out.pdf

: Remove ProcSets, a now-irrelevant data structure from early PDFs.

cpdf -remove-output-intents in.pdf -o out.pdf

: Remove output intents, a colour-matching system for documents intended to be printed.

# 6. BOOKMARKS

cpdf -list-bookmarks \[-utf8] in.pdf

: Print bookmark data to Standard Output. The data includes level, title, pagenumber linked to, and full link data. Use -utf8 always. For example:

0 "Part 1" 1 open
1 "Part 1A" 2 "[2 /XYZ 200 400 null]"
1 "Part 1B" 3
0 "Part 2" 4
1 "Part 2a" 5

cpdf -list-bookmarks-json \[-preserve-actions] in.pdf

: Print bookmark data to Standard Output in JSON format instead. Here is a single entry in the JSON array of bookmarks:

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

cpdf -remove-bookmarks in.pdf -o out.pdf

: Remove all bookmarks from a PDF.

cpdf -add-bookmarks \<bookmark file> in.pdf -o out.pdf

: Add bookmarks, given an old-style bookmark file.

cpdf -add-bookmarks-json \<bookmark file> in.pdf -o out.pdf

: Add bookmarks, given a new-style JSON bookmark file.

cpdf -bookmarks-open-to-level \<n> in.pdf -o out.pdf

: Set all bookmarks up to and including a given level to be open.

cpdf -table-of-contents \[-toc-title] \[-toc-no-bookmark] \[-toc-dot-leaders] \[-font <font>] \[-font-size <size>] \[-embed-std14 /path/to/fonts] \[-process-struct-trees] \[-subformat <subformat>] in.pdf -o out.pdf

: Generate a typeset table of contents from existing bookmarks, adding it to the beginning of the document.

**-toc-title**

: Title (default is "Table of Contents")

**-toc-no-bookmark**

: Do not add an entry for the new table of contents in the document's bookmarks.

**-toc-dot-leaders**

: Add dot leaders.

**-font**

: Give the font (default Times Roman)

**-font-size**

: Give the font size (default 12pt)

**-embed-std14**

: Embed Standard 14 fonts given their path (see cpdfmanual.pdf for details).

**-process-struct-trees**

: Create a structure tree for the new table of contents and merge it with the document's.

**-subformat**

: Add -subformat "PDF/UA-2" when adding a table of contents to a PDF/UA-2 file to keep compatibility.

# 7. PRESENTATIONS

cpdf -presentation in.pdf \[\<range>] -o out.pdf \[-trans \<transition-name>] \[-duration \<float>] \[-vertical] \[-outward] \[-direction \<int>] \[-effect-duration \<float>]

: Make a slide-show presentation from a PDF.

**-trans**

: The transition style, one of Split, Blinds, Box, Wipe, Dissolve, Glitter.

**-duration**

: Time in seconds before presentation advances (default: no automatic advancement).

**-vertical**

: Select vertical blinds for transition type Blinds.

**-outward**

: Select outward sweep for transition type Box.

**-direction**

: Direction for Wipe and Glitter styles. See cpdfmanual.pdf for full information.

**-effect-duration**

: Length in seconds of the transition itself.

# 8. TEXT AND STAMPS

cpdf [-stamp-on | -stamp-under] source.pdf [-scale-stamp-to-fit] [<positioning command>] [-relative-to-cropbox] [-process-struct-trees] in.pdf [<range>] [-fast] -o out.pdf

: a

**-scale-stamp-to-fit**

: Scale the stamp to fit the page before applying it.

**-relative-to-cropbox**

: Take the positioning command relative to the crop box rather than the media box.

**-process-struct-trees**

: Maintain tagged PDF. The main file will keep its structure; the stamp will be marked as an artifact.

See below for positioning commands.

cpdf -combine-pages over.pdf under.pdf [-fast] [-prerotate] [-no-warn-rotate] [-process-struct-trees] [-underneath] [-stamp-scale-to-fit] -o out.pdf

: a

**-prerotate**

: Remove any rotation differences between the files before combining.

**-no-warn-rotate**

: Do not warn of unresolved rotation differences.

**-underneath**

: Reverse the order of "over" and "under" files.

**-process-struct-trees**

: Maintain tagged PDF. The "under" file will keep its structure; the "over" file will be marked as an artifact.

cpdf (\[-add-text \<text-format> | -add-rectangle \<size>]) [-font \<fontname>] [-font-size \<size-in-points>] [-load-ttf \<name>=\<file>] [-embed-std14] [-color \<color>] [-line-spacing \<number>] [-outline] [-linewidth \<number>] [-underneath] [-relative-to-cropbox] [-prerotate] [-no-warn-rotate] [-bates \<number>] [-bates-at-range \<number>] [-bates-pad-to \<number>] [-opacity \<number>] [-midline] [-topline] [-fast] [-process-struct-trees] in.pdf \[\<range>] -o out.pdf
 
: Add text to a PDF. Various special codes for page numbers or time and date may be used. For example:

%Page Page number in arabic notation (1, 2, 3. . . )
%PageDiv2 Page number in arabic notation divided by two
%roman Page number in lower-case roman notation (i, ii, iii. . . )
%Roman Page number in upper-case roman notation (I, II, III. . . )
%EndPage Last page of document in arabic notation
%Label The page label of the page
%EndLabel The page label of the last page
%filename The full file name of the input document
%URL[text|URL] Add text, which links to URL (does not work for diagonal text)
%Bookmark<n> Bookmark text at level n (0, 1, 2, 3, 4)
%Bates bates number

And date and time formats:

%a Abbreviated weekday name (Sun, Mon etc.)
%A Full weekday name (Sunday, Monday etc.)
%b Abbreviated month name (Jan, Feb etc.)
%B Full month name (January, February etc.)
%d Day of themonth (01-31)
%e Day of themonth (1-31)
%H Hour in 24-hour clock (00-23)
%I Hour in 12-hour clock (01-12)
%j Day of the year (001-366)
%m Month of the year (01-12)
%M Minute of the hour (00-59)
%p "a.m" or "p.m"
%S Second of theminute (00-61)
%T Same as %H:%M:%S %u Weekday (1-7, 1 = Sunday)
%w Weekday (0-6, 0 = Sunday)
%Y Year (0000-9999)
%% The % character

\n may be used to demarcate multiple lines.

**-font**

: Give the font (default Times Roman). Options are:

Times-Roman Times-Bold Times-Italic Times-BoldItalic Helvetica Helvetica-Bold Helvetica-Oblique Helvetica-BoldOblique Courier Courier-Bold Courier-Oblique Courier-BoldOblique

**-font-size**

: Give the font size (default 12pt)

**-load-ttf**

: Load a truetype font, and give it name which may be used with -font. For example -load-ttd A=NotoSans-Black.ttf

**-embed-std14**

: Embed the standard 14 fonts given the path to the URW Base35 free fonts.

**-color**

: Choose the text colour using one (Grey), three (RGB), or four (CMYK) numbers from 0-1. E.g "0.5 0.4 0.5".

**-line-spacing**

: Set the spacing for multi-line text (default 1).

**-outline**

: Use outline text.

**-linewidth**

: Line width for outline text.

**-underneath**

: Put the text underneath the page instead of on top of the page.

**-relative-to-cropbox**

: Positions are relative to the crop box, rather than the media box.

**-prerotate**

: Remove any viewing rotation before adding text.

**-no-warn-rotate**

: Do not warn of unresolved viewing rotation.

**-bates**

: Set the bates number for use with %Bates

**-bates-at-range**

: Set the bates number for the first page in the range.

**-bates-pad-to**

: Pad bates numbers to a given number of leading zeros.

**-opacity**

: Set text opacity. Wholly opaque is 1, wholly transparent is 0.

**-midline**

: Position is relative to the midline of text rather than the baseline.

**-topline**

: Position is relative to the topline of text rather than the baseline.

**-process-struct-trees**

: Maintain tagged PDF, for example with PDF/UA. The main file will keep its structure; the stamped text will be marked as an artifact.

Positioning commands:

**-pos-left**

: Position the left of the baseline of the text at the given coordinates e.g "100 200".

**-pos-center**

: Position the center of the baseline of the text at the given coordinates e.g "100 200".

**-pos-right**

: Position the right of the baseline of the text at the given coordinates e.g "100 200".

**-top 10**

: Position the baseline of the text 10 pts from the top middle of the page.

**-topleft 10**

: Position the left of the baseline of the text 10 pts below and right of the top left of the page.

**-topleft "10 20"**

: Position the left of the baseline of the text 20 pts below and 10 pts right of the top left of the page.

**-topright 10**

: Position the right of the baseline of the text 10 pts below and left of the top right of the page.

**-topright "10 20"**

: Position the right of the baseline of the text 20 pts below and 10 pts left of the top right of the page.

**-left 10**

: Position the left of the baseline of the text 10 pts right of the left middle of the page.

**-bottomleft 10**

: Position the left of the baseline of the text 10pts up and right of the bottom left of the page.

**-bottomleft "10 20"**

: Position the left of the baseline of the text 20pts up and 10pts right of the bottom left of the page.

**-bottom 10**

: Position the center of the baseline of the text 10pts up from the bottom middle of the page.

**-bottomright 10**

: Position the right of the baseline of the text 10pts up and left from the bottom right of the page.

**-bottomright "10 20"**

: Position the right of the baseline of the text 20pts up and 10pts left from the bottom right of the page.

**-right 10**

: Position the right of the baseline of the text 10pts left of the center right of the page.

**-diagonal**

: Position text diagonally, bottom left to top right.

**-reverse-diagonal**

: Position text diagonally, top left to bottom right.

**-center**

: Position text centered on the page.

**-justify-left**

: Set left justifcation for multi-line text. Default depends upon position.

**-justify-right**

: Set right justification for multi-line text. Default depends upon position.

**-justify-center**

: Set center justification for multi-line text. Default depends upon position.

cpdf -remove-text in.pdf \[\<range>] -o out.pdf

: Remove text previously added by Cpdf.

cpdf -prepend-content \<content> in.pdf \[\<range>] -o out.pdf

: A low-level operation to prepend raw content to the beginning of page streams.

cpdf -postpend-content \<content> in.pdf \[\<range>] -o out.pdf

: A low-level operation to postpend raw content to the end of page streams.

cpdf -stamp-as-xobject stamp.pdf in.pdf \[\<range>] -o out.pdf

: A low-level operation to add stamp.pdf as a Form XObject in the given pages
of a PDF and write to Standard Output its name. 

# 9. MULTIPAGE FACILITIES

cpdf [-pad-before | -pad-after] in.pdf \[\<range>] \[-pad-with pad.pdf] -o out.pdf

: a

**-pad-with**

cpdf -pad-every \[\<integer>] in.pdf \[-pad-with pad.pdf] -o out.pdf

: a

**-pad-with**

cpdf [-pad-multiple | -pad-multiple-before] \[\<integer>] in.pdf -o out.pdf

: a

cpdf -redact \[-process-struct-trees] in.pdf \[\<range>] -o out.pdf

: a

**-process-struct-trees**

: a

cpdf \[-impose \<pagesize> | impose-xy "\<x> \<y>"] \[-impose-columns] \[-impose-rtl] \[-impose-btt] \[-impose-margin <margin>] \[-impose-spacing \<spacing>] \[-impose-linewidth \<width>] \[-fast] \[-process-struct-trees] in.pdf -o out.pdf

: a

**-impose-columns**

: a

**-impose-rtl**

**-impose-btt**

**-impose-margin**

**-impose-spacing**

**-impose-linewidth**

**-process-struct-trees**

cpdf -twoup-stack \[-fast] \[-process-struct-trees] in.pdf -o out.pdf

: a

**-process-struct-trees**

: a

cpdf -twoup \[-fast] \[-process-struct-trees] in.pdf -o out.pdf

: a

**-process-struct-trees**

: a

cpdf -chop "\<x> \<y>" \[-chop-columns] \[-chop-rtl] \[-chop-btt] in.pdf \[\<range>] -o out.pdf

: a

**-chop-columns**

: a

**-chop-rtl**

: a

**-chop-btt**

: a

cpdf \[-chop-h \<y> | -chop-v \<x>] \[-chop-columns] in.pdf \[\<range>] -o out.pdf

**-chop-columns**

: a

# 10. ANNOTATIONS

cpdf -list-annotations in.pdf \[\<range>]

: a

cpdf -list-annotations-json in.pdf \[\<range>]

: a

cpdf -set-annotations \<filename> \[-underneath] in.pdf \[\<range>] -o out.pdf

**-underneath**

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
cpdf -set-author \<author of document> \[-also-set-xmp] \[-just-set-xmp] in.pdf -o out.pdf
cpdf -set-subject \<subject of document> \[-also-set-xmp] \[-just-set-xmp] in.pdf -o out.pdf
cpdf -set-keywords \<keywords of document> \[-also-set-xmp] \[-just-set-xmp] in.pdf -o out.pdf
cpdf -set-creator \<creator of document> \[-also-set-xmp] \[-just-set-xmp] in.pdf -o out.pdf
cpdf -set-producer \<producer of document> \[-also-set-xmp] \[-just-set-xmp] in.pdf -o out.pdf

: a

cpdf -set-create \<date> \[-also-set-xmp] \[-just-set-xmp] in.pdf -o out.pdf
cpdf -set-modify \<date> \[-also-set-xmp] \[-just-set-xmp] in.pdf -o out.pdf

: a

cpdf -set-trapped in.pdf -o out.pdf \[-also-set-xmp] \[-just-set-xmp] 
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

**-label-style**

: a

**-label-prefix**

: a

**-label-startval**

: a

**-labels-progress**

cpdf -remove-page-labels in.pdf -o out.pdf

: a

cpdf -print-page-labels\[-json] in.pdf

: a

cpdf -composition\[-json] in.pdf

: a

# 12. FILE ATTACHMENTS

cpdf -attach-file \<filename> \[-to-page \<page number>] \[-afd \<string>] \[-afr \<relationship>] \[-attach-file ...] in.pdf -o out.pdf

: a

**-to-page**

: a

**-afd**

: a

**-afr**

: a

cpdf -list-attached-files \[-json] \[-include-data] in.pdf

: a

**-json**

: a

**-include-data**

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
