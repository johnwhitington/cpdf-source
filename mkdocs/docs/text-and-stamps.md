# Text and Stamps

    cpdf -stamp-on source.pdf  
         [-scale-stamp-to-fit] [<positioning command>]  
         [-relative-to-[crop|trim|art|bleed]box]  
         [-process-struct-trees] in.pdf [<range>] [-fast] -o out.pdf

    cpdf -stamp-under source.pdf  
         [-scale-stamp-to-fit] [<positioning command>] 
         [-relative-to-[crop|trim|art|bleed]box]  
         [-process-struct-trees] in.pdf [<range>] [-fast] -o out.pdf

    cpdf -combine-pages over.pdf under.pdf  
         [-fast] [-prerotate] [-no-warn-rotate] [-process-struct-trees]  
         [-underneath] [-stamp-scale-to-fit] -o out.pdf

    cpdf ([-add-text <text-format> | -add-rectangle <size>])  
          [-font <fontname>]          [-font-size <size-in-points>]  
          [-load-ttf <name>=<file>]   [-embed-std14]  
          [-color <color>]            [-line-spacing <number>]  
          [-outline]                  [-linewidth <number>]  
          [-underneath]               [-relative-to-[crop|trim|art|bleed]box]  
          [-prerotate]                [-no-warn-rotate]  
          [-bates <number>]           [-bates-at-range <number>]  
          [-bates-pad-to <number>]    [-opacity <number>]  
          [-midline]                  [-topline]  
          [-fast]                     [-process-struct-trees]  
          [-url-border]               [-text-90 | -text-180 | -text-270]  
          in.pdf [<range>] -o out.pdf

    See also positioning commands below.

    cpdf -remove-text in.pdf [<range>] -o out.pdf

    cpdf -prepend-content <content> in.pdf [<range>] -o out.pdf

    cpdf -postpend-content <content> in.pdf [<range>] -o out.pdf

    cpdf -stamp-as-xobject stamp.pdf in.pdf [<range>] -o out.pdf

    NB: See discussion of `-fast` in under Malformed Files in Basic Usage.

## Add a Watermark or Logo

The `-stamp-on` and `-stamp-under` operations stamp the first page of a
source PDF onto or under each page in the given range of the input file.
For example,

`cpdf -stamp-on logo.pdf in.pdf odd -o out.pdf`

stamps the file `logo.pdf` onto the odd pages of `in.pdf`, writing to
`out.pdf`. A watermark should go underneath each page:

`cpdf -stamp-under topsecret.pdf in.pdf -o out.pdf`

The position commands in [Position](#position) can be used to locate the stamp more precisely
(they are calculated relative to the crop box of the stamp). Or,
preprocess the stamp with `-shift` first.

The `-scale-stamp-to-fit` option can be added to scale the stamp to fit
the page before applying it. The use of positioning commands together
with `-scale-stamp-to-fit` is not recommended.

The `-relative-to-[crop|trim|art|bleed]box` option takes the positioning
command to be relative to the given box of each page rather than the
media box.

To maintain Tagged PDF, for example with PDF/UA, add
`-process-struct-trees`. The main file will keep its structure; the
stamp will be marked as an artifact.

The `-combine-pages` operation takes two PDF files and stamps each page
of one over each page of the other. The length of the output is the same
as the length of the "under" file. For instance:

`cpdf -combine-pages over.pdf under.pdf -o out.pdf`

Page attributes (such as the display rotation) are taken from the
"under" file. For best results, remove any rotation differences in the
two files using `-upright` first, or by adding `-prerotate` to the
command.

To maintain Tagged PDF, for example with PDF/UA, add
`-process-struct-trees`. The "under" file will keep its structure; the
"over" file will be marked as an artifact.

To reverse the order of combination (to have "over" under "under") add
`-underneath`. To scale the "over" file to fit, add
`-scale-stamp-to-fit`.

## Stamp Text, Dates and Times.

The `-add-text` operation allows text, dates and times to be stamped
over one or more pages of the input at a given position and using a
given font, font size and color.

`cpdf -add-text "Copyright 2014 ACME Corp." in.pdf -o out.pdf`

The default is black 12pt Times New Roman text in the top left of each
page. The text can be placed underneath rather than over the page by
adding the `-underneath` option.

Text previously added by Cpdf may be removed by the `-remove-text`
operation.

`cpdf -remove-text in.pdf -o out.pdf`

NB: Cpdf can only use characters available in the font specified. If
necessary, supply your own font with `-load-ttf` (see below). Times
Roman as used in the preceding example refers to the Standard PDF font
Times Roman, not the Times Roman which is likely to be supplied with
your computer system - which may have more characters available.

### Page Numbers and other Special Codes

There are various special codes to include the page number in the text:

| Code | Effect|
|------|-------|
| `%Page`      | Page number in arabic notation (1, 2, 3...) |
| `%PageDiv2` |       Page number in arabic notation divided by two |
| `%roman`     |      Page number in lower-case roman notation (i, ii, iii...) |
| `%Roman`     |      Page number in upper-case roman notation (I, II, III...) |
| `%EndPage`   |      Last page of document in arabic notation |
| `%Label`     |      The page label of the page |
| `%EndLabel`  |      The page label of the last page |
| `%filename`   |     The full file name of the input document |
| `%URL[text|URL]` |   Add `text`, which links to `URL`. Add `-url-border` to show link area. |
| `%Bookmark<n>`  |   Bookmark text at level n (0, 1, 2, 3, 4) |

For example, the format `"Page %Page of %EndPage"` might become
"Page 5 of 17".

NB: In some circumstances (e.g in batch files) on Microsoft Windows, `%`
is a special character, and must be escaped (written as `%%`). Consult
your local documentation for details.

Bookmark text refers to the first bookmark of the given level on the
stamped page or, if none, the last bookmark text of that level before
that page, so long as uninterrupted by a bookmark of lower level. In
other words, these specials are suitable for adding running heads to a
document.

### Date and Time Formats

| Format | Effect |
|-------|----------------------------------------------|
|  `%a` |  Abbreviated weekday name (Sun, Mon etc.) |
|  `%A` |  Full weekday name (Sunday, Monday etc.) |
|  `%b` |  Abbreviated month name (Jan, Feb etc.) |
|  `%B` |  Full month name (January, February etc.) |
|  `%d` |  Day of the month (01-31) |
| `%e`  | Day of the month (1-31) |
|  `%H` |  Hour in 24-hour clock (00-23) |
|  `%I` |  Hour in 12-hour clock (01-12) |
|  `%j` |  Day of the year (001-366) |
|  `%m` |  Month of the year (01-12) |
|  `%M` |  Minute of the hour (00-59) |
|  `%p` |  "a.m" or "p.m" |
|  `%S` |  Second of the minute (00-61) |
|  `%T` |  Same as `%H:%M:%S` |
|  `%u` |  Weekday (1-7, 1 = Sunday) |
|  `%w` |  Weekday (0-6, 0 = Sunday) |
|  `%Y` |  Year (0000-9999) |
|  `%%` |  The % character. |

### Bates Numbers

Unique page identifiers can be specified by putting `%Bates` in the
format. The starting point can be set with the `-bates` option. For
example:

`cpdf -add-text "Page ID: %Bates" -bates 23745 in.pdf -o out.pdf`

To specify that bates numbering begins at the first page of the range,
use `-bates-at-range` instead. This option must be specified after the
range is specified. To pad the bates number up to a given number of
leading zeros, use `-bates-pad-to` in addition to either `-bates` or
`-bates-at-range`.

### Position

The position of the text may be specified in absolute terms:

`-pos-center "200 200"`

Position the center of the baseline text at (200pt, 200pt)

`-pos-left "200 200"`

Position the left of the baseline of the text at (200pt, 200pt)

`-pos-right "200 200"`

Position the right of the baseline of the text at (200pt, 200pt)

Position may be set relative to certain common points:

  | Specifiation | Effect |
  | ------------------------ | -------------------------------------------------------------- |
  |`-top 10`               | Center of baseline 10 pts down from the top center |
  |`-topleft 10`           | Left of baseline 10 pts down and in from top left |
  |`-topleft "10 20"`      | Left of baseline 10 pts down and 20 pts in from top left |
  |`-topright 10`          | Right of baseline 10 pts down and left from top right |
  |`-topright "10 20"`     | Right of baseline 10 pts down and 20 pts left from top right |
  |`-left 10`              | Left of baseline 10 pts in from center left |
  |`-bottomleft 10`        | Left of baseline 10 pts in and up from bottom left |
  |`-bottomleft "10 20"`   | Left of baseline 10 pts in and 20 pts up from bottom left |
  |`-bottom 10`            | Center of baseline 10 pts up from bottom center |
  |`-bottomright 10`       | Right of baseline 10 pts up and in from bottom right |
  |`-bottomright "10 20"`  | Right of baseline 10 pts up and 20 pts in from bottom right |
  |`-right 10`             | Right of baseline 10 pts in from the center right |
  |`-diagonal`             | Diagonal, bottom left to top right, centered on page |
  |`-reverse-diagonal`     | Diagonal, top left to bottom right, centered on page |
  |`-center`               | Centered on page |

No attempt is made to take account of the page rotation when
interpreting the position, so `-prerotate` may be added to the command
line if the file contains pages with a non-zero viewing rotation (to
silence the rotation warning, add `-no-warn-rotate` instead) This is
equivalent to pre-processing the document with `-upright` (see [Pages](pages.md)).

NB: The use of `-midline` with `-diagonal` and `-reverse-diagonal` is
typical for proper placement.

The `-relative-to-[crop|trim|art|bleed]box` modifier can be added to the
command line to make these measurements relative to the given box
instead of the media box. The `-midline` option may be added to specify
that the positioning commands above are to be considered relative to the
midline of the text, rather than its baseline. Similarly, the `-topline`
option may be used to specify that the position is taken relative to the
top of the text.

The options `-text-90`, `-text-180` and `-text-270` may be added to
rotate the text.

### Font and Size

The standard PDF fonts may be set with the `-font` option. They are:

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

For example, page numbers in Times Italic can be achieved by:

`cpdf -add-text "-%Page-" -font "Times-Italic" in.pdf -o out.pdf`

The font size can be altered with the `-font-size` option, which
specifies the size in points:

`cpdf -add-text "-%Page-" -font-size 36 in.pdf -o out.pdf`

Adding `-embed-std14 <directory>`, given a directory holding the URW
Base35 free fonts, will embed subsetted font files in the PDF for any of
the Standard fonts used. These free fonts may be downloaded from
<https://github.com/ArtifexSoftware/urw-base35-fonts>. This is
important, for example, for PDF/A or PDF/UA documents, which must have
their fonts embedded.

The standard fonts cover only the Latin characters, and are limiting.
Other TrueType fonts may be introduced with the `-load-ttf` option,
giving a name for, and the file name of the font. For example:

`cpdf -load-ttf A=NotoSans-Black.ttf -font A -add-text "-%Page-" -o out.pdf`

Here we have used the Noto Sans font from Google. This and other Google
fonts contain characters for a huge number of scripts, and are available
free from <https://fonts.google.com/noto/>. But you may use any TrueType
font.

### Colors

The `-color` option takes an RGB (3 values), CMYK (4 values), or Grey (1
value) color. Components range between 0 and 1. All the standard web
colours <https://www.w3.org/wiki/CSS/Properties/color/keywords> are
provided as RGB components, and may be selected by name.

`cpdf -add-text "Hullo" -color darkgrey in.pdf -o out.pdf`

`cpdf -add-text "Hullo" -color "0.5 0.5 0.5" in.pdf -o out.pdf`

`cpdf -add-text "Hullo" -color "0.75" in.pdf -o out.pdf`

`cpdf -add-text "Hullo" -color "0.5 0.5 0.4 0.9" in.pdf -o out.pdf`

Partly-transparent text may be specified using the `-opacity` option.
Wholly opaque is 1 and wholly transparent is 0. For example:

`cpdf -add-text "DRAFT" -color "red" -opacity 0.3 -o out.pdf`

### Outline Text

The `-outline` option sets outline text. The line width (default 1pt)
may be set with the `-linewidth` option. For example, to stamp documents
as drafts:

`cpdf -add-text "DRAFT" -diagonal -outline in.pdf -o out.pdf`

### Multi-line Text

The code `\n` can be included in the text string to move to
the next line. In this case, the vertical position refers to the
baseline of the first line of text (if the position is at the top, top
left or top right of the page) or the baseline of the last line of text
(if the position is at the bottom, bottom left or bottom right).

`cpdf -add-text "Specification\n%Page of %EndPage" -topright 10 in.pdf -o out.pdf`

The `-midline` option may be used to make these vertical positions
relative to the midline of a line of text rather than the baseline, as
usual.

The `-line-spacing` option can be used to increase or decrease the line
spacing, where a spacing of 1 is the standard.

`cpdf -add-text "Specification\n%Page of %EndPage" -topright 10 -line-spacing 1.5 in.pdf -o out.pdf`

Justification of multiple lines is handled by the `-justify-left`,
`-justify-right` and`-justify-center` options. The defaults are left
justification for positions relative to the left hand side of the page,
right justification for those relative to the right, and center
justification for positions relative to the center of the page. For
example:

`cpdf -add-text "Long line\nShort" -justify-right in.pdf -o out.pdf`

NB: The `-justify-` option must be after the position (e.g `-top-left`)
option since positions will reset default justification. Diagonal text
is always center-justified.

### Special Characters

If your command line allows for the inclusion of unicode characters, the
input text will be considered as UTF8 by Cpdf. Special characters which
exist in the PDF WinAnsiEncoding Latin 1 code (such as many accented
characters) will be reproduced in the PDF. This does not mean, however,
that every special character can be reproduced - it must exist in the
font. When using a custom font, Cpdf will attempt to convert from UTF8
to the encoding of that font automatically.

### Preserving structure information

To maintain Tagged PDF, for example with PDF/UA, add
`-process-struct-trees`. The main file will keep its structure; the
stamped text will be marked as an artifact.

## Stamping Rectangles

A rectangle may be placed on one or more pages by using the
`-add-rectangle <size>` command. Most of the options discussed above for
text placement apply in the same way. For example:

    cpdf -add-rectangle "200 300" -pos-right 30 -color red -outline in.pdf -o out.pdf

This can be used to blank out or highlight part of the document. The
following positioning options work as you would expect: `-topleft`,
`-top`, `-topright`, `-right`, `-bottomright`, `-bottom`, `-bottomleft`,
`-left`, `-center`. When using the option `-pos-left "x y"`, the point
(x, y) refers to the bottom-left of the rectangle. When using the option
`-pos-right "x y"`, the point (x, y) refers to the bottom-right of the
rectangle. When using the option `-pos-center "x y"`, the point (x, y)
refers to the center of the rectangle. The options `-diagonal` and
`-reverse-diagonal` have no meaning.

## Low-level facilities

These two operations add content directly to the beginning or end of the
page data for a page. You must understand the PDF page description
language to use these.

`cpdf -prepend-content <content> in.pdf [<range>] -o out.pdf`

`cpdf -postpend-content <content> in.pdf [<range>] -o out.pdf`

The `-fast` option may be added (see Chapter 1). The `-stamp-as-xobject`
operation puts a file in another as a Form XObject on the given pages.
You can then use `-prepend-content` or `-postpend-content` to use it.

`cpdf -stamp-as-xobject stamp.pdf in.pdf [<range>] -o out.pdf`
