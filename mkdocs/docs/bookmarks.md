# Bookmarks

    cpdf -list-bookmarks [-utf8] in.pdf

    cpdf -list-bookmarks-json [-preserve-actions] in.pdf

    cpdf -remove-bookmarks in.pdf -o out.pdf

    cpdf -add-bookmarks <bookmark file> in.pdf -o out.pdf

    cpdf -add-bookmarks-json <bookmark file> in.pdf -o out.pdf

    cpdf -bookmarks-open-to-level <n> in.pdf -o out.pdf

    cpdf -table-of-contents [-toc-title] [-toc-no-bookmark] [-toc-dot-leaders]  
         [-font <font>] [-font-size <size>] [-embed-std14 /path/to/fonts]  
         [-process-struct-trees] [-subformat <subformat>]  
         in.pdf -o out.pdf

PDF bookmarks (properly called the *document outline*) represent a tree
of references to parts of the file, typically displayed at the side of
the screen. The user can click on one to move to the specified place.
Cpdf provides facilities to list, add, and remove bookmarks. The format
used by the list and add operations is the same, so you can feed the
output of one into the other, for instance to copy bookmarks.

## List Bookmarks

The `-list-bookmarks` operation prints (to standard output) the
bookmarks in a file. The first column gives the level of the tree at
which a particular bookmark is. Then the text of the bookmark in quotes.
Then the page number which the bookmark points to. Then (optionally) the
word \"open\" if the bookmark should have its children (at the level
immediately below) visible when the file is loaded. Then the destination
(see below). For example, upon executing

`cpdf -list-bookmarks doc.pdf`

the result might be:

    0 "Part 1" 1 open
    1 "Part 1A" 2 "[2 /XYZ 200 400 null]"
    1 "Part 1B" 3
    0 "Part 2" 4
    1 "Part 2a" 5

If the page number is 0, it indicates that clicking on that entry
doesn't move to a page.

By default, Cpdf converts unicode to ASCII text, dropping characters
outside the ASCII range. To prevent this, and return unicode UTF8
output, add the `-utf8` option to the command. To prevent any
processing, use the `-raw` option. See Section FIXME for more information. A newline in a bookmark
is represented as `"\n"`.

By using `-list-bookmarks-json` instead, the bookmarks are formatted as
a JSON array, in order, of dictionaries formatted thus:

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

Note that the colour (RGB each from 0.0 to 1.0) and shape of the text
(bold, italic, or both) can be read and set with the JSON format.

The `-preserve-actions` option will give the target in its original PDF
form, rather than resolving it to a destination (see below).

See Chapter 15 for more details of Cpdf's JSON formatting. There are two
differences here: bookmark text is always UTF8, and the numbers for
`level` and `page` are plain, rather than begin surrounded with
` { "I": }`.

### Destinations

The destination is an extended description of where the bookmark should
point to (i.e it can be more detailed than just giving the page). For
example, it may point to a section heading halfway down a page. Here are
the possibilities:

| Format | Description |
| ------ | ------------|
| \[*p* /XYZ *left* *top* *zoom*\] | Display page number *p* with (*left*, *top*) positioned at upper-left of window and magnification of *zoom*. Writing "null" for any of *left*, *top* or *zoom* specifies no change. A *zoom* of 0 is the same as "null". |
| \[*p* /Fit\]                                 | Display page number *p* so as to fit fully within the window. |
| \[*p* /FitH *top*\]                          | Display page number *p* with vertical coordinate *top* at the top of the window and the page magnified so its width fits the window. A null value for *top* implies no change. |
| \[*p* /FitV *left*\]                          | Display page number *p* with horizontal coordinate *left* at the left of the window, and the page magnified so its height fits the window. A null value for *left* implies no change. |
| \[*p* /FitR *left* *bottom* *right* *top*\]   | Display page number *p* magnified so as to fit entirely within the rectangle specified by the other parameters. |
| \[*p* /FitB\]                                 | As for /Fit but with the page's bounding box (see below). |
| \[*p* /FitBH *top*\]                          | As for /FitH but with the page's bounding box (see below). |
| \[*p* /FitBV *left*\]                         | As for /FitV but with the page's bounding box (see below). |

The *bounding box* is the intersection of the page's crop box and the
bounding box of the page contents. Some other kinds of destination may
be produced by `-list-bookmarks`. They will be preserved by
`-add-bookmarks` and may be edited as your risk.

## Remove Bookmarks

The `-remove-bookmarks` operations removes all bookmarks from the file.

`cpdf -remove-bookmarks in.pdf -o out.pdf`

## Add Bookmarks

The `-add-bookmarks` file adds bookmarks as specified by a *bookmarks
file*, a text file in ASCII or UTF8 encoding and in the same format as
that produced by the `-list-bookmarks` operation. If there are any
bookmarks in the input PDF already, they are discarded. For example, if
the file `bookmarks.txt` contains the output from `-list-bookmarks`
above, then the command

`cpdf -add-bookmarks bookmarks.txt in.pdf -o out.pdf`

adds the bookmarks to the input file, writing to `out.pdf`. An error
will be given if the bookmarks file is not in the correct form (in
particular, the numbers in the first column which specify the level must
form a proper tree with no entry being more than one greater than the
last).

Bookmarks in JSON format (see above) may be added with
`-add-bookmarks-json`:

`cpdf -add-bookmarks-json bookmarks.json in.pdf -o out.pdf`

Remember that strings in JSON bookmark files are in UTF8.

Note that, if `-preserve-actions` is used with `-list-bookmarks-json`,
it will not be feasible to use `-add-bookmarks-json` to add these
bookmarks to a different or modified file. They may be round-tripped to
the same, unmodified file, of course.

## Opening bookmarks

As an alternative to extracting a bookmark file and manipulating the
open-status of bookmarks, mass manipulation may be achieved by the
following operation:

`cpdf -bookmarks-open-to-level <level> in.pdf -o out.pdf`

A level of 0 will close all bookmarks, level 1 will open just the top
level, closing all others etc. To open all of them, pick a sufficiently
large level.

## Making a Table of Contents

Cpdf can automatically generate a table of contents from existing
bookmarks, adding it to the beginning of the document.

`cpdf -table-of-contents in.pdf -o out.pdf`

The page(s) added will have the same dimensions, media and crop boxes as
the first page of the original file. The default title is "Table of
Contents", though this may be changed:

`cpdf -table-of-contents -toc-title "Contents" in.pdf -o out.pdf`

An empty title removes the title. The sequence `\n` may be used to split
the title into lines. The default font is 12pt Times Roman (and 24pt for
the title). The base font and size may be changed with `-font` and
`-font-size` (see Section FIXME for full details):

`cpdf -table-of-contents -font "Courier-Bold" -font-size 8` in.pdf -o out.pdf`

Dot leaders may be added with `-toc-dot-leaders`:

`cpdf -table-of-contents -toc-dot-leaders in.pdf -o out.pdf`

By default, an entry for the new table of contents will be added to the
document's bookmarks. To suppress this behaviour, add
`-toc-no-bookmark`:

`cpdf -table-of-contents -toc-no-bookmark in.pdf -o out.pdf`

To create a structure tree for the table of contents, and merge it with
the existing one (for example, when adding a table of contents to a
PDF/UA file), add `-process-struct-trees` to the command. For PDF/UA-2,
add also `-subformat "PDF/UA-2"`. You may also need `-embed-std14` as
described in chapter FIXME, since fully-embedded fonts are a requirement of
some PDF subformats.
