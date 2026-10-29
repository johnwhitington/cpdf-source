# Document Information and Metadata

    cpdf -info[-json] [-utf8] [-in | -cm | -mm] in.pdf

    cpdf -page-info[-json] [-in | -cm | -mm] in.pdf [<range>]

    cpdf -pages in.pdf

    cpdf -sig-info [-json] in.pdf

    cpdf -set-title \<title of document\>
         [-also-set-xmp] [-just-set-xmp] in.pdf -o out.pdf
    (Also -set-author etc.)

    cpdf -set-page-layout <layout> in.pdf -o out.pdf

    cpdf -set-page-mode <mode> in.pdf -o out.pdf
    cpdf -set-non-full-screen-page-mode <mode> in.pdf -o out.pdf

    cpdf -hide-toolbar <true | false> in.pdf -o out.pdf
         -hide-menubar
         -hide-window-ui
         -fit-window
         -center-window
         -display-doc-title

    cpdf -open-at-page <page number> in.pdf -o out.pdf
    cpdf -open-at-page-fit <page number> in.pdf -o out.pdf
    cpdf -open-at-page-custom <destination> in.pdf -o out.pdf

    cpdf -set-language <language> in.pdf -o out.pdf

    cpdf -set-metadata <metadata-file> in.pdf -o out.pdf
    cpdf -remove-metadata in.pdf -o out.pdf
    cpdf -remove-all-metadata in.pdf -o out.pdf
    cpdf -print-metadata in.pdf
    cpdf -extract-all-metadata in.pdf -o <directory>
    cpdf -create-metadata in.pdf -o out.pdf
    cpdf -set-metadata-date <date> in.pdf -o out.pdf

    cpdf -add-page-labels in.pdf [<range>] -o out.pdf
         [-label-style <style>] [-label-prefix <string>]
         [-label-startval <integer>] [-labels-progress]
    cpdf -add-page-labels-json in.json in.pdf -o out.pdf

    cpdf -remove-page-labels in.pdf -o out.pdf
    cpdf -print-page-labels[-json] in.pdf

    cpdf -composition[-json] in.pdf

## Reading Document Information

The `-info` operation prints entries from the document information
dictionary, and from any XMP metadata to standard output.

    $cpdf -info pdf\_reference.pdf
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

The details of the format for creation and modification dates can be
found in [Dates](dates.md). If page boxes vary among pages, the entry will read
`various`. Add `-in`, `-cm` or `-mm` to print boxes in inches,
centimetres, or millimetres instead of points.

By default, Cpdf strips to ASCII, discarding character codes in excess
of 127. In order to preserve the original unicode, add the `-utf8`
option. To disable all post-processing of the string, add `-raw`.

The `-info-json` operation prints the information in JSON format
instead. For example:

    {
      "Encryption": "Not encrypted",
      "Permissions": [],
      "Linearized": true,
      "Object streams": true,
      "ID": [
        "0b1f990718e2a92c0c112fbf08b233fb", "b2f1dbee369e11d9b951000393c97fd8"
      ],
      "Version": [ 1, 5 ],
      "Pages": 1236,
      "Title": "PDF Reference, version 1.6",
      "Author": "Adobe Systems Incorporated",
      "Subject": "Adobe Portable Document Format (PDF)",
      "Keywords": null,
      "Creator": "FrameMaker 7.0",
      "Producer": "Acrobat Distiller 6.0.1 for Macintosh",
      "Created": "D:20041114084116Z",
      "Modified": "D:20041114163850-08'00'",
      "Trapped": false,
      "PageMode": "UseOutlines",
      "PageLayout": null,
      "OpenAction":
        [{ "I": 1 }, { "N": "/XYZ" }, { "I": -32768 },
         { "I": -32768 }, { "I": 1 }]
      "HideToolbar": null,
      "HideMenubar": null,
      "HideWindowUI": null,
      "FitWindow": null,
      "CenterWindow": null,
      "DisplayDocTitle": true,
      "NonFullPageScreenMode": null,
      "AcroForm": false,
      "XFA": false,
      "Marked": false,
      "UserProperties": false,
      "Suspects": false,
      "MediaBox": [ 0.0, 0.0, 612.0, 792.0 ],
      "CropBox": [ 41.0, 63.0, 572.0, 729.0 ],
      "BleedBox": null,
      "TrimBox": "various",
      "ArtBox": "various",
      "Subformats": [],
      "Language": "en-us"
      "XMP dc:title": "PDF Reference, version 1.6",
      "XMP dc:creator": "Adobe Systems Incorporated",
      "XMP dc:description": "Adobe Portable Document Format (PDF)"
    }

The `-page-info` operation prints the page label, media box and other
boxes, and number of annotations page-by-page to standard output, for
all pages in the current range.

    $cpdf -page-info 14psfonts.pdf
    Page 1:
    Label: i
    MediaBox: 0.000000 0.000000 600.000000 450.000000
    CropBox: 200.000000 200.000000 500.000000 500.000000
    BleedBox: 
    TrimBox: 
    ArtBox:
    Rotation: 0
    Annotations: 0

Note that the format for boxes is minimum x, minimum y, maximum x,
maximum y. Add `-in`, `-cm` or `mm` to print boxes in inches,
centimetres, or millimetres instead of points. Using `-page-info-json`
we can get the information in JSON format. For example:

    [
      {
        "Page": 1,
        "Label": "i",
        "MediaBox": [ 0.0, 0.0, 600.0, 450.0 ],
        "CropBox": [ 200.0, 200.0, 500.0, 500.0 ],
        "BleedBox": null,
        "TrimBox": null,
        "ArtBox": null,
        "Rotation": 0,
        "Annotations": 0
      }
    ]

The `-pages` operation prints the number of pages in the file.

    cpdf -pages Archos.pdf
    8

The `-sig-info` operation prints information for each digital signature
in the document:

    /Sig, /Adobe.PPKLite, /adbe.x509.rsa\_sha1, DIN Deutsches Institut

Add `-json` to use JSON instead:

    [
      {
        "/Type": "/Sig",
        "/Filter": "/Adobe.PPKLite",
        "/SubFilter": "/adbe.x509.rsa\_sha1",
        "/Name": "DIN Deutsches Institut"
      }
    ]

## Setting Document Information

The *document information dictionary* in a PDF file specifies various
pieces of information about a PDF. These can be consulted in a PDF
viewer (for instance, Acrobat).

Here is a summary of the commands for setting entries in the document
information dictionary:

| Information | Example command-line fragment |
| ----------- | -------------------------------|
|  Title              | `cpdf -set-title "Discourses"` |
|  Author             | `cpdf -set-author "Joe Smith"` |
|  Subject            | `cpdf -set-subject "Behavior"` |
|  Keywords           | `cpdf -set-keywords "Ape Primate"` |
|  Creator            | `cpdf -set-creator "Original Program"` |
|  Producer           | `cpdf -set-producer "Distilling Program"` |
|  Creation Date      | `cpdf -set-create "D:19970915110347-08'00'"` |
|  Modification Date  | `cpdf -set-modify "D:19970915110347-08'00'"` |
|  Mark as Trapped    | `cpdf -set-trapped` |
|  Mark as Untrapped  | `cpdf -set-untrapped` |

(The details of the format for creation and modification dates can be
found in [Dates](dates.md). Using the date `"now"` uses the time and date at
which the command is executed (unless the environment variable
`CPDF_REPRODUCIBLE_DATES` is set to `true`). Note also that `-producer`
and `-creator` may be used to set the producer and/or the creator when
writing any file, separate from the operations described in this
chapter.)

For example, to set the title, the full command line would be

`cpdf -set-title "A Night in London" in.pdf -o out.pdf`

The text string is considered to be in UTF8 format, unless the `-raw`
option is added - in which case, it is unprocessed, save for the
replacement of any octal escape sequence such as `\017`, which is
replaced by a character of its value (here, 15).

To set also any field in the XMP metadata, add `-also-set-xmp`. The
field must exist already. To set only the field (not the document
information dictionary), add `-just-set-xmp` instead.

To delete existing non-XMP metadata in line with PDF 2.0, use
`-remove-dict-entry "/Info"` as described in [Miscellaneous](miscellaneous.md).

## XMP Metadata

PDF files can contain one or more pieces of arbitrary metadata, often in
XMP format. This is typically stored in an uncompressed stream, so that
other applications can read it without having to decode the whole PDF.

To set the main metadata:

`cpdf -set-metadata data.xml in.pdf -o out.pdf`

To remove the main metadata:

`cpdf -remove-metadata in.pdf -o out.pdf`

To remove all metadata:

`cpdf -remove-all-metadata in.pdf -o out.pdf`

To print the main metadata to standard output:

`cpdf -print-metadata in.pdf`

To extract all metadata to file:

`cpdf -extract-all-metadata in.pdf -o dir`

This will create zero or one files `dir/main.xml` and zero or more files
`dir/<objnum>.xml`.

To create main XMP metadata from scratch, using any information in the
Document Information Dictionary (old-style metadata):

`cpdf -create-metadata in.pdf -o out.pdf`

To set the main XMP metadata date field, use:

`cpdf -set-metadata-date <date> in.pdf -o out.pdf`

The date format is defined in [Dates](dates.md). Using the
date "now" uses the time and date at which the command is executed.

## Upon Opening a Document

A mark can be put in a PDF to set the page viewing characteristics upon
opening.

NB: If the file has a valid `/OpenAction` setting, which tells the PDF
reader to open at a certain page or position on a page, this can
override the page layout or display options described below. To prevent
this, preprocess the file with the `-remove-dict-entry` functionality
from [Miscellaneous](miscellaneous.md):

`cpdf -remove-dict-entry /OpenAction in.pdf -o out.pdf`

You can see if the file has such an open action by referring to the
output of `-info`.

NB: The initial view displayed by the dialog box File $\rightarrow$
Properties $\rightarrow$ Initial View in Adobe Reader / Acrobat may not
reflect exactly the options here. The options here set the flags within
the PDF - Adobe products may show different wording.

### Page Layout

The `-set-page-layout` operation specifies the page layout to be used
when a document is opened in, for instance, Acrobat. The possible
(case-sensitive) values are:

| Layout | Effect |
| ------ | ------ |
| `SinglePage`    |   Display one page at a time |
| `OneColumn`     |   Display the pages in one column |
|  `TwoColumnLeft`|    Display the pages in two columns, odd numbered pages on the left |
|  `TwoColumnRight`|   Display the pages in two columns, even numbered pages on the left |
|  `TwoPageLeft`    |  (PDF 1.5 and above) Display the pages two at a time, odd numbered pages on the left |
|  `TwoPageRight`   |  (PDF 1.5 and above) Display the pages two at a time, even numbered pages on the left |

For instance:

`cpdf -set-page-layout TwoColumnRight in.pdf -o out.pdf`

### Page Mode

The *page mode* in a PDF file defines how a viewer should display the
document when first opened. The possible (case-sensitive) values are:

| Mode | Effect |
| ---- | ------ |
|  `UseNone`      |    Neither document outline nor thumbnail images visible |
|  `UseOutlines`  |    Document outline (bookmarks) visible |
| `UseThumbs`     |   Thumbnail images visible |
|  `FullScreen`   |    Full-screen mode (no menu bar, window controls, or anything but the document visible) |
|  `UseOC`        |    (PDF 1.5 and above) Optional content group panel visible |
|  `UseAttachments`|   (PDF 1.5 and above) Attachments panel visible |

For instance:

`cpdf -set-page-mode FullScreen in.pdf -o out.pdf`

If full screen mode is selected for document opening, we can also set a
mode to be used when the user exits from full-screen mode:

`cpdf -set-non-full-screen-page-mode UseAttachments in.pdf -o out.pdf`

As would be expected, `FullScreen` is not allowed here.

### Display Options

The appearance of the PDF viewer upon opening a document may be set with
these options. Each is boolean - supply `true` or `false`:

| Option | Effect |
| ------ | ------ |
|  `-hide-toolbar`     |   Hide the viewer's toolbar |
|  `-hide-menubar`     |   Document outline (bookmarks) visible |
|  `-hide-window-ui`   |   Hide the viewer's scroll bars |
|  `-fit-window`       |   Resize the document's windows to fit size of first page |
|  `-center-window`    |   Position the document window in the center of the screen |
| `-display-doc-title` |  Display the document title instead of the file name in the title bar |

For instance:

`cpdf -hide-toolbar true in.pdf -o out.pdf`

The page a PDF file opens at can be set using `-open-at-page`:

`cpdf -open-at-page 15 in.pdf -o out.pdf`

To have that page scaled to fit the window in the viewer, use
`-open-at-page-fit` instead:

`cpdf -open-at-page-fit end in.pdf -o out.pdf`

(Here, we used `end` to open at the last page. Any page specification
describing a single page is ok here.)

Alternatively, we may specify a full destination, of the kind described
on page :

`cpdf -open-at-page-custom "[3 /FitR 100 100 300 300]" in.pdf -o out.pdf`

## Document Language

The document language may be set by giving an IETF BCP 47 language tag:

`cpdf -set-language "en-GB" in.pdf -o out.pdf`

This is the top-level language tag. Existing tags on individual parts of
the document are preserved.

## Page Labels

It is possible to add *page labels* to a document. These are not the
printed on the page, but may be displayed alongside thumbnails or in
print dialogue boxes by PDF readers. We use `-add-page-labels` to do
this, by default with decimal arabic numbers (1,2,3...). We can add
`-label-style` to choose what type of labels to add from these kinds:


| Label style | Example |
| ----------- | ------- |
| `DecimalArabic` | 1, 2, 3, 4, 5... |
|       `LowercaseRoman` | i, ii, iii, iv, v... |
|       `UppercaseRoman` | I, II, III, IV, V... |
|     `LowercaseLetters` | a, b, c, ..., z, aa, bb... |
|     `UppercaseLetters` | A, B, C, ..., Z, AA, BB... |
|    `NoLabelPrefixOnly` | No number, but a prefix will be used if defined. |

We can use `-label-prefix` to add a textual prefix to each label.
Consider a file with twenty pages and no current page labels (a PDF
reader will assume 1,2,3...if there are none). We will add the following
page labels:

i, ii, iii, iv, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, A-0, A-1, A-2, A-3, A-4,
A-5

Here are the commands, in order:

`cpdf -add-page-labels in.pdf 1-4 -label-style LowercaseRoman -o out.pdf`

`cpdf -add-page-labels out.pdf 5-14 -o out2.pdf`

`cpdf -add-page-labels out2.pdf 15-20 -label-prefix "A-" -label-startval 0 -o out3.pdf`

By default the labels begin at page number 1 for each range. To override
this, we can use `-label-startval` (we used 0 in the final command),
where we want the numbers to begin at zero rather than one. The option
`-labels-progress` can be added to make sure the start value progresses
between sub-ranges when the page range specified is disjoint, e.g
`1-9, 30-40` or `odd`.

Alternatively, we can use `-add-page-labels-json` where a JSON page
label file has been prepared as described below.

Page labels may be removed altogether by using `-remove-page-labels`
command. To print the page labels from an existing file, use
`-print-page-labels`. For example:

    $ cpdf -print-page-labels in.pdf
    labelstyle: LowercaseRoman
    labelprefix: None
    startpage: 1
    startvalue: 1
    labelstyle: DecimalArabic
    labelprefix: A
    startpage: 9
    startvalue: 1

Or, in JSON format with `-print-page-labels-json`:

    [
      {
        "labelstyle": "LowercaseRoman",
        "labelprefix": null,
        "startpage": 1,
        "startvalue": 1
      },
      {
        "labelstyle": "DecimalArabic",
        "labelprefix": "A",
        "startpage": 9,
        "startvalue": 1
      }
    ]

In both cases, the `labelprefix` string is in UTF8 format.

## Composition of a PDF

The `-composition` and `-composition-json` operations show how much
space in a PDF is used by each kind of data. Here is the output of
`-composition` for this manual:

    $ cpdf -composition cpdfmanual.pdf
    Images: 0 bytes (0.00%)
    Fonts: 144731 bytes (46.72%)
    Content streams: 132767 bytes (42.85%)
    Structure Info: 0 bytes (0.00%)
    Attached Files: 0 bytes (0.00%)
    XRef Table: 21082 bytes (6.80%)
    Piece Info: 0 bytes (0.00%)
    Unclassified: 11229 bytes (3.62%)

And here it is in JSON format:

    $ cpdf -composition-json cpdfmanual.pdf
    [
      ["Images", 0, 0.0],
      ["Fonts", 144731, 46.71620256351494],
      ["Content streams", 132767, 42.854468398271194],
      ["Structure Info", 0, 0.0],
      ["Attached Files", 0, 0.0],
      ["XRef Table", 21082, 6.8048378194306816],
      ["Piece Info", 0, 0.0],
      ["Unclassified", 11229, 3.6244912187831857]
    ]

Note that, due to small inaccuracies in the method, it is possible for
the `Unclassified` numbers to be negative.
