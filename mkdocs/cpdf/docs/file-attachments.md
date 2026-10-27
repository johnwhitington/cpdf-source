# File Attachments

cpdf -attach-file <filename> [-to-page <page number>]
     [-afd <string>] [-afr <relationship>] [-attach-file ...]
     in.pdf -o out.pdf
cpdf -attach-files-json in.json in.pdf -o out.pdf

cpdf -list-attached-files [-json] [-include-data] in.pdf

cpdf -remove-files in.pdf -o out.pdf

cpdf -dump-attachments in.pdf -o <directory>

PDF supports adding attachments (files of any kind, including other
PDFs) to an existing file. The Cpdf tool supports adding and removing
*document-level attachments* --- that is, ones which are associated with
the document as a whole rather than with an individual page, and also
*page-level attachments*, associated with a particular page.

## Adding Attachments

To add an attachment, use the `-attach-file` operation. For instance,

`cpdf -attach-file sheet.xls in.pdf -o out.pdf`

attaches the Excel spreadsheet `sheet.xls` to the input file. If the
file already has attachments, the new file is added to their number. You
can specify multiple files to be attached by using `-attach-file`
multiple times. They will be attached in the given order.

The `-to-page` option can be used to specify that the files will be
attached to the given page, rather than at the document level. The
`-to-page` option may be specified at most once.

The `-afd` and `-afr` options may be used to specify the textual
description and/or relationship for the file.

Alternatively, attachments may be added with `-attach-files-json` from a
JSON file in the format used by
`-list-attached-files -json -include-data`, described below.

## Listing Attachments

To list all document- and page-level attachments, use the
`-list-attached-files` operation. The page number and filename of each
attachment is given, page 0 representing a document-level attachment,
then any description then any relationship:

    $cpdf -list-attached-files 14psfonts.pdf
    0 | utility.ml |  | Unspecified
    0 | utility.mli |  |
    4 | notes.xls | "My notes" | Unspecified

The same data may be listed in JSON format:

    $cpdf -list-attached-files -json 14psfonts.pdf
    [
      {
        "Page": 0,
        "Name": "utility.ml",
        "Description": null,
        "Relationship": null
      }
    ]
    ...

Adding `-include-data` to the command line when calling
`-list-attached-files -json` will include an extra JSON field `data`
containing the file data for each attachment.

## Removing Attachments

To remove all document-level and page-level attachments from a file, use
the `-remove-files` operation:

`cpdf -remove-files in.pdf -o out.pdf`

## Extracting Attachments

The `-dump-attachments` operation, when given a PDF file and a directory
path as the output, will write each attachment under its filename (as
displayed by `-list-attached-files` to that directory. The directory
must exist prior to the call.

`cpdf -dump-attachments in.pdf -o /home/fred/attachments`

Unless either the `-raw` or `-utf8` option is given, the filenames are
stripped of dubious special characters before writing. It is converted
from unicode to 7 bit ASCII, and the following characters are removed,
in addition to any character with ASCII code less than 32:

` / ? < > \ : * | " ^ + =`

As an alternative, and as described, adding `-include-data` to the
command line when calling `-list-attached-files -json` will include an
extra JSON field `data` containing the file data for each attachment.
