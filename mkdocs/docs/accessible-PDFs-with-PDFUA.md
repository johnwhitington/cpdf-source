# Accessible PDFs with PDF/UA

cpdf -print-struct-tree in.pdf

cpdf -extract-struct-tree in.pdf -o out.json

cpdf -replace-struct-tree in.json in.pdf -o out.pdf

cpdf -remove-struct-tree in.pdf -o out.pdf

cpdf -mark-as-artifact in.pdf -o out.pdf

cpdf -verify "PDF/UA-1(matterhorn)" [-json] in.pdf

cpdf -verify "PDF/UA-1(matterhorn)" -verify-single \<test\> [-json] in.pdf

cpdf -mark-as ["PDF/UA-1" | "PDF/UA-2"] in.pdf -o out.pdf

cpdf -remove-mark ["PDF/UA-1" | "PDF/UA-2"] in.pdf -o out.pdf

cpdf -create-pdf-ua-\<1|2\> \<title\> [-create-pdf-pages \<n\>]
      [-create-pdf-papersize \<paper size\>] -o out.pdf

PDF/UA (Universal Accessibility) is a PDF subformat whose rules consist
of a set of machine-checkable and human-checkable-only requirements to
make PDF documents accessible for all users - for example, those using
screen readers. Cpdf has some basic facilities for manipulating the
extra PDF constructs which are used in (amongst others) PDF/UA, and a
basic verifier for many of the machine-checkable requirements.

## Structure trees

In a PDF document, the optional Structure Tree is a parallel construct
which describes the logical structure of a document (as opposed to the
information for rendering the document on the screen or printing it out,
which every PDF of course contains.)

We can print an abbreviated form of the structure tree to standard
output:

`cpdf -print-struct-tree in.pdf`

This might yield:

    StructTreeRoot
        Document
            Sect
                P (1)
                    Span (1)
                Figure (1)
            Sect
                H1 (2)
                TOC
                    TOCI
                        P
                          Link (2)
        .       .
        .       .
        .       .

The numbers in parentheses are the page numbers for structure elements,
where present. We can extract the full structure tree to JSON for
inspection or manupulation:

`cpdf -extract-struct-tree in.pdf -o out.json`

Here is a typical fragment:

    [
      [ 0, { "/CPDFJSONformatversion": 1, "/CPDFJSONpageobjnumbers": [ 52 ] } ],
      [
        102,
        {
          "/Type": { "N": "/StructElem" },
          "/S": { "N": "/TD" },
          "/P": 98,
          "/Pg": 52,
          "/K": { "I": 38 },
          "/T": { "U": "row #7, col #3" },
          "/A": {
            "/O": { "N": "/Layout" },
            "/Height": { "F": 18.28 },
            "/Width": { "F": 73.07689999999999 }
          }
        }
      ],
      [
        15,
        {
          "/Type": { "N": "/StructElem" },
          "/S": { "N": "/TD" },
          "/P": 59,
          "/Pg": 52,
          "/K": { "I": 20 },
          "/T": { "U": "row #3, col #5" },
          "/A": {
            "/O": { "N": "/Layout" },
            "/Height": { "F": 18.28 },
            "/Width": { "F": 73.07689999999999 }
          }
        }
      ],
    ...

This JSON file contains the structure tree objects from the file, using
the format described in chapter [15](#chap:15){reference-type="ref"
reference="chap:15"}. There is a special entry in object `0` which gives
the key to the page object numbers. In this example, there is one page
with object number `52`.

This JSON file can be edited, for example to change text strings, and
reapplied to the same file from which it was extracted:

`cpdf -replace-struct-tree out.json in.pdf -o out.pdf`

If extra objects are required, they should be introduced with negative
object numbers: Cpdf will renumber them on import so as not to clash
with any existing numbers.

To remove a structure tree from a PDF, we can use `-remove-struct-tree`:

`cpdf -remove-struct-tree in.pdf -o out.pdf`

This removes the structure tree and all references to it, including from
inside page content. In addition we can, afterward, use
`-mark-as-artifact`:

`cpdf -mark-as-artifact in.pdf -o out.pdf`

This marks all content in the file as being an artifact.

## Verifying conformance to PDF/UA

Cpdf contains a new, experimental verifier for PDF/UA via most of the
machine-checkable subset of the Matterhorn Protocol, a list of checks
based on the PDF/UA-1 specification. For example, we can run:

`cpdf -verify "PDF/UA-1(matterhorn)" in.pdf`

We see:

    06-001 UA1:7.1-8 Document does not contain an XMP metadata stream 
    07-001 UA1:7.1-9 ViewerPreferences dictionary of the Catalog dictionary does
    not contain a DisplayDocTitle entry 
    11-006 UA1:7.2-3 Natural language for document metadata cannot be determined.
    ("No top-level /Lang")
    28-004 UA1:7.18.1-4 An annotation, other than of subtype Widget, does not
    have a Contents entry and does not have an alternative description (in the
    form of an Alt entry in the enclosing structure element). 
    28-008 UA1:7.18.3-1 A page containing an annotation does not contain a Tabs
    entry 
    28-011 UA1:7.18.5-1 A link annotation is not nested within a <Link> tag. 
    28-012 UA1:7.18.5-2 A link annotation does not include an alternate
    description in its Contents entry. 

The first column here is the Matterhorn Protocol checkpoint, the second
the reference in the PDF/UA-1 standard docunment, the third the textual
description from the Matterhorn Protocol, and an optional fourth (in
parentheses) any extra information available.

The same information is available in JSON format by adding `-json` to
the command line:

    [
      {
        "name": "06-001",
        "section": "UA1:7.1-8",
        "error": "Document does not contain an XMP metadata stream",
        "extra": null
      },
      {
        "name": "07-001",
        "section": "UA1:7.1-9",
        "error": "ViewerPreferences dictionary of the Catalog dictionary does not
    contain a DisplayDocTitle entry",
        "extra": null
      },
      {
        "name": "11-006",
        "section": "UA1:7.2-3",
        "error": "Natural language for document metadata cannot be determined.",
        "extra": "No top-level /Lang"
      },
      {
        "name": "28-004",
        "section": "UA1:7.18.1-4",
        "error": "An annotation, other than of subtype Widget, does not have a
    Contents entry and does not have an alternative description (in the form of
    an Alt entry in the enclosing structure element).",
        "extra": null
      },
      {
        "name": "28-008",
        "section": "UA1:7.18.3-1",
        "error": "A page containing an annotation does not contain a Tabs entry",
        "extra": null
      },
      {
        "name": "28-011",
        "section": "UA1:7.18.5-1",
        "error": "A link annotation is not nested within a <Link> tag.",
        "extra": null
      },
      {
        "name": "28-012",
        "section": "UA1:7.18.5-2",
        "error": "A link annotation does not include an alternate description in
    its Contents entry.",
        "extra": null
      }

If verifying many files for a single fault, we may choose which test to
run by adding`-verify-single <testname>` to the command line. For
example:

`cpdf -verify "PDF/UA-1(matterhorn)" -verify-single "28-012" in.pdf`

Presently, Matterhorn tests 31-001--016,018,030 are unimplemented.
Matterhorn tests 31-027,10-001,11-001--005 are partially implemented.
All others are implemented.

## PDF/UA compliance markers

Once we are sure a file complies to PDF/UA, in terms of both machine and
human checks, we can mark it as such:

`cpdf -mark-as "PDF/UA-1" in.pdf -o out.pdf`

Or, for the more recent PDF/UA-2 standard:

`cpdf -mark-as "PDF/UA-2" in.pdf -o out.pdf`

To remove such a marker we can use, for example:

`cpdf -remove-mark "PDF/UA-1" in.pdf -o out.pdf`

## Merging and splitting PDF/UA files

The `-process-struct-trees` option should always be used in conjunction
with any splitting or merging command to preserve PDF/UA compliance.
Sometimes `-subformat` may be required too. Details are given in chapter FIXME.

## Creating new PDF/UA files

To create a new PDF/UA-1 file, with A4 portrait paper, one page, and the
title `"My Book"`, we may write:

`cpdf -create-pdf-ua-1 "My Book" -o out.pdf`

A title is needed for every PDF/UA document (even a blank one) for it to
meet the standard. For `PDF/UA-2`, use `-create-pdf-ua-2` instead. To
make it valid, you must also draw a top-level PDF/UA-2 Document tag as
described below i.e:

`cpdf -create-pdf-ua-2 "My Book" AND -draw -draw-struct-tree`\
`     -namespace PDF2 -stag Document -end-stag -o out.pdf`

## Drawing PDF/UA files

Cpdf can add PDF/UA structure data when drawing on new PDF/UA files. For
example the following produces a valid PDF/UA-1 file with structure
information:

`cpdf -create-pdf-ua-1 "Hello" AND`\
`     -embed-std14 /path/to/fonts -draw-struct-tree`\
`     -draw -bt -text "Hello, World" -et -o out.pdf`

Note we had to specify embedded fonts to make this a valid PDF/UA-1
file. To make a valid PDF/UA-2 file we must also add a top-level
Document structure tag with the appropriate namespace. Here is the
PDF/UA-2 version of our file:

`cpdf -create-pdf-ua-2 "Hello" AND `\
`     -embed-std14 /path/to/fonts -draw-struct-tree`\
`     -draw -namespace PDF2 -stag Document -namespace PDF`\
`     -bt -text "Hello, World" -et -end-stag -o out.pdf`

See chapter FIXME for more details about adding structure information when
drawing.

## Remediation of PDF/UA verification errors

Remediation of a file which claims to match PDF/UA but which does not (either
failing human or mechanical tests) is a complex topic. In this section, we list
possible remediations for a file which fails mechanical verification with Cpdf
or another verification tool. Sometimes these will be clear and simple -- for
example where some piece of document metadata is missing -- and sometimes they
will be almost impossible. Of course, often the truth lies between those two
extremes.

When all else fails, it may be possible to modify the basic structures of the
PDF manually. This may be done by extracting the PDF to JSON using
`-output-json` from chapter FIXME, modifying the file manually in a text editor
or automatically with a JSON processing tool such as `jq` and converting back
to a PDF with `-j`. If the remediation requires altering page content streams,
the option `-output-json-parse-content-streams` may be used.

### Remediation List

The following table lists each mechanically-verifiable test from the
Matterhorn protocol. For each, we give the number, description from the
Matterhorn protocol, and the reference into the PDF/UA standard. Then we
describe, if possible, how to use Cpdf to remediate the failure.
Sometimes this is a definitive command, sometimes a last-ditch attempt
to re-process the file (to embed missing fonts or correct font
structures, for example) and sometimes simply a direction to try the
manual remediation procedure described above.

+--------------------------+-------------------------------+-----------------------------+
| **[Number]{.smallcaps}** | **[Description]{.smallcaps}** | **[Reference]{.smallcaps}** |
+:=========================+:==============================+:============================+
| **01-003**               | **Content marked as Artifact  | **UA1:7.1-1**               |
|                          | is present inside tagged      |                             |
|                          | content.**                    |                             |
+--------------------------+-------------------------------+-----------------------------+
| **01-004**               | **Tagged content is present   | **UA1:7.1-1**               |
|                          | inside content marked as      |                             |
|                          | Artifact.**                   |                             |
+--------------------------+-------------------------------+-----------------------------+
| **01-005**               | **Content is neither marked   | **UA1:7-1-2**               |
|                          | as Artifact nor tagged as     |                             |
|                          | real content.**               |                             |
+--------------------------+-------------------------------+-----------------------------+
| File does not meet Tagged PDF standard - only manual remediation possible (see         |
| description above this table).                                                         |
+--------------------------+-------------------------------+-----------------------------+
| **01-007**               | **Suspects entry has a value  | **UA1:7-1-11**              |
|                          | of true.**                    |                             |
+--------------------------+-------------------------------+-----------------------------+
| If you are sure the file conforms to tagged PDF conventions, use                       |
| `cpdf -replace-obj /Root/MarkInfo/Suspects=false in.pdf -o out.pdf`.                   |
+--------------------------+-------------------------------+-----------------------------+
| **02-001**               | **One or more non-standard    | **UA1:7.1-3**               |
|                          | tag's mapping does not        |                             |
|                          | terminate with a standard     |                             |
|                          | type.**                       |                             |
+--------------------------+-------------------------------+-----------------------------+
| **02-003**               | **A circular mapping          | **UA1:7.1-3**               |
|                          | exists.**                     |                             |
+--------------------------+-------------------------------+-----------------------------+
| **02-004**               | **One or more standard types  | **UA1:7.1-4**               |
|                          | are remapped.**               |                             |
+--------------------------+-------------------------------+-----------------------------+
| File does not meet PDF/UA tagging standard - only manual remediation possible (see     |
| description above this table).                                                         |
+--------------------------+-------------------------------+-----------------------------+
| **06-001**               | **Document does not contain   | **UA1:7.1-8**               |
|                          | an XMP metadata stream**      |                             |
+--------------------------+-------------------------------+-----------------------------+
| Create XMP metadata from any existing old-style metadata in the file with              |
| `cpdf -create-metadata in.pdf -o out.pdf`. This may lead to further verification       |
| errors due to empty metadata entries.                                                  |
+--------------------------+-------------------------------+-----------------------------+
| **06-002**               | **The XMP metadata stream in  | **UA1:5**                   |
|                          | the Catalog dictionary does   |                             |
|                          | not include the PDF/UA        |                             |
|                          | identifier.**                 |                             |
+--------------------------+-------------------------------+-----------------------------+
| Mark the file as PDF/UA using                                                          |
| `cpdf -mark-as ["PDF/UA-1" | "PDF/UA-2"] in.pdf -o out.pdf`.                           |
+--------------------------+-------------------------------+-----------------------------+
| **06-003**               | **XMP metadata stream does    | **UA1:7.1-8**               |
|                          | not contain dc:title**        |                             |
+--------------------------+-------------------------------+-----------------------------+
| Add a title using `cpdf -set-title "My title" -also-set-xmp in.pdf -o out.pdf`.        |
+--------------------------+-------------------------------+-----------------------------+
| **07-001**               | **ViewerPreferences           | **UA1:7.1-9**               |
|                          | dictionary of the Catalog     |                             |
|                          | dictionary does not contain a |                             |
|                          | DisplayDocTitle entry**       |                             |
+--------------------------+-------------------------------+-----------------------------+
| Add the entry with `cpdf -display-doc-title true in.pdf -o out.pdf`.                   |
+--------------------------+-------------------------------+-----------------------------+
| **07-002**               | **ViewerPreferences           | **UA1:7.1-9**               |
|                          | dictionary of the Catalog     |                             |
|                          | dictionary contains a         |                             |
|                          | DisplayDocTitle entry with a  |                             |
|                          | value of false**              |                             |
+--------------------------+-------------------------------+-----------------------------+
| Replace the entry with `cpdf -display-doc-title true in.pdf -o out.pdf`.               |
+--------------------------+-------------------------------+-----------------------------+
| **09-004**               | **A table-related structure   | **UA1-7.2-1**               |
|                          | element is used in a way that |                             |
|                          | does not conform to the       |                             |
|                          | syntax defined in ISO         |                             |
|                          | 32000-1, Table 337.**         |                             |
+--------------------------+-------------------------------+-----------------------------+
| **09-005**               | **A list-related structure    | **UA1-7.2-1**               |
|                          | element is used in a way that |                             |
|                          | does not conform to Table 336 |                             |
|                          | in ISO 32000-1.**             |                             |
+--------------------------+-------------------------------+-----------------------------+
| **09-006**               | **A TOC-related structure     | **UA1-7.2-1**               |
|                          | element is used in a way that |                             |
|                          | does not conform to Table 333 |                             |
|                          | in ISO 32000-1.**             |                             |
+--------------------------+-------------------------------+-----------------------------+
| **09-007**               | **A Ruby-related structure    | **UA1-7.2-1**               |
|                          | element is used in a way that |                             |
|                          | does not conform to Table 338 |                             |
|                          | in ISO 32000-1.**             |                             |
+--------------------------+-------------------------------+-----------------------------+
| **09-008**               | **A Warichu-related structure | **UA1-7.2-1**               |
|                          | element is used in a way that |                             |
|                          | does not conform to Table 338 |                             |
|                          | in ISO 32000-1.**             |                             |
+--------------------------+-------------------------------+-----------------------------+
| File does not meet PDF/UA tagging standard - only manual remediation possible (see     |
| description above this table).                                                         |
+--------------------------+-------------------------------+-----------------------------+
| **10-001**               | **Character code cannot be    | **UA1:7.2-2**               |
|                          | mapped to Unicode.**          |                             |
+--------------------------+-------------------------------+-----------------------------+
| It is possible that reprocessing the file with `gs` using                              |
| `cpdf in.pdf -gs gs -gs-malformed-force -o out.pdf [-gs-quiet]` will correct the       |
| fonts.                                                                                 |
+--------------------------+-------------------------------+-----------------------------+
| **11-001**               | **Natural language for text   | **UA1:7.2-3**               |
|                          | in page content cannot be     |                             |
|                          | determined.**                 |                             |
+--------------------------+-------------------------------+-----------------------------+
| **11-002**               | **Natural language for text   | **UA1:7.2-3**               |
|                          | in Alt, ActualText and E      |                             |
|                          | attributes cannot be          |                             |
|                          | determined.**                 |                             |
+--------------------------+-------------------------------+-----------------------------+
| **11-003**               | **Natural language in the     | UA1:7.2-3                   |
|                          | Outline entries cannot be     |                             |
|                          | determined.**                 |                             |
+--------------------------+-------------------------------+-----------------------------+
| **11-004**               | **Natural language in the     | **UA1:7.2-3**               |
|                          | Contents entry for            |                             |
|                          | annotations cannot be         |                             |
|                          | determined.**                 |                             |
+--------------------------+-------------------------------+-----------------------------+
| **11-005**               | **Natural language in the TU  | **UA1:7.2-3**               |
|                          | entry for form fields cannot  |                             |
|                          | be determined.**              |                             |
+--------------------------+-------------------------------+-----------------------------+
| **11-006**               | **Natural language for        | **UA1:7.2-3**               |
|                          | document metadata cannot be   |                             |
|                          | determined.**                 |                             |
+--------------------------+-------------------------------+-----------------------------+
| Assuming the document is all in a single language, set the top-level language with,    |
| for example, `cpdf -set-language "en-US" in.pdf -o out.pdf`. If the document contains  |
| multiple languages, only manual remediation is possible.                               |
+--------------------------+-------------------------------+-----------------------------+
| **13-004**               | **\<Figure\> tag alternative  | **UA1:7.3-3**               |
|                          | or replacement text           |                             |
|                          | missing.**                    |                             |
+--------------------------+-------------------------------+-----------------------------+
| **14-002**               | **Does use numbered headings, | **UA1:7.4.2-1**             |
|                          | but the first heading tag is  |                             |
|                          | not \<H1\>.**                 |                             |
+--------------------------+-------------------------------+-----------------------------+
| **14-003**               | **Numbered heading levels in  | **UA1:7.4-1**               |
|                          | descending sequence are       |                             |
|                          | skipped (Example:             |                             |
|                          | \<H3\> follows directly after |                             |
|                          | \<H1\>).**                    |                             |
+--------------------------+-------------------------------+-----------------------------+
| **14-006**               | **A node contains more than   | **UA1:7.4.4-1**             |
|                          | one \<H\> tag.**              |                             |
+--------------------------+-------------------------------+-----------------------------+
| **14-007**               | **Document uses both          | **UA1:7.4.4-3**             |
|                          | \<H\> and \<H#\> tags.**      |                             |
+--------------------------+-------------------------------+-----------------------------+
| **15-003**               | **In a table not organized    | **UA1:7.5-2**               |
|                          | with Headers attributes and   |                             |
|                          | IDs, a \<TH\> cell does not   |                             |
|                          | contain a Scope attribute.**  |                             |
+--------------------------+-------------------------------+-----------------------------+
| **17-002**               | **\<Formula\> tag is missing  | **UA1:7.7-1**               |
|                          | an Alt attribute.**           |                             |
+--------------------------+-------------------------------+-----------------------------+
| File does not meet PDF/UA standard - only manual remediation possible (see description |
| above this table). Alternatively, edit the tree manually using `-extract-struct-tree`  |
| and `-replace-struct-tree` from this chapter.                                          |
+--------------------------+-------------------------------+-----------------------------+
| **17-003**               | **Unicode mapping             | **UA1:7.7-2**               |
|                          | requirements are not met.**   |                             |
+--------------------------+-------------------------------+-----------------------------+
| It is possible that reprocessing the file with `gs` using                              |
| `cpdf in.pdf -gs gs -gs-malformed-force -o out.pdf [-gs-quiet]` will correct the       |
| fonts.                                                                                 |
+--------------------------+-------------------------------+-----------------------------+
| **19-003**               | **ID entry of the             | **UA1:7.9-2**               |
|                          | \<Note\> tag is not           |                             |
|                          | present.**                    |                             |
+--------------------------+-------------------------------+-----------------------------+
| **19-004**               | **ID entry of the             | **UA1:7.9-2**               |
|                          | \<Note\> tag is non-unique.** |                             |
+--------------------------+-------------------------------+-----------------------------+
| **20-001**               | **Name entry is missing or    | **UA1:7.10-1**              |
|                          | has an empty string as its    |                             |
|                          | value in an Optional Content  |                             |
|                          | Configuration Dictionary in   |                             |
|                          | the Configs entry in the      |                             |
|                          | OCProperties entry in the     |                             |
|                          | Catalog dictionary.**         |                             |
+--------------------------+-------------------------------+-----------------------------+
| **20-002**               | **Name entry is missing or    | **UA1:7.10-1**              |
|                          | has an empty string as its    |                             |
|                          | value in an Optional Content  |                             |
|                          | Configuration Dictionary that |                             |
|                          | is the value of the D entry   |                             |
|                          | in the OCProperties entry in  |                             |
|                          | the Catalog dictionary.**     |                             |
+--------------------------+-------------------------------+-----------------------------+
| File does not meet PDF/UA standard - only manual remediation possible (see description |
| above this table). Alternatively, edit the tree manually using `-extract-struct-tree`  |
| and `-replace-struct-tree` from this chapter.                                          |
+--------------------------+-------------------------------+-----------------------------+
| **20-003**               | **An AS entry appears in an   | **UA1:7.10-2**              |
|                          | Optional Content              |                             |
|                          | Configuration Dictionary.**   |                             |
+--------------------------+-------------------------------+-----------------------------+
| File does not meet PDF/UA standard - only manual remediation possible (see description |
| above this table).                                                                     |
+--------------------------+-------------------------------+-----------------------------+
| **21-001**               | **The file specification      | **UA1:7.11-1**              |
|                          | dictionary for an embedded    |                             |
|                          | file does not contain F and   |                             |
|                          | UF entries.**                 |                             |
+--------------------------+-------------------------------+-----------------------------+
| File does not meet PDF/UA standard - only manual remediation possible (see description |
| above this table). Alternatively, edit the tree manually using `-extract-struct-tree`  |
| and `-replace-struct-tree` from this chapter.                                          |
+--------------------------+-------------------------------+-----------------------------+
| **25-001**               | **File contains the           | **UA1:7.15-1**              |
|                          | dynamicRender element with    |                             |
|                          | value "required".**           |                             |
+--------------------------+-------------------------------+-----------------------------+
| Not remediable, unless actually a wrong marker. This is an interactive PDF form which  |
| likely only works with Adobe Acrobat. If the marker is actually wrong, it may be       |
| edited manually inside the XML stream using the instructions above.                    |
+--------------------------+-------------------------------+-----------------------------+
| **26-001**               | **The file is encrypted but   | **UA1:7.16-1**              |
|                          | does not contain a P entry in |                             |
|                          | its encryption dictionary.**  |                             |
+--------------------------+-------------------------------+-----------------------------+
| **26-002**               | **The file is encrypted and   | **UA1:7.16-1**              |
|                          | does contain a P entry but    |                             |
|                          | the 10th bit position of the  |                             |
|                          | P entry is false.**           |                             |
+--------------------------+-------------------------------+-----------------------------+
| Re-encrypt the file with Cpdf as described in Chapter 4.                               |
+--------------------------+-------------------------------+-----------------------------+
| **28-002**               | **An annotation, other than   | **UA1:7.18.1-2**            |
|                          | of subtype Widget, Link and   |                             |
|                          | PrinterMark, is not a direct  |                             |
|                          | child of an                   |                             |
|                          | \<Annot\> structure           |                             |
|                          | element.**                    |                             |
+--------------------------+-------------------------------+-----------------------------+
| **28-004**               | **An annotation, other than   | **UA1:7.18.1-4**            |
|                          | of subtype Widget, does not   |                             |
|                          | have a Contents entry and     |                             |
|                          | does not have an alternative  |                             |
|                          | description (in the form of   |                             |
|                          | an Alt entry in the enclosing |                             |
|                          | structure element).**         |                             |
+--------------------------+-------------------------------+-----------------------------+
| **28-005**               | **A form field does not have  | **UA1:7.18.1-4**            |
|                          | a TU entry and does not have  |                             |
|                          | an alternative description    |                             |
|                          | (in the form of an Alt entry  |                             |
|                          | in the enclosing structure    |                             |
|                          | element).**                   |                             |
+--------------------------+-------------------------------+-----------------------------+
| File does not meet PDF/UA standard - only manual remediation possible (see description |
| above this table). Alternatively, edit the tree manually using `-extract-struct-tree`  |
| and `-replace-struct-tree` from this chapter.                                          |
+--------------------------+-------------------------------+-----------------------------+
| **28-006**               | **An annotation with subtype  | **UA1:7.18.2-1**            |
|                          | undefined in ISO 32000 does   |                             |
|                          | not meet 7.18.1.**            |                             |
+--------------------------+-------------------------------+-----------------------------+
| **28-007**               | **An annotation of subtype    | **UA1:7.18.2-2**            |
|                          | TrapNet exists.**             |                             |
+--------------------------+-------------------------------+-----------------------------+
| **28-008**               | **A page containing an        | **UA1:7.18.3-1**            |
|                          | annotation does not contain a |                             |
|                          | Tabs entry**                  |                             |
+--------------------------+-------------------------------+-----------------------------+
| **28-009**               | **A page containing an        | **UA1:7.18.3-1**            |
|                          | annotation has a Tabs entry   |                             |
|                          | with a value other than S.**  |                             |
+--------------------------+-------------------------------+-----------------------------+
| If annotations are not required, they may be removed with                              |
| `cpdf -remove-annotations in.pdf -o out.pdf`. Alternatively, use                       |
| `-list-annotations-json` and `-set-annotations` as described in Chapter                |
| [10](#chap:10){reference-type="ref" reference="chap:10"} to remove one or more         |
| specific annotations.                                                                  |
+--------------------------+-------------------------------+-----------------------------+
| **28-010**               | **A widget annotation is not  | **UA1:7.18.4-1**            |
|                          | nested within a               |                             |
|                          | \<Form\> tag.**               |                             |
+--------------------------+-------------------------------+-----------------------------+
| **28-011**               | **A link annotation is not    | **UA1:7.18.5-1**            |
|                          | nested within a               |                             |
|                          | \<Link\> tag.**               |                             |
+--------------------------+-------------------------------+-----------------------------+
| If annotations are not required, they may be removed with                              |
| `cpdf -remove-annotations in.pdf -o out.pdf`. Alternatively, use                       |
| `-list-annotations-json` and `-set-annotations` as described in Chapter                |
| [10](#chap:10){reference-type="ref" reference="chap:10"} to remove one or more         |
| specific annotations. Alternatively, edit the tree manually using                      |
| `-extract-struct-tree` and `-replace-struct-tree` from this chapter.                   |
+--------------------------+-------------------------------+-----------------------------+
| **28-012**               | **A link annotation does not  | **UA1:7.18.5-2**            |
|                          | include an alternate          |                             |
|                          | description in its Contents   |                             |
|                          | entry.**                      |                             |
+--------------------------+-------------------------------+-----------------------------+
| **28-014**               | **CT entry is missing from    |                             |
|                          | the media clip data           |                             |
|                          | dictionary.**                 |                             |
+--------------------------+-------------------------------+-----------------------------+
| **28-015**               | **Alt entry is missing from   | **UA1:7.18.6.2-1**          |
|                          | the media clip data           |                             |
|                          | dictionary.**                 |                             |
+--------------------------+-------------------------------+-----------------------------+
| **28-016**               | **File attachment annotations | **UA1:7.18.7-1**            |
|                          | do not conform to 7.11.**     |                             |
+--------------------------+-------------------------------+-----------------------------+
| If annotations are not required, they may be removed with                              |
| `cpdf -remove-annotations in.pdf -o out.pdf`. Alternatively, use                       |
| `-list-annotations-json` and `-set-annotations` as described in Chapter                |
| [10](#chap:10){reference-type="ref" reference="chap:10"} to remove one or more         |
| specific annotations.                                                                  |
+--------------------------+-------------------------------+-----------------------------+
| **28-017**               | **A PrinterMark annotation is | **UA1:7.18.8-1**            |
|                          | included in the logical       |                             |
|                          | structure.**                  |                             |
+--------------------------+-------------------------------+-----------------------------+
| If annotations are not required, they may be removed with                              |
| `cpdf -remove-annotations in.pdf -o out.pdf`. Alternatively, use                       |
| `-list-annotations-json` and `-set-annotations` as described in Chapter                |
| [10](#chap:10){reference-type="ref" reference="chap:10"} to remove one or more         |
| specific annotations. Alternatively, edit the tree manually using                      |
| `-extract-struct-tree` and `-replace-struct-tree` from this chapter.                   |
+--------------------------+-------------------------------+-----------------------------+
| **28-018**               | **The appearance stream of a  | **UA1:7.18.8-2**            |
|                          | PrinterMark annotation is not |                             |
|                          | marked as Artifact.**         |                             |
+--------------------------+-------------------------------+-----------------------------+
| If annotations are not required, they may be removed with                              |
| `cpdf -remove-annotations in.pdf -o out.pdf`. Alternatively, use                       |
| `-list-annotations-json` and `-set-annotations` as described in Chapter                |
| [10](#chap:10){reference-type="ref" reference="chap:10"} to remove one or more         |
| specific annotations.                                                                  |
+--------------------------+-------------------------------+-----------------------------+
| **30-001**               | **A reference XObject is      | **UA1:7.2**                 |
|                          | present.**                    |                             |
+--------------------------+-------------------------------+-----------------------------+
| A reference XObject references a page in another file. May be cut out manually using   |
| the manual remediation instructions above.                                             |
+--------------------------+-------------------------------+-----------------------------+
| **30-002**               | **Form XObject contains MCIDs | **UA1:7.21.3.1-1**          |
|                          | and is referenced more than   |                             |
|                          | once.**                       |                             |
+--------------------------+-------------------------------+-----------------------------+
| Unlikely to be remediable: the only option is to manually remove them, but this would  |
| then result in a tag tree pointing to non-existent MCIDs, which would be another kind  |
| of invalidity. Any PDF producer creating Tagged PDF with MCIDs like this is simply     |
| broken.                                                                                |
+--------------------------+-------------------------------+-----------------------------+
| **31-001**               | **A Type 0 font dictionary    | **UA1:7.21.3-1**            |
|                          | with encoding other than      |                             |
|                          | Identity-H and Identity-V has |                             |
|                          | values for Registry in both   |                             |
|                          | CIDSystemInfo dictionaries    |                             |
|                          | that are not identical.**     |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-002**               | **A Type 0 font dictionary    | **UA1:7.21.3.1-1**          |
|                          | with encoding other than      |                             |
|                          | Identity-H and Identity-V has |                             |
|                          | values for Ordering in both   |                             |
|                          | CIDSystemInfo dictionaries    |                             |
|                          | that are not identical.**     |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-003**               | **A Type 0 font dictionary    | **UA1:7.21.3.1-1**          |
|                          | with encoding other than      |                             |
|                          | Identity-H and Identity-V has |                             |
|                          | a value for Supplement in the |                             |
|                          | CIDSystemInfo dictionary of   |                             |
|                          | the CID font that is less     |                             |
|                          | than the value for Supplement |                             |
|                          | in the CIDSystemInfo          |                             |
|                          | dictionary of the CMap.**     |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-004**               | **A Type 2 CID font contains  | **UA1:7.21.3.2-1**          |
|                          | neither a stream nor the name |                             |
|                          | Identity as the value of the  |                             |
|                          | CIDToGIDMap entry.**          |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-005**               | **A Type 2 CID font does not  | **UA1:7.21.3.2-1**          |
|                          | contain a CIDToGIDMap         |                             |
|                          | entry.**                      |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-006**               | **A CMap is neither listed as | **UA1:7.21.3.3-1**          |
|                          | described in ISO 32000-       |                             |
|                          | 1:2008, 9.7.5.2, Table 118    |                             |
|                          | nor is it embedded.**         |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-007**               | **The WMode entry in a CMap   | **UA1:7.21.3.3-1**          |
|                          | dictionary is not identical   |                             |
|                          | to the WMode value in the     |                             |
|                          | CMap stream.**                |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-008**               | **A CMap references another   | **UA1:7.21.3.3-2**          |
|                          | CMap which is not listed in   |                             |
|                          | ISO 32000-1:2008, 9.7.5.2,    |                             |
|                          | Table 118.**                  |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-009**               | **For a font used by text     | **UA1:7.21.4.1-1**          |
|                          | intended to be rendered the   |                             |
|                          | font program is not           |                             |
|                          | embedded.**                   |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-011**               | **For a font used by text the | **UA1:7.21.4.1-3**          |
|                          | font program is embedded but  |                             |
|                          | it does not contain glyphs    |                             |
|                          | for all of the glyphs         |                             |
|                          | referenced by the text used   |                             |
|                          | for rendering.**              |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-012**               | **The FontDescriptor          | **UA1:7.21.4.2-1**          |
|                          | dictionary of an embedded     |                             |
|                          | Type 1 font contains a        |                             |
|                          | CharSet string, but at least  |                             |
|                          | one of the glyphs present in  |                             |
|                          | the font program is not       |                             |
|                          | listed in the CharSet         |                             |
|                          | string.**                     |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-013**               | **The FontDescriptor          | **UA1:7.21.4.2-2**          |
|                          | dictionary of an embedded     |                             |
|                          | Type 1 font contains a        |                             |
|                          | CharSet string, but at least  |                             |
|                          | one of the glyphs listed in   |                             |
|                          | the CharSet string is not     |                             |
|                          | present in the font           |                             |
|                          | program.**                    |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-014**               | **The FontDescriptor          | **UA1:7.21.4.2-3**          |
|                          | dictionary of an embedded CID |                             |
|                          | font contains a CIDSet        |                             |
|                          | string, but at least one of   |                             |
|                          | the glyphs present in the     |                             |
|                          | font program is not listed in |                             |
|                          | the CIDSet string.**          |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-015**               | **The FontDescriptor          | **UA1:7.21.4.2-4**          |
|                          | dictionary of an embedded CID |                             |
|                          | font contains a CIDSet        |                             |
|                          | string, but at least one of   |                             |
|                          | the glyphs listed in the      |                             |
|                          | CIDSet string is not present  |                             |
|                          | in the font program.**        |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-016**               | **For one or more glyphs, the | **UA1:7.21.5-1**            |
|                          | glyph width information in    |                             |
|                          | the font dictionary and in    |                             |
|                          | the embedded font program     |                             |
|                          | differ by more than 1/1000    |                             |
|                          | unit.**                       |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-017**               | **A non-symbolic TrueType     | **UA1:7.21.6-1**            |
|                          | font is used for rendering,   |                             |
|                          | but none of the cmap entries  |                             |
|                          | in the embedded font program  |                             |
|                          | is a non-symbolic cmap.**     |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-018**               | **A non-symbolic TrueType     | **UA1:7.21.6-2**            |
|                          | font is used for rendering,   |                             |
|                          | but for at least one glyph to |                             |
|                          | be rendered the glyph cannot  |                             |
|                          | be looked up by any of the    |                             |
|                          | non-symbolic cmap entries in  |                             |
|                          | the embedded font program.**  |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-019**               | **The font dictionary for a   | **UA1:7.21.6-3**            |
|                          | non-symbolic TrueType font    |                             |
|                          | does not contain an Encoding  |                             |
|                          | entry.**                      |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-020**               | **The font dictionary for a   | **UA1:7.21.6-4**            |
|                          | non-symbolic TrueType font    |                             |
|                          | contains an Encoding          |                             |
|                          | dictionary which does not     |                             |
|                          | contain a BaseEncoding        |                             |
|                          | entry.**                      |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-021**               | **The value for either the    | **UA1:7.21.6-5**            |
|                          | Encoding entry or the         |                             |
|                          | BaseEncoding entry in the     |                             |
|                          | Encoding dictionary in a      |                             |
|                          | non-symbolic TrueType font    |                             |
|                          | dictionary is neither         |                             |
|                          | MacRomanEncoding nor          |                             |
|                          | WinAnsiEncoding.**            |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-022**               | **The Differences array in    | **UA1:7.21.6-6**            |
|                          | the Encoding entry in a       |                             |
|                          | non-symbolic TrueType font    |                             |
|                          | dictionary contains one or    |                             |
|                          | more glyph names which are    |                             |
|                          | not listed in the Adobe Glyph |                             |
|                          | List.**                       |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-023**               | **The Differences array is    | **UA1:7.21.6-7**            |
|                          | present in the Encoding entry |                             |
|                          | in a non-symbolic TrueType    |                             |
|                          | font dictionary but the       |                             |
|                          | embedded font program does    |                             |
|                          | not contain a (3,1) Microsoft |                             |
|                          | Unicode cmap.**               |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-024**               | **The Encoding entry is       | **UA1:7.21.6-8**            |
|                          | present in the font           |                             |
|                          | dictionary for a symbolic     |                             |
|                          | TrueType font.**              |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-025**               | **The embedded font program   | **UA1:7.21.6-9**            |
|                          | for a symbolic TrueType font  |                             |
|                          | contains no cmap.**           |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-026**               | **The embedded font program   | **UA1:7.21.6-10**           |
|                          | for a symbolic TrueType font  |                             |
|                          | contains more than one cmap,  |                             |
|                          | but none of the cmap entries  |                             |
|                          | is a (3,0) Microsoft Symbol   |                             |
|                          | cmap.**                       |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-027**               | **A font dictionary does not  | **UA1:7.21.7-1**            |
|                          | contain the ToUnicode entry   |                             |
|                          | and none of the following is  |                             |
|                          | true: the font uses           |                             |
|                          | MacRomanEncoding,             |                             |
|                          | MacExpertEncoding or          |                             |
|                          | WinAnsiEncoding; the font is  |                             |
|                          | a Type 1 or Type 3 font and   |                             |
|                          | the glyph names of the glyphs |                             |
|                          | referenced are all contained  |                             |
|                          | in the Adobe Glyph List or    |                             |
|                          | the set of named characters   |                             |
|                          | in the Symbol font, as        |                             |
|                          | defined in ISO 32000-1:2008,  |                             |
|                          | Annex D; the font is a Type 0 |                             |
|                          | font, and its descendant      |                             |
|                          | CIDFont uses Adobe-GB1,       |                             |
|                          | Adobe-CNS1, Adobe-Japan1 or   |                             |
|                          | Adobe-Korea1 character        |                             |
|                          | collections; the font is a    |                             |
|                          | non-symbolic TrueType font.** |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-028**               | **One or more Unicode values  | **UA1:7.21.7-2**            |
|                          | specified in the ToUnicode    |                             |
|                          | CMap are zero (0).**          |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-029**               | **One or more Unicode values  | **UA1:7.21.7-3**            |
|                          | specified in the ToUnicode    |                             |
|                          | CMap are equal to either      |                             |
|                          | U+FEFF or U+FFFE.**           |                             |
+--------------------------+-------------------------------+-----------------------------+
| **31-030**               | **One or more characters used | **UA1:7.21.8-1**            |
|                          | in text showing operators     |                             |
|                          | reference the .notdef         |                             |
|                          | glyph.**                      |                             |
+--------------------------+-------------------------------+-----------------------------+
| It is possible that reprocessing the file with `gs` using                              |
| `cpdf in.pdf -gs gs -gs-malformed-force -o out.pdf [-gs-quiet]` will correct the       |
| fonts.                                                                                 |
+----------------------------------------------------------------------------------------+
