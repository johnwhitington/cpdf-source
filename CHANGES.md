# Changes

## 2.9.3 (October 2026)

### New features

- Remove search index with -remove-search-index *
- Add -annot-type to -list-annotations-json to filter *
- Remove all optional content with -ocg-remove *
- Remove marked content with -remove-marked-content *
- Optionally apply appearance of a redaction annotation *
- Remove AcroForm with -remove-acroform *
- Remove bookmarks and links to redacted content *
- Flatten annotations with -flatten-annotations *

### Extended features

- Erase or filter text sections post-redaction *
- Add /NM, /M and /QuadPoints to redaction annotations *
- More finely controlled redaction with 'covers' specification *
- Create and style overlay text for redaction annotations *
- Text styling (e.g colours) works for -typeset, -toc etc. *
- Set margin for -typeset with -margin *

\* = Supported by a grant from NLnet

## 2.9.2 (September 2026)

### New features

- Redact documents with -redact-shape *
- Apply redaction annotations with -redact-apply *
- Apply other annotations as redactions with -redact-apply-type *
- Create redaction annotations with -add-annotation *

### Fixes

- Reprocessing an RGB JPEG could result in a grayscale JPEG, leading to a malformed PDF.

\* = Supported by a grant from NLnet

## 2.9.1 (May 2026)

### New features

- Print number of revisions in a PDF with -revisions
- Choose which revision to load with -revision
- Update a PDF by appending with -update
- Show digital signature information with -sig-info
- Add attached files from JSON data
- Add page labels from JSON data
- List OCGs in JSON form with -ocg-list-json
- Edit & round-trip OCGs with -ocg-replace
- Remove stream content for debug
- Show bounding boxes of page content with -show-bboxes *
- Suppress adjacent identical errors
- Export basic page content as JSON *

### Extended features

- Operations -relative-to-[bleed|art|trim]box as well as crop
- Document all debug functions in -summary
- Rotate text for -add-text using -text-[90|180|270].
- Optionally add a border to URLs in -add-text
- %URL now works with diagonal text
- Identify custom encodings in -list-fonts

### Fixes

- -midline and -topline now work properly for custom fonts
- -summary goes to stdout
- Correct parent links for non-dests in -set-annotations

\* = Supported by a grant from NLnet

## 2.9 (March 2026)

### New features

- Cpdf now has a man page, which abbreviates cpdfmanual.pdf
- More sensible -help (current -help moved to -summary)
- Detect and remove JavaScript
- Show progress on long files
- Create PDF portfolios
- Remove procsets, article threads, page pieces
- Remove web capture, output intents, alternate images
- Reprocess JPEG2000s within files
- Convert lossless images to JPEG2000
- Remove all metadata streams
- Extract all metadata streams to file
- Allow the empty range
- Allow a range specifying some pages which do not exist
- Extract a single image with -extract-single-image

### Extended features

- CCITT and CCITTG4 encoders in -process-images
- Force image processing even when image larger
- -thinlines can ensure maximum as well as minimum thickness
- New -just-content for easier content stream debugging
- -squeeze processes xobjects inside xobjects
- Speed increases for -squeeze on huge files
- Select only such pages as are annotated
- Allow -create-pdf and friends to appear in middle of AND
- -draw can use JPEG2000s
- New -list-attached-files -json optionally including file data
- List and add descriptions and relationships for attachments
- Process lossless CMYK images
- Use 8bpp Alpha PNGs with -rasterize, -output-image, -png and when drawing
- -list-images and friends show mask status, and list masks
- -list-images and friends now list inline images
- -list-images now distinguishes types of CCITT encoding
- -list-images now distinguishes lossy and lossless JBIG2
- Extract inline images
- Extract soft masks
- Optionally combine soft masks with image when extracting images
- Decompress can decompress JBIG2 encoded streams with jbig2dec

### Fixes

- Sanitizes inputs to prevent command injection attacks *
- Fix PMAXX et al. w.r.t -add-rectangle
- Allow Float/Int to appear anywhere in CPDFJSON / bookmark JSON
- Allow -info in the case where objstm reading requires password
- Allow -stdin in -info and in -merge (once)
- Cpdfimages was accidentally creating palettised PNGs
- Page label prefixes now in UTF8
- Regression where bookmarks were being corrupted
- Removes the dependency on wmic for reading the date.
- -pad-after, -pad-before were failing if range not sorted

\* = Supported by a grant from NLnet

## 2.8.1 (April 2025)

### New features

- -remove-struct-tree removes whole structure tree *
- -mark-as-artifact marks a whole file as an artifact *

### Extended features

- -obj JSON output
- -obj/-obj-json can follow a chain from an object number *
- -obj/-obj-json can explore through arrays and name/number trees *
- -print-dict-entry, -remove-dict-entry and -replace-dict-entry can follow a chain from each found dictionary entry *
- Much more of the PDF/UA Matterhorn verification suite implemented *
- PDF/UA verifier now returns extra information with most failures *
- Font lister now returns the font itself *
- Allow %objnum in output name for -extract-images
- -show-boxes, -trim-marks, -table-of-contents, -add-text -add-rectangle, -impose, -twoup, -twoup-stack now preserve PDF/UA and PDF/UA-2 compatibility *
- Preserve PDF/UA and PDF/UA-2 when drawing on existing PDF *
- Preserve PDF/UA and PDF/UA-2 with -stamp-on and -combine-pages *
- Allow -scale-stamp-to-fit for -combine-pages
- Allow -underneath for -combine-pages *
- Preserve roundtripping of named destinations / actions *
- -preserve-actions allows PDF/UA-2 style roundtripping of outlines *

### Fixes

- Harden auto-compression against malformed streams
- Add backup JPEG dimensions method in Cpdfimage
- Correct object stream behaviour when splitting
- Fix margin calculation for -table-of-contents

\* = Supported by a grant from NLnet

## 2.8 (December 2024)

### New features

- New -center-to-fit centres pages on a given paper size
- New -jpeg-to-jpeg-scale and -jpeg-to-jpeg-dpi
- Rasterize PDFs by calling out to GhostScript
- Extract pages as PNG/JPEG by calling out to GhostScript
- Replace stream content with -replace-stream

### Extended features

- Expand page characteristics to cover Art, Trim, Bleed
- Add Piece Info to -composition[-json]
- Add `@b<n>@` for trimming bookmark text to given length
- Allow bold, italic, colours for JSON bookmarks
- Show OpenAction in -info
- Show more form information in -info
- Allow JSON / PDF syntax in dict processing and object exploration
- Show %Bookmark text when stamping text
- Change units for -info[-json] and -page-info[-json]
- Optionally add dot leaders to tables of content
- Add -collate-n to extend -collate to multiple pages at once

### Fixes

- Clean up @B implementation for -split-on-bookmarks
- -merge-add-bookmarks now has proper titles for images
- Font operations now include fonts within xobjects
- Image extraction now includes images within xobjects within xobjects
- HTML manual now ranks equally with PDF manual *

\* = Supported by a grant from NLnet

## 2.7.2 (October 2024)

### New features

- New -args-json for better control files *
- New -replace-obj to edit dictionary entries via chain *
- Create PDF/UA files from scratch with -create-pdf-ua-[1|2] *
- Create structure information for files with -draw *
- Draw can now make paragraphs with -para, -paras *
- Add structure information to -typeset *
- -typeset can make PDF/UA documents *
- -jpeg, -png and friends can make PDF/UA documents *
- Merge PDF/UA-1 and PDF/UA-2 files, preserving standards compliance *
- -stretch scales without preserving aspect ratio
- -redact removes whole pages

### Extended features

- Rectangles may be specified as x y x' y' rather than x y w h

### Fixes

- Removed setting of Producer field in AGPL version
- Removed long-deprecated -control *
- More compact -print-struct-tree *
- -image-resolution-json was listing all images

\* = Supported by a grant from NLnet

## 2.7.1 (July 2024)

### New features

- Build PDF files from JPEG2000 (.jp2, .jpf/.jpx) files
- Mark a file as PDF/UA compliant, or remove such marking *
- Partial verification to PDF/UA via the Matterhorn protocol *
- Extract, edit and reapply document structure tree *
- Print structure tree summary *
- Split structure tree when splitting PDF to save size *
- Combine structure trees when merging or stamping PDFs *
- Set the natural language of a file *

### Extended features

- Allow -obj to look up nested PDF information
- Merge structure trees better when merging files *
- Report top-level natural language on -info *
- Report mark information dictionary contents on -info *

### Backward-incompatible change

- -process-struct-tree replaces -no-process-struct-tree newly introduced in previous version *

\* = Supported by a grant from NLnet

## 2.7 (February 2024)

### New features

- Split files to max size with -split-max
- Spray splits a file to multiple outputs by alternating pages
- List document and page info in JSON format
- List page labels in JSON format
- List fonts in JSON format
- Identify PDF/A, PDF/X, PDF/E, PDF/VT, PDF/UA in -info
- Identify AcroForm in -info
- Extract font files from a document
- List images on a page with -list-images[-json]
- Chop pages up into sections with -chop
- Build PDF files from JBIG2 streams, including globals
- Reprocess images within PDFs to further compress them
- Extract streams to disk
- Explore PDFs by printing objects
- Shift page boxes with -shift-boxes

### Extended features

- -list-images-used[-json] extends -image-resolution
- Use -raw with -extract-images to get PNMs
- -extract-images can extract JBIG2 images and their globals
- More PNGs - greyscale 1, 2, 4, 8, 16bpp and RGB 16bpp
- Report number of annotations in -page-info
- Specify image based only on file extension
- -squeeze updates old compression methods
- Show page size summary in -info
- Add -no-process-struct-trees to prevent merging of structure trees

### Fixes

- Added opam file in-source
- Fixed -set-annotations with page links
- Allow Exif JPEGs as well as JFIF ones in -jpeg and -draw-jpeg
- Only compress a stream if it actually makes it smaller

## 2.6.1 (September 2023)

- Fixed regression in UTF8 text with -add-text

## 2.6 (July 2023)

### New features

- Create new PDF documents or draw on existing ones with -draw
- Embed TrueType fonts with -load-ttf
- Embed the 14 standard fonts if requested
- Add links to parts of text with -add-text as %URL[|]
- Convert JPEGs and PNGs to PDFs with -jpeg and -png
- Export, import, and thereby round-trip annotations via JSON
- Show composition of PDF with -composition[-json]
- Use page labels like `<iii>` and `<A-2>` in page specifications

### Extended features

- Allow -utf8 with -split-bookmarks -o @B.pdf to produce UTF8 filenames
- -merge-add-bookmarks now works with unicode filenames
- Better transformation of some annotation types
- -list-annotations[-json] now respects page range
- Merge now merges structure trees (tagged PDF)
- Merge now rewrites clashing name tree entries
- Preserve /GoTo actions in bookmarks when merging
- UTF8 option for JSON output
- -info now shows object stream, /ID data, page mode and layout
- More options for viewer preference control
- More default colours, by using the CSS colour list

### Fixes

- Updated Yojson to remove dependency on Stream, ready for OCaml 5
- -typeset was opening files in text mode, instead of binary
- Fixed behaviour of -squeeze-no-pagedata / -squeeze-no-recompress
- Significant improvements to malformed file reading
- Allow DUP page specifications to use larger numbers
- Reworked functions transforming pages to better preserve patterns

## 2.5.1 (January 2022)

- Fix a regression where standard fonts could not be chosen

## 2.5 (January 2022)

### New features

- Can read as well as write PDFs in JSON format with -j
- New operation -typeset typesets a text file as a PDF
- New operation -table-of-contents makes table of contents from bookmarks
- New operations -impose and -impose-xy for document imposition
- New operation -print-font-table gives (charcode, unicode, glyph name) triples
- New -print-dict-entry operation prints values for a given key
- New -replace-dict-entry function to search & replace e.g URLs
- Prepend e.g 2DUP to a page range to make 1,2,3 --> 1,1,2,2,3,3 etc.
- Prepend NOT to a page range to invert it
- Output annotations in JSON form with -list-annotations-json
- Output bookmarks in JSON format with -list-bookmarks-json
- Load bookmarks in JSON format with -add-bookmarks-json
- New option -collate to collate pages when merging
- Text added in existing fonts is now encoding-aware (plus new raw mode)

### Extended features

- Extend -remove-dict-entry to allow search
- Annotation /QuadPoints processed in addition to /Rect when transforming pages
- When adding text or graphics, may choose CMYK or Grey instead of RGB
- The -list-fonts operation now obeys the range
- Can now search for a font by real name with -font
- Basic merging of AcroForms when merging documents
- Add -idir-only-pdfs to restrict -idir to just files ending in .pdf
- Option -debug-force now properly documented as -decrypt-force

### Internal changes

- Switch to Yojson for faster/better JSON input/output
- Environment variable CPDF_REPRODUCIBLE_DATES for testing
- Environment variable CPDF_DEBUG for -debug
- Effectively make stderr unbuffered
- Split cpdf.ml into separate modules

## 2.4 (June 2021)

- New operation -extract-images
- New operation -output-json et al. to export PDF files in JSON format
- New operations to manipulate Optional Content Groups
- New operation -stamp-as-xobject to add one PDF as an xobject in another
- Optional Content Groups now preserved when merging / stamping pages
- Listing, coalescing and modifying Optional Content Groups.
- New -labels-progress option to improve page labels interface
- Appearance streams transformed when transforming annotations
- Bookmark destination positions transformed when transforming pages
- No longer depends on Bigarray or Unix modules

## 2.3 (patchlevel 1, December 2019)

- Fixed bug which prevented -info working on encrypted files
- Allow -shift with -add-text for additional adjustment
- Prepend and postpend directly to page content streams

## 2.3 (October 2019)

- Directly set and remove Trim, Art, and Bleed boxes
- Dump attachments to file
- Extended bookmark format, preserving all bookmark information
- New -pad-with, -pad-multiple-before options
- Set or create XMP metadata
- Remove graphics clipping
- Extended support for reading malformed files
- Embed missing fonts by calling out to gs
- Set bookmarks open to a given level
- Create PDF files from scratch
- Remove single images by name
- Add trim marks

## 2.2 (patchlevel 1)

- Fix for inability to read null objects in streams
- Workaround for Adobe "Error 21" on re-saving encrypted files
- More efficient bookmark operations on files with many pages
- New operation -hard-box to clip contents to a given box

## 2.2 (March 2017)

- Perform arithmetic on dimensions when specifying size or position
- Add simple rectangles to PDF to blank things out
- Stamping operations now preserve annotations
- Decryption fully on-demand for speed.
- -keep-l keeps existing linearization status
- -remove-dict-entry to remove the contents of a dictionary entry
- -topline in addition to -midline
- -producer and -creator may be used to alter producer and creator in conjunction with any other operation
- -topline and -midline now apply to stamps
- -list-spot-colours
- -bates-at-range and -bates-pad-to
- -print-page-labels
- -squeeze replaces smpdf
- Preserve more sharing of data when doing merges and page alterations

## 2.1 (December 2014)

- Encryption now much, much faster
- Faster parsing of delayed object streams on large files
- -decompress now leaves object streams decompressed
- Select pages by landscape-ness or portrait-ness in a page range
- New -open-at-page and -open-at-page-fit option to set the open action
- New -recrypt option to re-encrypt output
- Reads XMP metadata and outputs it on -info
- New -center position for text
- -stamp can now use positions, just like stamping text
- Better handling of permissions for files with user passwords
- Linearization excised
- Can encrypt or recrypt output of -split and -split-bookmarks now
- -args replaces -control with better behaviour
- Can scale a stamp to fit before stamping with -scale-stamp-to-fit
