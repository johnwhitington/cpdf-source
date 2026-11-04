<img width="400" height="163" alt="cpdf logo" src="https://github.com/user-attachments/assets/af9e0a36-4bf3-49e0-b7c0-2534e7e1f0d6" />

Coherent PDF Command Line Tools Source
======================================

(Binaries are here https://www.github.com/coherentgraphics/cpdf-binaries or may be downloaded by clicking on "Releases" on this GitHub page.)

Cpdf is a command line tool for processing PDFs:

* Quality Split and Merge, keeping bookmarks. Extract pages. Split on Bookmarks.
* Encrypt and Decrypt (including AES 128 and AES 256 encryption)
* Scale, rotate, crop and flip pages. Scale pages to fit.
* Copy, Remove and Add bookmarks
* Stamp logos, watermarks, page numbers and multiline text. Transparency.
* Embed TrueType fonts when adding text
* Supports Unicode UTF8 text input and output
* Make PDF-based presentations
* Put multiple pages on a single page
* List, copy, set, or remove annotations
* Read and set document information and metadata
* Add and remove file attachments to document or page.
* Thicken hairlines, blacken text, make draft documents
* Reconstruct malformed files
* Detect missing fonts, low resolution images
* Export and import PDF files in JSON format
* Build table of contents
* Convert text to PDF
* Draw on PDF files with graphics and text
* Create and process PDF/UA-1 and PDF/UA-2 files
* Process images to, for example, reduce resolution
* Rasterize PDF files

CPDF is distributed under the AGPL - see LICENSE.md. If you are unable to abide
by the terms of the AGPL, you will need a commercial license.

For commercial licenses, email
[contact@coherentgraphics.co.uk](mailto:contact@coherentgraphics.co.uk) or
visit [https://www.coherentpdf.com/](https://www.coherentpdf.com/)

See also original copyright and/or license statements in cpdfxmlm.ml*,
cpdfyojson.ml*, cpdfua.ml, cpdfunicodedata.*, and compressor. Copyright Coherent Graphics Ltd 2026.

This code builds two things:

a) The CPDF PDF command line tool for editing PDF files.

b) cpdf as an OCaml library, for which documentation is here:

[https://coherentpdf.com/cpdf/](https://coherentpdf.com/cpdf/)

See [BUILDING.md](BUILDING.md) for instructions.

Keep up to date
---------------

Join the very low volume cpdf-announce mailing list:

https://groups.google.com/g/cpdf-announce/

Documentation
-------------

The file [cpdfmanual.pdf](cpdfmanual.pdf) in this folder is the user manual for
the command line tools. The documentation for the cpdf OCaml library is in
`doc/html/cpdf/` in this folder once the library has been built or can be found
online at [https://coherentpdf.com/cpdf](https://coherentpdf.com/cpdf).

Acknowledgments
---------------

The files [cpdfxmlm.ml](cpdfxmlm.ml) and [cpdfuuidm.ml](cpdfuudm.ml) were
written by Daniel Bünzli.

The file [cpdfyojson.ml](cpdfyojson.ml) was written by Martin Jambon and others.

The files [gpc.c](gpc.c) and [gpc.h](gpc.h) were written by Alan Murta.

Unicode Data Files are under the Unicode Inc. License agreement.

The error descriptions for -verify "PDF/UA-1(matterhorn)" come from the PDF
Association, and are licensed under the Creative Commons Attribution 4.0
International license.

PDF/UA support in Cpdf was implemented under NLnet grant 2023-12-031.

Redaction support in Cpdf is being implemented under NLnet grant 2025-10-235.

Contact
-------

[contact@coherentgraphics.co.uk](mailto:contact@coherentgraphics.co.uk)
