# Compression

    cpdf -decompress [-just-content] [-jbig2dec <path>] in.pdf -o out.pdf

    cpdf -compress in.pdf -o out.pdf

    cpdf -squeeze in.pdf [-squeeze-log-to <filename>]  
    [-squeeze-no-recompress] [-squeeze-no-pagedata] -o out.pdf

    cpdf -remove-article-threads in.pdf -o out.pdf 
    cpdf -remove-page-piece in.pdf -o out.pdf  
    cpdf -remove-web-capture in.pdf -o out.pdf  
    cpdf -remove-procsets in.pdf -o out.pdf  
    cpdf -remove-output-intents in.pdf -o out.pdf

Cpdf provides facilities for decompressing and compressing PDF streams,
and for losslessly reprocessing the whole file to 'squeeze' it. For
lossy recompression of images within a PDF, see [Images](images.md).

## Decompressing a Document

To decompress the streams in a PDF file, for instance to manually
inspect the PDF, use:

`cpdf -decompress in.pdf -o out.pdf`

If Cpdf finds a compression type it can't cope with, the stream is left
compressed. To decompress only page content streams, add
`-just-content`.

When using `-decompress`, object streams are not removed. It may be
easier for manual inspection to also remove object streams, by adding
the `-no-preserve-objstm` option to the command.

To decompress JBIG2-compressed streams Cpdf needs the help of the
external `jbig2dec` program. Use `-jbig2dec` to specify it, or leave it
unspecified to leave these streams compressed.

## Compressing a Document

To compress the streams in a PDF file, use:

`cpdf -compress in.pdf -o out.pdf`

Cpdf compresses any streams which have no compression using the
**Flate­Decode** method, with the exception of Metadata streams, which
are left uncompressed.

## Squeezing a Document

To *squeeze* a PDF file, reducing its size by an average of about twenty
percent (though sometimes not at all), use:

`cpdf -squeeze in.pdf -o out.pdf`

Adding `-squeeze` to the command line when using another operation will
*squeeze* the file or files upon output.

The `-squeeze` operation writes some information about the squeezing
process to standard output. The squeezing process involves several
processes which losslessly attempt to reduce the file size. It is slow,
so should not be used without thought.

    $ ./cpdf -squeeze in.pdf -o out.pdf
    Initial file size is 238169 bytes
    Beginning squeeze: 123847 objects
    Squeezing... Down to 114860 objects
    Squeezing... Down to 114842 objects
    Squeezing page data
    Recompressing document
    Final file size is 187200 bytes,  78.60% of original.

The `-squeeze-log-to <filename>` option writes the log to the given file
instead of to standard output. Log content is appended to the end of the
log file, preserving existing contents.

The option `-squeeze-no-pagedata` avoids the reprocessing of page data,
which avoids problems in case of malformed files, and makes the process
much faster at the cost of a little less compression. The option
`-squeeze-no-recompress` is deprecated as of version 2.6 and has no
effect.

## Other Lossless Compression

[Images](images.md) describes `-process-images`, which supports the lossless
re-compression of images within a PDF, depending on the options given.

## Lossy Compression

We have discussed only methods of compression which do not remove
content or metadata. Cpdf also provides some functions for stripping out
unwanted metadata:

`-remove-article-threads`

> Removes article threads, which contain information on the logical
  order of a document's parts.

`-remove-page-piece`

> Removes page-piece information, which is a kind of private PDF
  information found in PDFs saved from image editors or converted from
  other formats.

`-remove-web-capture`

> Removes web capture data, a now-deprecated way of saving additional
  metadata in PDFs originating as web content.

`-remove-procsets`

> Removes ProcSets, a now-irrelevant data structure often in early PDFs

`-remove-output-intents`

> Removes Output Intents, a colour-matching system for documents
  intended to be printed.

For each operation, the command looks like this:

`cpdf -remove-* in.pdf -o out.pdf`

Throughout this manual a number of other functions are described which
could be characterised as lossy compression, depending upon the
circumstance. We list and briefly describe each here for convenience:

`-remove-annotations`

> In [Annotations](annotations.md). Removes annotations from a PDF.

`-remove-metadata`

> In [Document Information and Metdata](document-information-and-metadata.md#xmp-metadata). Remove main XMP metadata.

`-remove-all-metadata`

> In [Document Information and Metdata](document-information-and-metadata.md#xmp-metadata). Remove all XMP metadata.

`-remove-files`

> In [File Attachments](file-attachments.md). Removes attached files from a PDF.

`-process-images`

> In [Images](images.md). Lossy (and lossless) reprocessing of images within PDFs.

`-remove-fonts`

> In [Fonts](fonts.md). Remove embedded fonts from a PDF.

`-remove-all-text`

> In [Miscellaneous](miscellaneous.md). Remove text from a PDF.

`-draft`

> In [Miscellaneous](miscellaneous.md). Remove images from a PDF.

To find out what is in a PDF, use `-composition[-json]` from [Document Information and Metadata](document-information-and-metadata.md).
