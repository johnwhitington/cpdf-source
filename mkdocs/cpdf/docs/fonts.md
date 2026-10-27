# Fonts

cpdf -list-fonts[-json] in.pdf

cpdf -print-font-table <font name> -print-font-table-page <n> in.pdf

cpdf -copy-font fromfile.pdf -copy-font-page <int>
     -copy-font-name <name> in.pdf [<range>] -o out.pdf

cpdf -remove-fonts in.pdf -o out.pdf

cpdf -missing-fonts in.pdf

cpdf -embed-missing-fonts -gs <path to gs> in.pdf -o out.pdf

cpdf -extract-font <page number>,<pdf font name> in.pdf -o out.font

## Listing Fonts

The `-list-fonts` operation prints the fonts in the document, one-per-line to
standard output. For example:

    1 /F245 /Type0 /Cleargothic-Bold /Identity-H
    1 /F247 /Type0 /ClearGothicSerialLight /Identity-H
    1 /F248 /Type1 /Times-Roman /WinAnsiEncoding
    1 /F250 /Type0 /Cleargothic-RegularItalic /Identity-H
    2 /F13 /Type0 /Cleargothic-Bold /Identity-H
    2 /F16 /Type0 /Arial-ItalicMT /Identity-H
    2 /F21 /Type0 /ArialMT /Identity-H
    2 /X02 /F58 /Type1 /Times-Roman /WinAnsiEncoding
    2 /F59 /Type0 /ClearGothicSerialLight /Identity-H
    2 /F61 /Type0 /Cleargothic-BoldItalic /Identity-H
    2 /F68 /Type0 /Cleargothic-RegularItalic /Identity-H
    3 /F47 /Type0 /Cleargothic-Bold /Identity-H
    3 /F49 /Type0 /ClearGothicSerialLight /Identity-H
    3 /F50 /Type1 /Times-Roman /WinAnsiEncoding
    3 /F52 /Type0 /Cleargothic-BoldItalic /Identity-H
    3 /F54 /Type0 /TimesNewRomanPS-BoldItalicMT /Identity-H
    3 /F57 /Type0 /Cleargothic-RegularItalic /Identity-H
    4 /F449 /Type0 /Cleargothic-Bold /Identity-H
    4 /F451 /Type0 /ClearGothicSerialLight /Identity-H
    4 /F452 /Type1 /Times-Roman /WinAnsiEncoding

The first column gives the page number, the second the internal unique
font name (or, if the font is used in a Form XObject, the path e.g
`/X1/F0`), the third the type of font (Type1, TrueType etc), the fourth
the PDF font name, the fifth the PDF font encoding.

The information is also available in JSON format with
`-list-fonts-json`:

    [
      {
        "page": 1,
        "name": "/F47",
        "subtype": "/Type1",
        "basefont": "/XYPLPB+NimbusSanL-Bold",
        "encoding": "Custom"
      },
      {
        "page": 1,
        "name": "/F50",
        "subtype": "/Type0",
        "basefont": "/MCBERL+URWPalladioL-Roma",
        "encoding": "/Identity-H"
      }
    ]

## Listing characters in a font

We can use Cpdf to find out which characters are available in a given
font, and to print the map between character codes, unicode codepoints,
and Adobe glyph names. This is presently a best-effort service, and does
not cover all font/encoding types.

We find the name of the font by using `-list-fonts`:

    $ ./cpdf -list-fonts cpdfmanual.pdf 1
    1 /F46 /Type1 /XYPLPB+NimbusSanL-Bold 
    1 /F49 /Type1 /MCBERL+URWPalladioL-Roma 

We may then print the table, giving either the font's name (e.g `/F46`)
or basename (e.g `/XYPLPB+NimbusSanL-Bold`):

    $ ./cpdf -print-font-table /XYPLPB+NimbusSanL-Bold
             -print-font-table-page 1 cpdfmanual.pdf
    67 = U+0043 (C - LATIN CAPITAL LETTER C) = /C
    68 = U+0044 (D - LATIN CAPITAL LETTER D) = /D
    70 = U+0046 (F - LATIN CAPITAL LETTER F) = /F
    71 = U+0047 (G - LATIN CAPITAL LETTER G) = /G
    76 = U+004C (L - LATIN CAPITAL LETTER L) = /L
    80 = U+0050 (P - LATIN CAPITAL LETTER P) = /P
    84 = U+0054 (T - LATIN CAPITAL LETTER T) = /T
    97 = U+0061 (a - LATIN SMALL LETTER A) = /a
    99 = U+0063 (c - LATIN SMALL LETTER C) = /c
    100 = U+0064 (d - LATIN SMALL LETTER D) = /d
    101 = U+0065 (e - LATIN SMALL LETTER E) = /e
    104 = U+0068 (h - LATIN SMALL LETTER H) = /h
    105 = U+0069 (i - LATIN SMALL LETTER I) = /i
    108 = U+006C (l - LATIN SMALL LETTER L) = /l
    109 = U+006D (m - LATIN SMALL LETTER M) = /m
    110 = U+006E (n - LATIN SMALL LETTER N) = /n
    111 = U+006F (o - LATIN SMALL LETTER O) = /o
    112 = U+0070 (p - LATIN SMALL LETTER P) = /p
    114 = U+0072 (r - LATIN SMALL LETTER R) = /r
    115 = U+0073 (s - LATIN SMALL LETTER S) = /s
    116 = U+0074 (t - LATIN SMALL LETTER T) = /t

The first column is the character code, the second the Unicode
codepoint, the character itself and its Unicode name, and the third the
Adobe glyph name.

## Copying Fonts

In order to use a font other than the standard 14 with `-add-text`, it
must be added to the file. The font source PDF is given, together with
the font's resource name on a given page, and that font is copied to all
the pages in the input file's range, and then written to the output
file.

The font is named in the output file with its basefont name, so it can
be easily used with `-add-text`.

For example, if the file `fromfile.pdf` has a font `/GHLIGA+c128` with
the name `/F10` on page 1 (this information can be found with
`-list-fonts`), the following would copy the font to the file `in.pdf`
on all pages, writing the output to `out.pdf`:

`cpdf -copy-font fromfile.pdf -copy-font-name /F10`\
`     -copy-font-page 1 in.pdf -o out.pdf`

Text in this font can then be added by giving `-font /GHLIGA+c128`. Be
aware that due to the vagaries of PDF font handling concerning which
characters are present in the source font, not all characters may be
available, or Cpdf may not be able to work out the conversion from UTF8
to the font's own encoding. You may add `-raw` to the command line to
avoid any conversion, but the encoding (mapping from input codes to
glyphs) may be non-obvious and require knowledge of the PDF format to
divine.

## Removing Fonts

To remove embedded fonts from a document, use `-remove-fonts`. PDF
readers will substitute local fonts for the missing fonts. The use of
this function is only recommended when file size is the sole
consideration.

`cpdf -remove-fonts in.pdf -o out.pdf`

## Missing Fonts

The `-missing-fonts` operation lists any unembedded fonts in the
document, one per line.

`cpdf -missing-fonts in.pdf`

The format is

`Page number, Name, Subtype, Basefont, Encoding`

The operation `-embed-missing-fonts` will process the file with `gs`
(which must be installed) to embed missing fonts (where found):

`cpdf -embed-missing-fonts -gs gs in.pdf -o out.pdf`

*Note: putting a PDF file through `gs` in this manner may not be
lossless: some metadata may not be preserved.*

## Extracting Fonts

We may extract a font file by giving the page number and the PDF font
resource name, as printed by `-list-fonts` or `-list-fonts-json`. For
example, for the TrueType font `/F50` on page 5:

`cpdf -extract-font 5,/F50 in.pdf -o out.ttf`
