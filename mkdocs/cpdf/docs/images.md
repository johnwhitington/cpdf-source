# Images

cpdf -list-images[-json] [-inline] in.pdf [<range>]

cpdf -image-resolution[-json] <n> [-inline] in.pdf [<range>]

cpdf -list-images-used[-json] [-inline] in.pdf [<range>]

cpdf -extract-images in.pdf [<range>] [-im <path>] [-p2p <path>]
     [-dedup | -dedup-perpage] [-raw] [-inline] [-merge-masks] -o <path>

cpdf -extract-single-image <object number> [-im <path>] [-p2p <path>]
     [-raw] [-merge-masks] in.pdf -o <filename>

cpdf -process-images [-process-images-info] in.pdf [<range>]
     [-process-images-force]
     [-im <path>] [-jbig2enc <path>] [-jbig2dec <path>]
     [-lossless-resample[-dpi] <n> | -lossless-to-jpeg <n>]
     [-jpeg-to-jpeg <n>] [-jpeg-to-jpeg-scale <n>]
     [-lossless-to-jpeg2000 <n>] [-jpeg2000-to-jpeg2000 <n>]
     [-jpeg-to-jpeg-dpi <n>] [-1bpp-method <method>]
     [-jbig2-lossy-threshold <n>] [-pixel-threshold <n>]
     [-length-threshold <n>] [-percentage-threshold <n>]
     [-dpi-threshold <n>] [-resample-interpolate]
     -o out.pdf

cpdf -rasterize in.pdf [<range>] -o out.pdf
     [-rasterize[-gray|-1bpp|-jpeg|-jpeg-gray]
     [-rasterize-res <n>] [-rasterize-jpeg-quality <n>]
     [-rasterize-no-antialias | -rasterize-downsample]
     [-rasterize-annots] | [-rasterize-alpha]

cpdf -output-image in.pdf [<range>] -o <format>
     [-rasterize[-gray|-1bpp|-jpeg|-jpeg-gray]
     [-rasterize-res <n>] [-rasterize-jpeg-quality <n>]
     [-rasterize-no-antialias | -rasterize-downsample]
     [-rasterize-annots] [-rasterize-alpha]
     [-tobox <BoxName>]

## Listing images

The `-list-images` operation lists all images in the file:

    6, 1, /I0, 3300, 2550, 13432, 1, /DeviceGray, /FlateDecode, NoMask, none
    9, 2 3, /I1, 3376, 2649, 37972, 1, /DeviceGray, /FlateDecode, NoMask, none

The fields are *object number, page numbers, image name, width, height,
size in bytes, bits per pixel, colour space, filter (compression
method), mask type, mask object number*. Image masks are also listed,
and the mask object number may be used for cross-referencing. Mask types
are `ExplicitMask`, `ColourKeyMask`, `SMask`, `SMaskInData` and
`NoMask`.

With `-list-images-json`, the same information is available in JSON
format:

    [
      {
        "Object": 6,
        "Pages": [ 1 ],
        "Name": "/I0",
        "Width": 3300,
        "Height": 2550,
        "Bytes": 13432,
        "BitsPerComponent": 1,
        "Colourspace": "/DeviceGray",
        "Filter": "FlateDecode",
        "Mask": "NoMask",
        "MaskObjNum": null
      },
      {
        "Object": 9,
        "Pages": [ 2, 3 ],
        "Name": "/I0",
        "Width": 3376,
        "Height": 2649,
        "Bytes": 37972,
        "BitsPerComponent": 1,
        "Colourspace": "/DeviceGray",
        "Filter": "/FlateDecode"
        "Mask": "NoMask",
        "MaskObjNum": null
      }
    ]

By adding `-inline` to the command line, inline images will be listed
too. For inline images, the object number will be zero and the image
name will be `/InlineImage`.

## Listing images at point of use {#imageres}

To list all images in the given range of pages which fall below a given
resolution (in dots-per-inch), use the `-image-resolution` function:

`cpdf -image-resolution 300 in.pdf [<range>]`

Here is the result:

    2, /Im5, 531, 684, 149.935297, 150.138267, 31
    2, /Im6, 184, 164, 149.999988, 150.458710, 39
    2, /Im7, 171, 156, 149.999996, 150.579145, 40
    2, /Im9, 65, 91, 149.999986, 151.071856, 57
    2, /Im10, 94, 60, 149.999990, 152.284285, 59
    2, /Im15, 184, 139, 149.960011, 150.672060, 91
    4, /Im29, 53, 48, 149.970749, 151.616446, 93

The format is *page number, image name, x pixels, y pixels, x
resolution, y resolution, object number*. The resolutions refer to the
image's effective resolution at point of use (taking account of scaling,
rotation etc).

The information is also available in JSON format:

    [
      {
        "Object": 240,
        "Page": 79,
        "XObject": "/Z_Im0",
        "W": 3326,
        "H": 2584,
        "Xdpi": 300.0,
        "Ydpi": 300.0
      },
      {
        "Object": 243,
        "Page": 80,
        "XObject": "/Z_Im0",
        "W": 3300,
        "H": 2550,
        "Xdpi": 300.0,
        "Ydpi": 300.0
      }
    ]

To list all images regardless of resolution, use `-list-images-used` or
`-list-images-used-json` instead. Add `-inline` to list inline images
too.

## Extracting images

Cpdf can extract the raster images to a given location. JPEG and
JPEG2000 and lossless JBIG2 images are extracted directly.

Lossy JBIG2 images are extracted likewise, but an extra `__<n>` is
added, giving the number of the JBIG2Global stream for this image, which
is extracted as `<n>.jbig2global`. You may reconstruct the individual
images with, for example, `jbig2dec`.

Other images are written as PNGs, processed with either ImageMagick's
"magick" command, or NetPBM's "pnmtopng" program, whichever is
installed.

`cpdf -extract-images in.pdf [<range>] [-im <path>] [-p2p <path]`
`     [-dedup | -dedup-perpage] -o <path>`

The `-im` or `-p2p` option is used to give the path to the external
tool, one of which must be installed (unless `-raw` is added, which
outputs instead just JPEG or plain `.pnm` files).

The output specifier, e.g `-o output/%%%` gives the number format for
numbering the images. Output files are named serially from 0, and
include the page number too. For example, output files might be called
`output/000-p1.jpg`, `output/001-p1.png`, `output/002-p3.jpg` etc. The
specification `%objnum` may also be used to insert the object number of
the image. Here is an example invocation:

`cpdf -extract-images in.pdf -im magick -o output/%%%`

The `output` directory must already exist. The `-dedup` option
deduplicates images entirely; the `-dedup-perpage` option only per page.
The `-inline` option also extracts inline images; they will have
`-inline` appended to the stem of the file name.

Some images can have soft masks, which are a mechanism for adding
transparency to images in a PDF. Such masks will be extracted with a
`-mask` suffix. Adding `-merge-masks` to the command line will
post-process by merging each soft mask and its image to produce an
output PNG with an alpha channel, named by concatenating the two
existing file names and adding the suffix `-combined`.

To extract a single image, we can use the object number printed when we
use either `-list-images[-json]` or `-list-images-used[-json]`. For
example:

`cpdf -extract-single-image 14 in.pdf -im magick -o output`

This will extract the image at object 14 to
`output.{png, pnm, jpeg, jpeg2000, jbig2}`. Any soft mask will be
extracted with name `output-smask`. Add `-merge-masks` to merge soft
masks as already described. This single image extraction procedure does
not work for lossy JBIG2 images with JBIG2Globals streams.

## Removing an Image

To remove a particular image, find its name using `-list-images-used`
then apply the `-draft` and `-draft-remove-only` operations from Section FIXME.

## Processing Images

Cpdf can process images within a PDF, replacing the original with the
processed version. It does this by saving out the image data, putting it
through an external process, and then reading it back in and
re-inserting it. This is typically used to reduce the size of image
data, and thus the size of the PDF.

There are a number of option to deal with lossy (e.g JPEG) and lossless
images, one or more of which is specified. For example, the
`-jpeg-to-jpeg` option processes existing JPEG images to a given JPEG
quality level:

`cpdf -process-images -im magick -jpeg-to-jpeg 65 in.pdf -o out.pdf`

ImageMagick is required. Use `-im` to supply it. If we specify
`-process-images-info` too, we can see the work being done:

`cpdf -process-images -process-images-info -jpeg-to-jpeg 65`\
`     -im magick in.pdf -o out.pdf`

Here is sample output:

    (20/344) Object 265 (JPEG)... JPEG to JPEG 40798 -> 33463 (82%)
    (38/344) Object 278 (JPEG)... JPEG to JPEG 4382 -> 3482 (79%)
    (87/344) Object 266 (JPEG)... JPEG to JPEG 37227 -> 30199 (81%)
    (243/344) Object 209 (JPEG)... no size reduction
    (246/344) Object 270 (JPEG)... JPEG to JPEG 202568 -> 191175 (94%)
    (281/344) Object 280 (JPEG)... JPEG to JPEG 12255 -> 9825 (80%)
    (312/344) Object 279 (JPEG)... JPEG to JPEG 4117 -> 3157 (76%)

Similar output appears for the other methods, when they are specified.
You can see the counter of work being done, and the result for each
image chosen for processing. (The actual calls to external processes
like imagemagick may be seen by setting the `CPDF_SHOW_EXT` environment
variable to `true`).

The `-lossless-to-jpeg` option converts lossless images within PDFs to
JPEG too, at the given quality level. It may be specified in addition to
`-jpeg-to-jpeg`:

`cpdf -process-images -jpeg-to-jpeg 65 -lossless-to-jpeg 80`\
`     -im magick in.pdf -o out.pdf`

Images are only processed if they meet certain thresholds. Changes to
the default thresholds may be specified:

  Option                    Effect                                                             Default value
  ------------------------- ------------------------------------------------------------------ ----------------
  `-pixel-threshold`        Images below this number of pixels not processed                   25
  `-length-threshold`       Images with less than this number of bytes of data not processed   100
  `-percentage-threshold`   Results not below this percentage of original size discarded       99
  `-dpi-threshold`          Only images above this threshold at all use points processed       (no dpi check)

The `-process-images-force` option will process the image even if the
resulting image requires more storage than the original.

We may pick JPEG2000 compression instead of JPEG compression by choosing
the option `-lossless-to-jpeg2000` instead of `-lossless-to-jpeg` or
`-jpeg2000-to-jpeg2000` instead of `-jpeg-to-jpeg` or both.

Instead of compressing lossless images with lossy JPEG or JPEG2000
compression, we can resample losslessly:

`cpdf -process-images -im magick -lossless-resample 80 in.pdf -o out.pdf`

This will resample losslessly-compressed images to be 80 percent of the
original width and height. By default, there will be no interpolation.
To use interpolation, which may result in slightly larger data, add
`-resample-interpolate`. To use a DPI target instead, use
`-lossless-resample-dpi` instead:

`cpdf -process-images -im magick -lossless-resample-dpi 300`\
`     in.pdf -o out.pdf`

We can also use resampling with `-jpeg-to-jpeg`, buy specifying
`-jpeg-to-jpeg-scale`:

`cpdf -process-images -im magick -jpeg-to-jpeg 70 -jpeg-to-jpeg-scale 50`\
`     in.pdf -o out.pdf`

We can alternatively use a DPI target:

`cpdf -process-images -im magick -jpeg-to-jpeg 70 -jpeg-to-jpeg-dpi 150`\
`     in.pdf -o out.pdf`

The methods so far introduced do not operate on 1 bit per pixel data.
Different compression mechanisms are typically in use, and we need a
different approach. The `-1bpp-method` option specifies what to do with
losslessly compressed 1 bit-per-pixel images.

  Method         Effect                                                       
  -------------- ------------------------------------------------------------ --
  `JBIG2`        Lossless JBIG2                                               
  `JBIG2Lossy`   Lossy JBIG2, sharing JBIG2Globals data amongst all images.   
  `CCITTG4`      CCITT Group 4 fax, the best non-JBIG2 option.                
  `CCITTG3`      CCITT Group 3, obsolete.                                     

The JBIG2 options always require the `jbig2enc` program, whose location
may be specified with `-jbig2enc`. To convert from any JBIG2 compression
type to any other JBIG2 or non-JBIG2 compression type in addition
requires the `jbig2dec` program which may be specified with `-jbig2dec`.

For lossy JBIG2, the threshold for similarity of data may be set with\
`-jbig2-lossy-threshold`. For example:

`cpdf -process-images -jbig2enc jbig2enc -1bpp-method JBIG2Lossy`\
`     -jbig2-lossy-threshold 75 in.pdf -o out.pdf`

The `-process-images-force` option is always on when processing 1bpp
images, though for true forcing the length and pixel thresholds must
also be removed.

## Rasterization (PDF to image conversion)

Cpdf can send individual pages of a PDF out to `gs` to rasterize them -
they are then read back in and replace the original page content:

`cpdf -gs gs -rasterize in.pdf -o out.pdf`

Other metadata (for example, bookmarks) is preserved. By default, the
resolution is 144dpi, and the raster data is losslessly compressed. It
is the Crop Box which is rasterized, or the Media Box if absent. The
following options may be added:

  Option                      Effect                                               
  --------------------------- ---------------------------------------------------- --
  `-rasterize-gray`           Use grayscale instead of colour                      
  `-rasterize-1bpp`           Use monochrome instead of colour                     
  `-rasterize-jpeg`           Use JPEG instead of lossless compression             
  `-rasterize-jpeg-gray`      Use grayscale JPEG instead of lossless compression   
  `-rasterize-jpeg-quality`   Set JPEG image quality (0..100)                      
  `-rasterize-res`            Set the resolution                                   
  `-rasterize-annots`         Rasterize annotations instead of retaining           
  `-rasterize-no-antialias`   Turn off antialiasing                                
  `-rasterize-downsample`     Use better but slower antialiasing                   
  `-rasterize-alpha`          Produce an alpha channel (lossless only)             
  `-gs-quiet`                 Don't show `gs` output                               

In addition to rasterization of pages, we can export them in PNG or JPEG
format, again by the use of `gs`:

`cpdf -gs gs -output-image in.pdf 10-end -o image%%%.png`

This will extract pages 10 onwards to the files `image010.png`,
`image011.png` and so on. All the options above apply, and in addition
we can choose which box is rasterized:

  Option     Effect                     
  ---------- -------------------------- --
  `-tobox`   Choose rasterization box   

For example:

`cpdf -gs gs -output-image -tobox /BleedBox -rasterize-jpeg in.pdf`\
`     -o image%%%.jpeg`
