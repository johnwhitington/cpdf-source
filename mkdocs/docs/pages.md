# Pages


    cpdf -scale-page "<scale x> <scale y>" [-fast] [<position>]  
         in.pdf [<range>] -o out.pdf

    cpdf -scale-to-fit "<x size> <y size>" [-fast] [-prerotate] [<position>]  
         [-scale-to-fit-scale <scale>]  
         [-scale-to-fit-rotate-clockwise] [-scale-to-fit-rotate-anticlockwise]  
         in.pdf [<range>] -o out.pdf

    cpdf -stretch "<x size> <y size>" [-fast] in.pdf [<range>] -o out.pdf

    cpdf -center-to-fit "<x size> <y size>" [-fast] in.pdf [<range>] -o out.pdf

    cpdf -scale-contents [<scale>] [<position>] [-fast]  
         in.pdf [<range>] -o out.pdf

    cpdf -shift "<shift x> <shift y>" [-fast] in.pdf [<range>] -o out.pdf

    cpdf -shift-boxes "<shift x> <shift y>" in.pdf [<range>] -o out.pdf

    cpdf -rotate <angle> in.pdf [<range>] -o out.pdf

    cpdf -rotateby <angle> in.pdf [<range>] -o out.pdf

    cpdf -upright [-fast] in.pdf [<range>] -o out.pdf

    cpdf -rotate-contents <angle> [-fast] in.pdf [<range>] -o out.pdf

    cpdf -hflip [-fast] in.pdf [<range>] -o out.pdf

    cpdf -vflip [-fast] in.pdf [<range>] -o out.pdf

    cpdf -[media|crop|art|trim|bleed]box <boxspec> in.pdf [<range>] -o out.pdf

    cpdf -remove-[crop|art|trim|bleed]box in.pdf [<range>] -o out.pdf

    cpdf -frombox <boxname> -tobox <boxname> [-mediabox-if-missing]  
         in.pdf [<range>] -o out.pdf

    cpdf -hard-box <boxname> [-fast] in.pdf [<range>]  
         [-mediabox-if-missing] -o out.pdf

    cpdf -show-boxes [-fast] in.pdf [<range>] -o out.pdf

    cpdf -trim-marks [-fast] in.pdf [<range>] -o out.pdf

## Page Sizes

Any time when a page size is
required, instead of writing, for instance `"210mm 197mm"` one can
instead write `a4portrait`. Here is a list of supported page sizes:

    a0portrait         a1portrait          a2portrait
    a3portrait         a4portrait          a5portrait
    a6portrait         a7portrait          a8portrait
    a9portrait         a10portrait         
                                             
    a0landscape        a1landscape         a2landscape
    a3landscape        a4landscape         a5landscape
    a6landscape        a7landscape         a8landscape
    a9landscape        a10landscape        
                                             
    usletterportrait   usletterlandscape   
    uslegalportrait    uslegallandscape    

Note that this also works when four numbers are required: for example,
when setting the mediabox `"0 0 a3landscape"` will suffice.

## Scale Pages

The `-scale-page` operation scales each page in the range by the X and Y
factors given. This scales both the page contents, and the page size
itself. It also scales any Crop Box and other boxes (Art Box, Trim Box
etc). As with several of these commands, remember to take into account
any page rotation when considering what the X and Y axes relate to.

`cpdf -scale-page "2 2" in.pdf -o out.pdf`

> Convert an A4 page to A2, for instance.

The `-scale-to-fit` operation scales each page in the range to fit a
given page size, preserving aspect ratio and centring the result. If a
crop box is present, it is preferred to the media box.

`cpdf -scale-to-fit "297mm 210mm" in.pdf -o out.pdf`

`cpdf -scale-to-fit a4portrait in.pdf -o out.pdf`

> Scale a file's pages to fit A4 portrait.

To avoid centring, supply `-top`, `-bottom`, `-left` or `-right` as
appropriate. The scale can optionally be set to a proportion of the
available area, instead of filling it:

`cpdf -scale-to-fit a4portrait -scale-to-fit-scale 0.9 in.pdf -o out.pdf`

> Scale a file's pages to fit A4 portrait, scaling the page 90% of its
possible size.

To automatically calculate one or other dimension, use an asterisk:

`cpdf -scale-to-fit "297mm *" in.pdf -o out.pdf`

> Scale a file's pages to fit 297mm width, maintaining proportion.

To automatically rotate pages to maximise use of area on the output
size, add the option `-scale-to-fit-rotate-[anti]clockwise`.

The `-stretch` operation scales the contents to the given size without
regard to aspect ratio:

`cpdf -stretch a4landscape in.pdf -o out.pdf`

> Scale a file's pages and their content to fit A4 landscape.

The `-center-to-fit` operation changes the page size without scaling the
contents. It centers the old page on the new page.

`cpdf -center-to-fit a3portrait in.pdf -o out.pdf`

> Set a file's pages to the given size and center the content.

Sizes with asterisks like `"* 500pt"` may also be used here.

The `-scale-contents` operation scales the contents about the center of
the crop box (or, if absent, the media box), leaving the page dimensions
(boxes) unchanged.

`cpdf -scale-contents 0.5 in.pdf -o out.pdf`

> Scale a file's contents on all pages to 50% of its original dimensions.

To scale about a point other than the center, one can use the
positioning commands described in FIXME. For
example:

`cpdf -scale-contents 0.5 -topright 20 in.pdf -o out.pdf`

> Scale a file's contents on all pages to 50% of its original dimensions
about a point 20pts from its top right corner.

## Shift Page Contents

The `-shift` operation shifts the contents of each page in the range by
X points horizontally and Y points vertically.

`cpdf -shift "50 0" in.pdf even -o out.pdf`

> Shift pages to the right by 50 points (for instance, to increase the
binding margin).

The `-shift-boxes` operation has the same effect, but operates by moving
the page boxes only, avoiding processing the page contents. It is
therefore faster. Of course, the numbers must be inverted, since it is
the boxes being moved not the page:

`cpdf -shift-boxes "-50 0" in.pdf even -o out.pdf`

> Shift pages to the right by 50 points (for instance, to increase the
binding margin).

## Rotating Pages

There are two ways of rotating pages: (1) setting a value in the PDF
file which asks the viewer (e.g. Acrobat) to rotate the page on-the-fly
when viewing it (use `-rotate` or `-rotateby`) and (2) actually rotating
the page contents and/or the page dimensions (use `-upright` (described
elsewhere in this chapter) afterwards or `-rotate-contents` to just
rotate the page contents).

The possible values for `-rotate` and `-rotate-by` are 0, 90, 180 and
270, all interpreted as being clockwise. Any value may be used for
`-rotate-contents`.

The `-rotate` operation sets the viewing rotation of the selected pages
to the absolute value given.

`cpdf -rotate 90 in.pdf -o out.pdf`

> Set the rotation of all the pages in the input file to ninety degrees
clockwise.

The `-rotateby` operation changes the viewing rotation of all the given
pages by the relative value given.

`cpdf -rotateby 90 in.pdf -o out.pdf`

> Rotate all the pages in the input file by ninety degrees clockwise.

The `-rotate-contents` operation rotates the contents of the page by the
given relative value.

`cpdf -rotate-contents 90 in.pdf -o out.pdf`

> Rotate all the page contents in the input file by ninety degrees
clockwise. Does not change the page dimensions.

The `-upright` operation does whatever
combination of `-rotate` and `-rotate-contents` is required to change
the rotation of the document to zero without altering its appearance. In
addition, it makes sure the media box has its origin at (0,0), changing
other boxes to compensate. This is important because some operations in
CPDF (such as scale-to-fit), and in other PDF-processing programs, work
properly only when the origin is (0, 0).

`cpdf -upright in.pdf -o out.pdf`

> Make pages upright.

## Flipping Pages

The `-hflip` and `-vflip` operations flip the contents of the chosen
pages horizontally or vertically. No account is taken of the current
page rotation when considering what \"horizontally\" and \"vertically\"
mean, so you may like to use `-upright` (see above) first.

`cpdf -hflip in.pdf even -o out.pdf`

> Flip the even pages in `in.pdf` horizontally.

`cpdf -vflip in.pdf -o out.pdf`

> Flip all the pages in `in.pdf` vertically.

## Boxes and Cropping

All PDF files contain a *media box* for each page, giving the dimensions
of the paper. To change these dimensions (without altering the page
contents in any way), use the `-mediabox` operation.

`cpdf -mediabox "0pt 0pt 500pt 500pt" in.pdf -o out.pdf`

> Set the media box to 500 points square.

The four numbers are minimum x, minimum y, width, height. x coordinates
increase to the right, y coordinates increase upwards. To use absolute
numbers rather than width and height we may add an initial question mark
and write, for example, `?100pt 200pt 300pt 400pt` which represents the
rectangle with lower-left corner (100pt, 200pt) and upper-right corner
(300pt, 400pt).

PDF files can also optionally contain a *crop box* for each page,
defining to what extent the page is cropped before being displayed or
printed. A crop box can be set, changed and removed, without affecting
the underlying media box. To set or change the crop box use `-cropbox`.
To remove any existing crop box, use `-remove-cropbox`.

`cpdf -cropbox "0pt 0pt 200mm 200mm" in.pdf -o out.pdf`

Crop pages to the bottom left 200-millimeter square of the page.

`cpdf -remove-cropbox in.pdf -o out.pdf`

> Remove cropping.

Note that the crop box is only obeyed in some viewers. Similar
operations are available for the bleed, art, and trim boxes (`-art`,
`-remove-bleed` etc.)

`cpdf -frombox <boxname> -tobox <boxname> [-mediabox-if-missing] in.pdf [<range>] -o out.pdf`

> Copy the contents of one box to another.

This operation copies the contents of one box (Media box, Crop box, Trim
box etc.) to another. If `-mediabox-if-missing` is added, the media box
will be substituted when the 'from' box is not set for a given page. For
example

`cpdf -frombox /TrimBox -tobox /CropBox in.pdf -o out.pdf`

copies the Trim Box of each page to the Crop Box of each page. The
possible boxes are `/MediaBox`, `/CropBox`, `/BleedBox`, `/TrimBox`,
`/ArtBox`.

A hard box (one which clips its contents by inserting a clipping
rectangle) may be created with the `-hard-box` operation:

`cpdf -hard-box /TrimBox in.pdf -o out.pdf`

This means the resultant file may be used as a stamp without contents
outside the given box reappearing. The `-mediabox-if-missing` option may
also be used here.

## Showing Boxes and Printer's Marks

The `-show-boxes` operation displays the boxes present on each page as
method of debugging. Since boxes may be coincident, they are shown in
differing colours and dash patterns so they may be identified even where
they overlap. The colours are:

    Media box   Red      
    Crop box    Green    
    Art box     Blue     
    Trim box    Orange   
    Bleed box   Pink     

The `-trim-marks` operation adds trim marks to a PDF file. The trim box
must be present.
