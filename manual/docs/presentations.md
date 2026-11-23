# Presentations

    cpdf -presentation in.pdf [<range>] -o out.pdf  
                       [-trans <transition-name>] [-duration <float>]  
                       [-vertical] [-outward] [-direction <int>]  
                       [-effect-duration <float>]  

The PDF file format, starting at Version 1.1, provides for simple
slide-show presentations in the manner of Microsoft Powerpoint. These
can be played in Acrobat and possibly other PDF viewers, typically
started by entering full-screen mode. The `-presentation` operation
allows such a presentation to be built from any PDF file.

The `-trans` option chooses the transition style. When a page range is
used, it is the transition *from* each page named which is altered. The
following transition styles are available:

Split

> Two lines sweep across the screen, revealing the new page. By default
  the lines are horizontal. Vertical lines are selected by using the
  `-vertical` option.

Blinds

> Multiple lines sweep across the screen, revealing the new page. By
  default the lines are horizontal. Vertical lines are selected by using
  the `-vertical` option.

Box

> A rectangular box sweeps inward from the edges of the page. Use
  `-outward` to make it sweep from the center to the edges.

Wipe

> A single line sweeps across the screen from one edge to the other in a
  direction specified by the `-direction` option.

Dissolve

> The old page dissolves gradually to reveal the new one.

Glitter

> The same as **Dissolve** but the effect sweeps across the page in the
  direction specified by the `-direction` option.

To remove a transition style currently applied to the selected pages,
omit the `-trans` option.

The `-effect-duration` option specifies the length of time in seconds
for the transition itself. The default value is one second.

The `-duration` option specifies the maximum time in seconds that the
page is displayed before the presentation automatically advances. The
default, in the absence of the `-duration` option, is for no automatic
advancement.

The `-direction` option (for **Wipe** and **Glitter** styles only)
specifies the direction of the effect. The following values are valid:

- Left to right

- Bottom to top (**Wipe** only)

- Right to left (**Wipe** only)

- Top to bottom

- Top-left to bottom-right (**Glitter** only)

For example:

`cpdf -presentation in.pdf 2-end -trans Split -duration 10 -o out.pdf`

> The **Split** style, with vertical lines, and each slide staying ten
seconds unless manually advanced. The first page (being a title) does
not move on automatically, and has no transition effect.

To use different options on different page ranges, run Cpdf multiple
times on the file using a different page range each time.
