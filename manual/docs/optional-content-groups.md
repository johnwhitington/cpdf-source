# Optional Content Groups

    cpdf -ocg-list in.pdf

    cpdf -ocg-list-json in.pdf

    cpdf -ocg-replace in.json in.pdf -o out.pdf

    cpdf -ocg-rename -ocg-rename-from <a> -ocg-rename-to <b> in.pdf -o out.pdf

    cpdf -ocg-remove in.pdf -o out.pdf

    cpdf -ocg-coalesce-on-name in.pdf -o out.pdf

    cpdf -ocg-order-all in.pdf -o out.pdf

In a PDF file, optional content groups are used to group graphical
elements together, so they may appear or not, depending on the
preference of the user. They are similar in some ways to layers in
graphics illustration programs.

List the optional content groups in the PDF, one per line, to standard
output, UTF8:

`cpdf -ocg-list in.pdf`

To list in JSON format instead, use `-ocg-list-json`. This
JSON data may be edited, and the result round-tripped back into the same
file with `-ocg-replace`:

`cpdf -ocg-replace in.json in.pdf -o out.pdf`

To rename an optional content group:

`cpdf -ocg-rename -ocg-rename-from <a> -ocg-rename-to <b> in.pdf -o out.pdf`

To remove the optional content group structure entirely, making all
content visible:

`cpdf -ocg-remove in.pdf -o out.pdf`

To coalesce optional content groups:

`cpdf -ocg-coalesce-on-name in.pdf -o out.pdf`

For example, if we merge or stamp two files both with an OCG called
"Layer 1", we will have two different optional content groups. Running
`-ocg-coalesce-on-name` will merge the two into a single optional
content group.

To ensure that every optional content group appears in the order list:

`cpdf -ocg-order-all in.pdf -o out.pdf`
