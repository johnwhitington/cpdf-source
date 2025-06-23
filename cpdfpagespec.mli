(** Page specifications and ranges *)

(** Here are the rules for building input ranges:

{ul
{- Page numbers can be given plain, like 1 or 2, or as page labels, like <1> or <iii> (backslash to escape intended angle bracket)}
{-  A page label may be used in place of a number e.g [[iii]] represents the first page found which is labelled [iii]. }
{- A dash (-) defines ranges e.g 1-5 or 6-3.}
{- A comma (,) allows one to specify several ranges, e.g. 1-2,4-5.}
{- The word end represents the last page number.}
{- The words odd and even can be used in place of or at the end of a page range to restrict to just the odd or even pages. }
{- The words portrait and landscape can be used in place of or at the end of a page range to restrict to just those pages which are portrait or landscape. Note that the meaning of portrait and landscape does not take account of any viewing rotation in place. A page with equal width and height is considered neither portrait nor landscape.}
{- The word reverse is the same as end-1.}
{- The word all is the same as 1-end.}
{- A range must contain no spaces.}
{- A tilde (~) defines a page number counting from the end of the document rather than the beginning. Page ~1 is the last page, ~2 the penultimate page etc.}
{- The word "annotated" refers to only such pages as contain annotations. Restrain with, e.g. "annotated:Highlight".}
{- Prepending NOT to a whole page range inverts it, once the whole is parsed. }
{- Appending DUP2 to a whole page range duplicates each page twice (or 3, or 4 etc. times), once the whole is parsed. }
}
*)

(** Parse a (valid) page specification to a page range *)
val parse_pagespec : Pdf.t -> string -> int list

(** Same, but without a PDF. Thus 'end' etc. don't work *)
val parse_pagespec_without_pdf : string -> int list

(** Is a page specification, in theory, valid? This is the most we can find out
without supplying a PDF, and thus knowing how many pages there are in it. *)
val validate_pagespec : string -> bool

(** Return a string for the given range. Knows how to identify all, odd, even,
x-y ranges etc. *)
val string_of_pagespec : Pdf.t -> int list -> string

(** Invert a range, given the maximum page number and the range. *)
val invert_range : int -> int list -> int list
