(* cpdf command line tools *)
let demo = false
let agpl = true
let major_version = 2
let minor_version = 7
let minor_minor_version = 2
let version_date = "(28th October 2024)"

open Pdfutil
open Pdfio

let combine_with_spaces strs =
  String.trim
    (fold_left (fun x y -> x ^ (if x <> "" then " " else "") ^ y) "" strs)

let tempfiles = ref []

let exit n =
  begin try iter Sys.remove !tempfiles with _ -> exit n end;
  exit n

let null () = ()

let initial_file_size = ref 0

let empty = Pdf.empty ()

(* Wrap up the file reading functions to exit with code 1 when an encryption
problem occurs. This happens when object streams are in an encrypted document
and so it can't be read without the right password... The existing error
handling only dealt with the case where the document couldn't be decrypted once
it had been loaded. *)
let pdfread_pdf_of_input ?revision a b c =
  try Pdfread.pdf_of_input ?revision a b c with
    Pdf.PDFError s when String.length s >=10 && String.sub s 0 10 = "Encryption" ->
      raise (Cpdferror.SoftError "Bad owner or user password when reading document")

let pdfread_pdf_of_channel_lazy ?revision ?source b c d =
  try Pdfread.pdf_of_channel_lazy ?revision ?source b c d with
    Pdf.PDFError s when String.length s >=10 && String.sub s 0 10 = "Encryption" ->
      raise (Cpdferror.SoftError "Bad owner or user password when reading document")

let pdfread_pdf_of_file ?revision a b c =
  try Pdfread.pdf_of_file ?revision a b c with
    Pdf.PDFError s when String.length s >=10 && String.sub s 0 10 = "Encryption" ->
      raise (Cpdferror.SoftError "Bad owner or user password when reading document")

let optstring = function
  | "" -> None
  | x -> Some x

let _ =
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true

let stay_on_error = ref false

exception StayOnError

(* Fatal error reporting. *)
let error s =
  Pdfe.log (s ^ "\nUse -help for help.\n");
  if not !stay_on_error then exit 2 else raise StayOnError

let soft_error s =
  Pdfe.log (Printf.sprintf "%s\n" s);
  if not !stay_on_error then exit 1 else raise StayOnError

let parse_pagespec pdf spec =
  try Cpdfpagespec.parse_pagespec pdf spec with
    Failure x -> error x

(* We allow an operation such as ScaleToFit on a range such as 'portrait' to be silently null to allow, for example:
cpdf -scale-to-fit a4portrait in.pdf portrait AND -scale-to-fit a4landscape landscape -o out.pdf
*)
let parse_pagespec_allow_empty pdf spec =
  try Cpdfpagespec.parse_pagespec pdf spec with
    Pdf.PDFError ("Page range specifies no pages") -> []

(* Operations. *)
type op =
  | CopyFont of string
  | CountPages
  | Version
  | Encrypt
  | Decrypt
  | StampOn of string
  | StampUnder of string
  | CombinePages of string
  | TwoUp
  | TwoUpStack
  | Impose of bool
  | RemoveBookmarks
  | AddBookmarks of string
  | AddText of string
  | AddRectangle
  | RemoveText
  | Draft
  | PadBefore
  | PadAfter
  | PadEvery of int
  | PadMultiple of int
  | PadMultipleBefore of int
  | Shift
  | ShiftBoxes
  | Scale
  | ScaleToFit
  | Stretch
  | CenterToFit
  | ScaleContents of float
  | AttachFile of string list
  | RemoveAttachedFiles
  | ListAttachedFiles
  | DumpAttachedFiles
  | RemoveAnnotations
  | ListAnnotations
  | CopyAnnotations of string
  | SetAnnotations of string
  | Merge
  | Split
  | SplitOnBookmarks of int
  | SplitMax of int
  | Spray
  | Clean
  | Info
  | PageInfo
  | Metadata
  | SetMetadata of string
  | RemoveMetadata
  | Fonts
  | RemoveFonts
  | Compress
  | Decompress
  | Crop
  | Trim
  | Bleed
  | Art
  | RemoveCrop
  | RemoveArt
  | RemoveTrim
  | RemoveBleed
  | CopyBox
  | MediaBox
  | HardBox of string
  | Rotate of int
  | Rotateby of int
  | RotateContents of float
  | Upright
  | VFlip
  | HFlip
  | ThinLines of float
  | SetAuthor of string
  | SetTitle of string
  | SetSubject of string
  | SetKeywords of string
  | SetCreate of string
  | SetModify of string
  | SetCreator of string
  | SetProducer of string
  | SetTrapped
  | SetUntrapped
  | SetVersion of int
  | ListBookmarks
  | SetPageLayout of string
  | SetPageMode of string
  | SetNonFullScreenPageMode of string
  | HideToolbar of bool
  | HideMenubar of bool
  | HideWindowUI of bool
  | FitWindow of bool
  | CenterWindow of bool
  | DisplayDocTitle of bool
  | Presentation
  | ChangeId
  | RemoveId
  | CopyId of string
  | BlackText
  | BlackLines
  | BlackFills
  | ExtractImages
  | ListImages
  | ImageResolution of float
  | MissingFonts
  | ExtractFontFile of string
  | ExtractText
  | OpenAtPage of string
  | OpenAtPageFit of string
  | OpenAtPageCustom of string
  | AddPageLabels
  | RemovePageLabels
  | PrintPageLabels
  | RemoveDictEntry of string
  | ReplaceDictEntry of string
  | PrintDictEntry of string
  | ListSpotColours
  | RemoveClipping
  | SetMetadataDate of string
  | CreateMetadata
  | EmbedMissingFonts
  | BookmarksOpenToLevel of int
  | CreatePDF
  | RemoveAllText
  | ShowBoxes
  | TrimMarks
  | Prepend of string
  | Postpend of string
  | OutputJSON
  | OCGCoalesce
  | OCGList
  | OCGRename
  | OCGOrderAll
  | StampAsXObject of string
  | PrintFontEncoding of string
  | TableOfContents
  | Typeset of string
  | TextWidth of string
  | Draw
  | Composition of bool
  | Chop of int * int
  | ChopHV of bool * float
  | ProcessImages
  | ExtractStream of string
  | PrintObj of string
  | ReplaceObj of string * string
  | Verify of string
  | MarkAs of Cpdfua.subformat
  | RemoveMark of Cpdfua.subformat
  | PrintStructTree
  | ExtractStructTree
  | ReplaceStructTree of string
  | SetLanguage of string
  | Redact

let string_of_op = function
  | PrintFontEncoding _ -> "PrintFontEncoding"
  | PrintDictEntry _ -> "PrintDictEntry"
  | Impose _ -> "Impose"
  | CopyFont _ -> "CopyFont"
  | CountPages -> "CountPages"
  | Version -> "Version"
  | Encrypt -> "Encrypt"
  | Decrypt -> "Decrypt"
  | StampOn _ -> "StampOn"
  | StampUnder _ -> "StampUnder"
  | CombinePages _ -> "CombinePages"
  | TwoUp -> "TwoUp"
  | TwoUpStack -> "TwoUpStack"
  | RemoveBookmarks -> "RemoveBookmarks"
  | AddBookmarks _ -> "AddBookmarks"
  | AddText _ -> "AddText"
  | AddRectangle -> "AddRectangle"
  | RemoveText -> "RemoveText"
  | Draft -> "Draft"
  | PadBefore -> "PadBefore"
  | PadAfter -> "PadAfter"
  | PadEvery _ -> "PadEvery"
  | PadMultiple _ -> "PadMultiple"
  | PadMultipleBefore _ -> "PadMultipleBefore"
  | Shift -> "Shift"
  | ShiftBoxes -> "ShiftBoxes"
  | Scale -> "Scale"
  | ScaleToFit -> "ScaleToFit"
  | Stretch -> "Stretch"
  | CenterToFit -> "CenterToFit"
  | ScaleContents _ -> "ScaleContents"
  | AttachFile _ -> "AttachFile"
  | RemoveAttachedFiles -> "RemoveAttachedFiles"
  | ListAttachedFiles -> "ListAttachedFiles"
  | DumpAttachedFiles -> "DumpAttachedFiles"
  | RemoveAnnotations -> "RemoveAnnotations"
  | ListAnnotations -> "ListAnnotations"
  | CopyAnnotations _ -> "CopyAnnotations"
  | SetAnnotations _ -> "SetAnnotations"
  | Merge -> "Merge"
  | Split -> "Split"
  | SplitOnBookmarks _ -> "SplitOnBookmarks"
  | SplitMax _ -> "SplitMax"
  | Spray -> "Spray"
  | Clean -> "Clean"
  | Info -> "Info"
  | PageInfo -> "PageInfo"
  | Metadata -> "Metadata"
  | SetMetadata _ -> "SetMetadata"
  | RemoveMetadata -> "RemoveMetadata"
  | Fonts -> "Fonts"
  | RemoveFonts -> "RemoveFonts"
  | Compress -> "Compress"
  | Decompress -> "Decompress"
  | Crop -> "Crop"
  | RemoveCrop -> "RemoveCrop"
  | CopyBox -> "CopyBox"
  | MediaBox -> "MediaBox"
  | HardBox _ -> "HardBox"
  | Rotate _ -> "Rotate"
  | Rotateby _ -> "Rotateby"
  | RotateContents _ -> "RotateContents"
  | Upright -> "Upright"
  | VFlip -> "VFlip"
  | HFlip -> "HFlip"
  | ThinLines _ -> "ThinLines"
  | SetAuthor _ -> "SetAuthor"
  | SetTitle _ -> "SetTitle"
  | SetSubject _ -> "SetSubject"
  | SetKeywords _ -> "SetKeywords"
  | SetCreate _ -> "SetCreate"
  | SetModify _ -> "SetModify"
  | SetCreator _ -> "SetCreator"
  | SetProducer _ -> "SetProducer"
  | SetTrapped -> "SetTrapped"
  | SetUntrapped -> "SetUntrapped"
  | SetVersion _ -> "SetVersion"
  | ListBookmarks -> "ListBookmarks"
  | SetPageLayout _ -> "SetPageLayout"
  | SetPageMode _ -> "SetPageMode"
  | SetNonFullScreenPageMode _ -> "SetNonFullScreenPageMode" 
  | HideToolbar _ -> "HideToolbar"
  | HideMenubar _ -> "HideMenubar"
  | HideWindowUI _ -> "HideWindowUI"
  | FitWindow _ -> "FitWindow"
  | CenterWindow _ -> "CenterWindow"
  | DisplayDocTitle _ -> "DisplayDocTitle"
  | Presentation -> "Presentation"
  | ChangeId -> "ChangeId"
  | RemoveId -> "RemoveId"
  | CopyId _ -> "CopyId"
  | BlackText -> "BlackText"
  | BlackLines -> "BlackLines"
  | BlackFills -> "BlackFills"
  | ExtractImages -> "ExtractImages"
  | ListImages -> "ListImages"
  | ImageResolution _ -> "ImageResolution"
  | MissingFonts -> "MissingFonts"
  | ExtractFontFile _ -> "ExtractFontFile"
  | ExtractText -> "ExtractText"
  | OpenAtPage _ -> "OpenAtPage"
  | OpenAtPageFit _ -> "OpenAtPageFit"
  | OpenAtPageCustom _ -> "OpenAtPageCustom"
  | AddPageLabels -> "AddPageLabels"
  | RemovePageLabels -> "RemovePageLabels"
  | PrintPageLabels -> "PrintPageLabels"
  | RemoveDictEntry _ -> "RemoveDictEntry"
  | ReplaceDictEntry _ -> "ReplaceDictEntry"
  | ListSpotColours -> "ListSpotColours"
  | RemoveClipping -> "RemoveClipping"
  | Trim -> "Trim"
  | Art -> "Art"
  | Bleed -> "Bleed"
  | RemoveArt -> "RemoveArt"
  | RemoveTrim -> "RemoveTrim"
  | RemoveBleed -> "RemoveBleed"
  | SetMetadataDate _ -> "SetMetadataDate"
  | CreateMetadata -> "CreateMetadata"
  | EmbedMissingFonts -> "EmbedMissingFonts"
  | BookmarksOpenToLevel _ -> "BookmarksOpenToLevel"
  | CreatePDF -> "CreatePDF"
  | RemoveAllText -> "RemoveAllText"
  | ShowBoxes -> "ShowBoxes"
  | TrimMarks -> "TrimMarks"
  | Prepend _ -> "Prepend"
  | Postpend _ -> "Postpend"
  | OutputJSON -> "OutputJSON"
  | OCGCoalesce -> "OCGCoalesce"
  | OCGList -> "OCGList"
  | OCGRename -> "OCGRename"
  | OCGOrderAll -> "OCGOrderAll"
  | StampAsXObject _ -> "StampAsXObject"
  | TableOfContents -> "TableOfContents"
  | Typeset _ -> "Typeset"
  | TextWidth _ -> "TextWidth"
  | Draw -> "Draw"
  | Composition _ -> "Composition"
  | Chop _ -> "Chop"
  | ChopHV _ -> "ChopHV"
  | ProcessImages -> "ProcessImages"
  | ExtractStream _ -> "ExtractStream"
  | PrintObj _ -> "PrintObj"
  | ReplaceObj _ -> "ReplaceObj"
  | Verify _ -> "Verify"
  | MarkAs _ -> "MarkAs"
  | RemoveMark _ -> "RemoveMark"
  | PrintStructTree -> "PrintStructTree"
  | ExtractStructTree -> "ExtractStructTree"
  | ReplaceStructTree _ -> "ReplaceStructTree"
  | SetLanguage _ -> "SetLanguage"
  | Redact -> "Redact"

(* Inputs: filename, pagespec. *)
type input_kind = 
  | AlreadyInMemory of Pdf.t * string
  | InFile of string
  | StdIn

let string_of_input_kind = function
  | AlreadyInMemory (_, s) -> s
  | InFile s -> s
  | StdIn -> "Stdin"

type input =
  input_kind * string * string * string * bool ref * int option
  (* input kind, range, user_pw, owner_pw, was_decrypted_with_owner, revision *)

type output_method =
  | NoOutputSpecified
  | Stdout
  | File of string

(* Outputs are also added here, in case -spray is in use. *)
let spray_outputs = ref []

(* A list of PDFs to be output, if no output method was specified. *)
let output_pdfs : Pdf.t list ref = ref []

let standard_namespace = "http://iso.org/pdf/ssn"
let pdf2_namespace = "http://iso.org/pdf2/ssn"

type font =
  | StandardFont of Pdftext.standard_font
  | EmbeddedFont of string
  | OtherFont of string

type args =
  {mutable op : op option;
   mutable preserve_objstm : bool;
   mutable create_objstm : bool;
   mutable out : output_method;
   mutable inputs : input list;
   mutable chunksize : int;
   mutable linearize : bool;
   mutable keeplinearize : bool;
   mutable rectangle : string;
   mutable coord : string;
   mutable duration : float option;
   mutable transition : string option;
   mutable horizontal : bool;
   mutable inward : bool;
   mutable direction : int;
   mutable effect_duration : float;
   mutable font : font;
   mutable fontname : string;
   mutable fontencoding : Pdftext.encoding;
   mutable fontsize : float;
   mutable embedstd14 : string option;
   mutable color : Cpdfaddtext.colour;
   mutable opacity : float;
   mutable position : Cpdfposition.position;
   mutable underneath : bool;
   mutable linespacing : float;
   mutable midline : bool;
   mutable topline : bool;
   mutable justification : Cpdfaddtext.justification;
   mutable bates : int;
   mutable batespad : int option;
   mutable prerotate : bool;
   mutable relative_to_cropbox : bool;
   mutable keepversion : bool;
   mutable bycolumns : bool;
   mutable pagerotation : int;
   mutable crypt_method : string;
   mutable owner : string;
   mutable user : string;
   mutable no_edit : bool;
   mutable no_print : bool;
   mutable no_copy : bool;
   mutable no_annot : bool;
   mutable no_forms : bool;
   mutable no_extract : bool;
   mutable no_assemble : bool;
   mutable no_hq_print : bool;
   mutable debug : bool;
   mutable debugcrypt : bool;
   mutable debugforce : bool;
   mutable boxes : bool;
   mutable encrypt_metadata : bool;
   mutable retain_numbering : bool;
   mutable process_struct_trees : bool;
   mutable remove_duplicate_fonts : bool;
   mutable remove_duplicate_streams : bool;
   mutable encoding : Cpdfmetadata.encoding;
   mutable scale : float;
   mutable copyfontpage : int;
   mutable copyfontname : string option;
   mutable fast : bool;
   mutable dashrange : string;
   mutable outline : bool;
   mutable linewidth : float;
   mutable path_to_ghostscript : string;
   mutable path_to_im : string;
   mutable path_to_p2p : string;
   mutable path_to_jbig2enc : string;
   mutable frombox : string option;
   mutable tobox : string option;
   mutable mediabox_if_missing : bool;
   mutable topage : string option;
   mutable scale_stamp_to_fit : bool;
   mutable labelstyle : Pdfpagelabels.labelstyle;
   mutable labelprefix : string option;
   mutable labelstartval : int;
   mutable labelsprogress : bool;
   mutable squeeze : bool;
   mutable squeeze_recompress : bool;
   mutable squeeze_pagedata: bool;
   mutable original_filename : string;
   mutable was_encrypted : bool;
   mutable cpdflin : string option;
   mutable recrypt : bool;
   mutable was_decrypted_with_owner : bool;
   mutable creator : string option;
   mutable producer : string option;
   mutable extract_text_font_size : float option;
   mutable padwith : string option;
   mutable alsosetxml : bool;
   mutable justsetxml : bool;
   mutable gs_malformed : bool;
   mutable gs_quiet : bool;
   mutable merge_add_bookmarks : bool;
   mutable merge_add_bookmarks_use_titles : bool;
   mutable createpdf_pages : int;
   mutable createpdf_pagesize : Pdfpaper.t;
   mutable removeonly : string option;
   mutable jsonparsecontentstreams : bool;
   mutable jsonnostreamdata : bool;
   mutable jsondecompressstreams : bool;
   mutable jsoncleanstrings : bool;
   mutable ocgrenamefrom : string;
   mutable ocgrenameto : string;
   mutable dedup : bool;
   mutable dedup_per_page : bool;
   mutable collate : bool;
   mutable impose_columns : bool;
   mutable impose_rtl : bool;
   mutable impose_btt : bool;
   mutable impose_center : bool;
   mutable impose_margin : float;
   mutable impose_spacing : float;
   mutable impose_linewidth : float;
   mutable format_json : bool;
   mutable replace_dict_entry_value : Pdf.pdfobject;
   mutable dict_entry_search : Pdf.pdfobject option;
   mutable toc_title : string;
   mutable toc_bookmark : bool;
   mutable idir_only_pdfs : bool;
   mutable no_warn_rotate : bool;
   mutable jpegquality : float;
   mutable jpegqualitylossless : float;
   mutable jpegtojpegscale : float;
   mutable jpegtojpegdpi : float;
   mutable onebppmethod : string;
   mutable pixel_threshold : int;
   mutable length_threshold : int;
   mutable percentage_threshold : float;
   mutable dpi_threshold : float;
   mutable resample_factor : float;
   mutable resample_interpolate : bool;
   mutable jbig2_lossy_threshold : float;
   mutable extract_stream_decompress : bool;
   mutable verify_single : string option;
   mutable draw_struct_tree : bool;
   mutable subformat : Cpdfua.subformat option;
   mutable indent : float option;
   mutable title : string option}

let args =
  {op = None;
   preserve_objstm = true;
   create_objstm = false;
   out = NoOutputSpecified;
   inputs = [];
   chunksize = 1;
   linearize = false;
   keeplinearize = false;
   rectangle = "0 0 0 0";
   coord = "0 0";
   duration = None;
   transition = None;
   horizontal = true;
   inward = true;
   direction = 0;
   effect_duration = 1.;
   font = StandardFont Pdftext.TimesRoman;
   fontname = "Times-Roman";
   fontsize = 12.;
   fontencoding = Pdftext.WinAnsiEncoding;
   color = Cpdfaddtext.RGB (0., 0., 0.);
   opacity = 1.;
   position = Cpdfposition.TopLeft (100., 100.);
   underneath = false;
   linespacing = 1.;
   midline = false;
   topline = false;
   justification = Cpdfaddtext.LeftJustify;
   bates = 0;
   batespad = None;
   prerotate = false;
   relative_to_cropbox = false;
   keepversion = false;
   bycolumns = false;
   pagerotation = 0;
   crypt_method = "";
   owner = "";
   user = "";
   no_edit = false;
   no_print = false;
   no_copy = false;
   no_annot = false;
   no_forms = false;
   no_extract = false;
   no_assemble = false;
   no_hq_print = false;
   debug = false;
   debugcrypt = false;
   debugforce = false;
   boxes = false;
   encrypt_metadata = true;
   retain_numbering = false;
   process_struct_trees = false;
   remove_duplicate_fonts = false;
   remove_duplicate_streams = false;
   encoding = Cpdfmetadata.Stripped;
   scale = 1.;
   copyfontpage = 1;
   copyfontname = None;
   fast = false;
   dashrange = "all";
   outline = false;
   linewidth = 1.0;
   path_to_ghostscript = "";
   path_to_im = "";
   path_to_p2p = "";
   path_to_jbig2enc = "";
   frombox = None;
   tobox = None;
   mediabox_if_missing = false;
   topage = None;
   scale_stamp_to_fit = false;
   labelstyle = Pdfpagelabels.DecimalArabic;
   labelprefix = None;
   labelstartval = 1;
   labelsprogress = false;
   squeeze = false;
   squeeze_recompress = true;
   squeeze_pagedata = true;
   original_filename = "";
   was_encrypted = false;
   cpdflin = None;
   recrypt = false;
   was_decrypted_with_owner = false;
   producer = None;
   creator = None;
   embedstd14 = None;
   extract_text_font_size = None;
   padwith = None;
   alsosetxml = false;
   justsetxml = false;
   gs_malformed = false;
   gs_quiet = false;
   merge_add_bookmarks = false;
   merge_add_bookmarks_use_titles = false;
   createpdf_pages = 1;
   createpdf_pagesize = Pdfpaper.a4;
   removeonly = None;
   jsonparsecontentstreams = false;
   jsonnostreamdata = false;
   jsondecompressstreams = false;
   jsoncleanstrings = false;
   ocgrenamefrom = "";
   ocgrenameto = "";
   dedup = false;
   dedup_per_page = false;
   collate = false;
   impose_columns = false;
   impose_rtl = false;
   impose_btt = false;
   impose_center = false;
   impose_margin = 0.;
   impose_spacing = 0.;
   impose_linewidth = 0.;
   format_json = false;
   replace_dict_entry_value = Pdf.Null;
   dict_entry_search = None;
   toc_title = "Table of Contents";
   toc_bookmark = true;
   idir_only_pdfs = false;
   no_warn_rotate = false;
   jpegquality = 100.;
   jpegqualitylossless = 101.;
   jpegtojpegscale = 100.;
   jpegtojpegdpi = 0.;
   onebppmethod = "";
   pixel_threshold = 25;
   length_threshold = 100;
   percentage_threshold = 99.;
   dpi_threshold = 0.;
   resample_factor = 101.;
   resample_interpolate = false;
   jbig2_lossy_threshold = 0.85;
   extract_stream_decompress = false;
   verify_single = None;
   draw_struct_tree = false;
   subformat = None;
   indent = None;
   title = None}

(* Do not reset original_filename or cpdflin or was_encrypted or
was_decrypted_with_owner or recrypt or producer or creator or path_to_* or
gs_malformed or gs_quiet or no-warn-rotate, since we want these to work
across ANDs. Or squeeze options: a little odd, but we want it to happen on
eventual output. Or -debug-force (from v2.6). *)
let reset_arguments () =
  args.op <- None;
  args.preserve_objstm <- true;
  args.create_objstm <- false;
  args.out <- NoOutputSpecified;
  args.inputs <- [];
  args.chunksize <- 1;
  args.linearize <- false;
  args.keeplinearize <- false;
  args.rectangle <- "0 0 0 0";
  args.coord <- "0 0";
  args.duration <- None;
  args.transition <- None;
  args.horizontal <- true;
  args.inward <- true;
  args.direction <- 0;
  args.effect_duration <- 1.;
  args.font <- StandardFont Pdftext.TimesRoman;
  args.fontname <- "Times-Roman";
  args.fontsize <- 12.;
  args.fontencoding <- Pdftext.WinAnsiEncoding;
  args.color <- Cpdfaddtext.RGB (0., 0., 0.);
  args.opacity <- 1.;
  args.position <- Cpdfposition.TopLeft (100., 100.);
  args.underneath <- false;
  args.linespacing <- 1.;
  args.midline <- false;
  args.topline <- false;
  args.justification <- Cpdfaddtext.LeftJustify;
  args.bates <- 0;
  args.batespad <- None;
  args.prerotate <- false;
  args.relative_to_cropbox <- false;
  args.keepversion <- false;
  args.bycolumns <- false;
  args.pagerotation <- 0;
  args.crypt_method <- "";
  args.owner <- "";
  args.user <- "";
  args.no_edit <- false;
  args.no_print <- false;
  args.no_copy <- false;
  args.no_annot <- false;
  args.no_forms <- false;
  args.no_extract <- false;
  args.no_assemble <- false;
  args.no_hq_print <- false;
  args.debug <- false;
  args.debugcrypt <- false;
  args.boxes <- false;
  args.encrypt_metadata <- true;
  args.retain_numbering <- false;
  args.process_struct_trees <- false;
  args.remove_duplicate_fonts <- false;
  args.remove_duplicate_streams <- false;
  args.encoding <- Cpdfmetadata.Stripped;
  args.scale <- 1.;
  args.copyfontpage <- 1;
  args.copyfontname <- None;
  args.fast <- false;
  args.dashrange <- "all";
  args.outline <- false;
  args.linewidth <- 1.0;
  args.frombox <- None;
  args.tobox <- None;
  args.mediabox_if_missing <- false;
  args.topage <- None;
  args.scale_stamp_to_fit <- false;
  args.labelstyle <- Pdfpagelabels.DecimalArabic;
  args.labelprefix <- None;
  args.labelstartval <- 1;
  args.labelsprogress <- false;
  args.embedstd14 <- None;
  args.extract_text_font_size <- None;
  args.padwith <- None;
  args.alsosetxml <- false;
  args.justsetxml <- false;
  args.merge_add_bookmarks <- false;
  args.merge_add_bookmarks_use_titles <- false;
  args.createpdf_pages <- 1;
  args.createpdf_pagesize <- Pdfpaper.a4;
  args.removeonly <- None;
  args.jsonparsecontentstreams <- false;
  args.jsonnostreamdata <- false;
  args.jsondecompressstreams <- false;
  args.jsoncleanstrings <- false;
  args.ocgrenamefrom <- "";
  args.ocgrenameto <- "";
  args.dedup <- false;
  args.dedup_per_page <- false;
  args.collate <- false;
  args.impose_columns <- false;
  args.impose_rtl <- false;
  args.impose_btt <- false;
  args.impose_center <- false;
  args.impose_margin <- 0.;
  args.impose_spacing <- 0.;
  args.impose_linewidth <- 0.;
  args.format_json <- false;
  args.replace_dict_entry_value <- Pdf.Null;
  args.dict_entry_search <- None;
  args.toc_title <- "Table of Contents";
  args.toc_bookmark <- true;
  args.idir_only_pdfs <- false;
  args.jpegquality <- 100.;
  args.jpegqualitylossless <- 101.;
  args.onebppmethod <- "";
  args.pixel_threshold <- 25;
  args.length_threshold <- 100;
  args.percentage_threshold <- 99.;
  args.dpi_threshold <- 0.;
  args.resample_factor <- 101.;
  args.resample_interpolate <- false;
  args.jbig2_lossy_threshold <- 0.85;
  args.extract_stream_decompress <- false;
  clear Cpdfdrawcontrol.fontpack_initialised;
  args.verify_single <- None;
  args.draw_struct_tree <- false;
  args.subformat <- None;
  args.indent <- None;
  args.title <- None

(* Prefer a) the one given with -cpdflin b) a local cpdflin, c) otherwise assume
installed at a system place *)
let find_cpdflin provided =
  match provided with
    Some x -> x
  | None ->
      let dotslash = match Sys.os_type with "Win32" -> "" | _ -> "./" in
      if Sys.file_exists "cpdflin" then (dotslash ^ "cpdflin") else
      if Sys.file_exists "cpdflin.exe" then (dotslash ^ "cpdflin.exe") else
        match Sys.os_type with
          "Win32" -> "cpdflin.exe"
        | _ -> "cpdflin"

(* Call cpdflin, given the (temp) input name, the output name, and the location
of the cpdflin binary. Returns the exit code. *)
let call_cpdflin cpdflin temp output best_password =
  let command =
    Filename.quote_command cpdflin
      ["--linearize"; ("--password=" ^ best_password); temp; output]
  in
    match Sys.os_type with
      "Win32" ->
        (* On windows, don't use LD_LIBRARY_PATH - it will happen automatically *)
        if args.debug then Pdfe.log (command ^ "\n");
        Sys.command command
    | _ ->
        (* On other platforms, if -cpdflin was provided, or cpdflin was in the
        current folder, set up LD_LIBRARY_PATH: *)
        match cpdflin with
          "cpdflin" ->
            if args.debug then Pdfe.log (command ^ "\n");
            Sys.command command
        | _ ->
            let command = 
              "DYLD_FALLBACK_LIBRARY_PATH=" ^ Filename.quote (Filename.dirname cpdflin) ^ " " ^
              "LD_LIBRARY_PATH=" ^ Filename.quote (Filename.dirname cpdflin) ^ " " ^
              command
            in
              if args.debug then Pdfe.log (command ^ "\n");
              Sys.command command

let get_pagespec () =
  match args.inputs with
  | (_, ps, _, _, _, _)::_ -> ps
  | _ -> error "No range specified for input, or specified too late."

let string_of_permission = function
  | Pdfcrypt.NoEdit -> "No edit"
  | Pdfcrypt.NoPrint -> "No print" 
  | Pdfcrypt.NoCopy -> "No copy"
  | Pdfcrypt.NoAnnot -> "No annotate"
  | Pdfcrypt.NoForms -> "No edit forms"
  | Pdfcrypt.NoExtract -> "No extract"
  | Pdfcrypt.NoAssemble -> "No assemble"
  | Pdfcrypt.NoHqPrint -> "No high-quality print"

let getpermissions pdf =
  fold_left
    (fun x y -> if x = "" then x ^ y else x ^ ", " ^ y)
    ""
    (map string_of_permission (Pdfread.permissions pdf))

let banlist_of_args () =
  let l = ref [] in
    if args.no_edit then l =| Pdfcrypt.NoEdit;
    if args.no_print then l =| Pdfcrypt.NoPrint;
    if args.no_copy then l =| Pdfcrypt.NoCopy;
    if args.no_annot then l =| Pdfcrypt.NoAnnot;
    if args.no_forms then l =| Pdfcrypt.NoForms;
    if args.no_extract then l =| Pdfcrypt.NoExtract;
    if args.no_assemble then l =| Pdfcrypt.NoAssemble;
    if args.no_hq_print then l =| Pdfcrypt.NoHqPrint;
    !l

(* If a file is encrypted, decrypt it using the owner password or, if not
present, the user password. If the user password is used, the operation to be
performed is checked to see if it's allowable under the permissions regime. *)

(* The bans. Each function has a list of bans. If any of these is present in the
bans list in the input file, the operation cannot proceed. Other operations
cannot proceed at all without owner password. *)
let banned banlist = function
  | Fonts | Info | Metadata | PageInfo | CountPages
  | ListAttachedFiles | ListAnnotations
  | ListBookmarks | ImageResolution _ | ListImages | MissingFonts
  | PrintPageLabels | Clean | Compress | Decompress
  | ChangeId | CopyId _ | ListSpotColours | Version
  | DumpAttachedFiles | RemoveMetadata | EmbedMissingFonts | BookmarksOpenToLevel _ | CreatePDF
  | SetPageMode _ | SetNonFullScreenPageMode _ | HideToolbar _ | HideMenubar _ | HideWindowUI _
  | FitWindow _ | CenterWindow _ | DisplayDocTitle _
  | RemoveId | OpenAtPageFit _ | OpenAtPage _ | OpenAtPageCustom _ | SetPageLayout _
  | ShowBoxes | TrimMarks | CreateMetadata | SetMetadataDate _ | SetVersion _
  | SetAuthor _|SetTitle _|SetSubject _|SetKeywords _|SetCreate _
  | SetModify _|SetCreator _|SetProducer _|RemoveDictEntry _ | ReplaceDictEntry _ | PrintDictEntry _ | SetMetadata _
  | ExtractText | ExtractImages | ExtractFontFile _
  | AddPageLabels | RemovePageLabels | OutputJSON | OCGCoalesce
  | OCGRename | OCGList | OCGOrderAll | PrintFontEncoding _ | TableOfContents | Typeset _ | Composition _
  | TextWidth _ | SetAnnotations _ | CopyAnnotations _ | ExtractStream _ | PrintObj _ | ReplaceObj _
  | Verify _ | MarkAs _ | RemoveMark _ | ExtractStructTree | ReplaceStructTree _ | SetLanguage _
  | PrintStructTree
     -> false (* Always allowed *)
  (* Combine pages is not allowed because we would not know where to get the
  -recrypt from -- the first or second file? *)
  | Decrypt | Encrypt | CombinePages _ -> true (* Never allowed *)
  | AddBookmarks _ | PadBefore | PadAfter | PadEvery _ | PadMultiple _ | PadMultipleBefore _
  | Merge | Split | SplitOnBookmarks _ | SplitMax _ | Spray | RotateContents _ | Rotate _
  | Rotateby _ | Upright | VFlip | HFlip | Impose _ | Chop _ | ChopHV _ | Redact ->
      mem Pdfcrypt.NoAssemble banlist
  | TwoUp | TwoUpStack | RemoveBookmarks | AddRectangle | RemoveText|
    Draft | Shift | ShiftBoxes | Scale | ScaleToFit|Stretch|CenterToFit|RemoveAttachedFiles|
    RemoveAnnotations|RemoveFonts|Crop|RemoveCrop|Trim|RemoveTrim|Bleed|RemoveBleed|Art|RemoveArt|
    CopyBox|MediaBox|HardBox _|SetTrapped|SetUntrapped|Presentation|
    BlackText|BlackLines|BlackFills|CopyFont _|StampOn _|StampUnder _|StampAsXObject _|
    AddText _|ScaleContents _|AttachFile _| ThinLines _ | RemoveClipping | RemoveAllText
  | Prepend _ | Postpend _ | Draw | ProcessImages ->
      mem Pdfcrypt.NoEdit banlist

let operation_allowed pdf banlist op =
  args.debugforce || 
  match op with
  | None ->
      if args.debugcrypt then Printf.printf "operation is None, so allowed!\n";
      true (* Merge *) (* changed to allow it *)
  | Some op ->
      if args.debugcrypt then Printf.printf "operation_allowed: op = %s\n" (string_of_op op);
      if args.debugcrypt then Printf.printf "Permissions: %s\n" (getpermissions pdf);
      not (banned banlist op)

let decrypt_if_necessary (_, _, user_pw, owner_pw, was_dec_with_owner, _) op pdf =
  if args.debugcrypt then
    begin match op with
      None -> flprint "decrypt_if_necessary: op = None\n"
    | Some x -> Printf.printf "decrypt_if_necessary: op = %s\n" (string_of_op x)
    end;
  if not (Pdfcrypt.is_encrypted pdf) then pdf else
    match op with Some (CombinePages _) ->
      (* This is a hack because we don't have support for recryption on combine
       * pages. This is prevented by permissions above, but in the case that the
       * owner password is blank (e.g christmas_tree_lights.pdf), we would end
       * up here. *)
      soft_error "Combine pages: both files must be unencrypted for this operation, or add -decrypt-force"
    | _ -> 
      match Pdfcrypt.decrypt_pdf_owner owner_pw pdf with
      | Some pdf ->
          args.was_decrypted_with_owner <- true;
          was_dec_with_owner := true;
          if args.debugcrypt then Printf.printf "Managed to decrypt with owner password\n";
          pdf
      | _ ->
        if args.debugcrypt then Printf.printf "Couldn't decrypt with owner password %s\n" owner_pw;
        match
          if args.debugcrypt then Printf.printf "call decrypt_pdf user\n";
          let r = Pdfcrypt.decrypt_pdf user_pw pdf in
          if args.debugcrypt then Printf.printf "returned from decrypt_pdf\n";
          r
        with
        | Some pdf, permissions ->
            if args.debugcrypt then Printf.printf "Managed to decrypt with user password\n";
            if operation_allowed pdf permissions op
              then pdf
              else soft_error "User password cannot give permission for this operation. Supply owner or add -decrypt-force."
        | _ ->
           if args.debugcrypt then Printf.printf "Failed to decrypt with user password: raising soft_error";
           soft_error "Failed to decrypt file: wrong password?"

(* Output Page Count *)
let output_page_count pdf =
  Printf.printf "%i\n" ((if args.fast then Pdfpage.endpage_fast else Pdfpage.endpage) pdf)

let detect_duplicate_op op =
  match args.op with
    None | Some Shift -> ()
  | _ ->
      Pdfe.log (Printf.sprintf "Operation %s already specified, so cannot specify operation %s.\nUse AND from Chapter 1 of the manual to chain commands together.\n"
      (string_of_op (unopt args.op)) (string_of_op op));
      exit 1

let setop op () =
  detect_duplicate_op op;
  args.op <- Some op

let setout name =
  args.out <- File name;
  spray_outputs := name::!spray_outputs

let setchunk c =
  if c > 0
    then args.chunksize <- c
    else error "invalid chunk size"

let fixdashes s =
  let bufferdashes chars =
    let buf = ref [] in
      iter
        (function '-' -> buf =@ [' '; '-'; ' '] | x -> buf =| x)
        chars;
      rev !buf
  in
    let chars = explode s in
      implode (bufferdashes chars)

let set_input_image f s =
  try
    let fh = open_in_bin s in
    let pdf = Cpdfimage.image_of_input ?subformat:args.subformat ?title:args.title ~process_struct_tree:args.process_struct_trees f (Pdfio.input_of_channel fh) in
      begin try close_in fh with _ -> () end;
      args.original_filename <- s;
      args.create_objstm <- true;
      args.inputs <- (AlreadyInMemory (pdf, s), "all", "", "", ref false, None)::args.inputs
  with
    Sys_error _ -> error "Image file not found"

let jbig2_global = ref None

let set_input_png s = set_input_image (fun () -> Cpdfimage.obj_of_png_data) s

let set_input_jpeg s = set_input_image (fun () -> Cpdfimage.obj_of_jpeg_data) s

let set_input_jpeg2000 s = set_input_image (fun () -> Cpdfimage.obj_of_jpeg2000_data) s

let set_input_jbig2 s =
  set_input_image
    (fun () -> Cpdfimage.obj_of_jbig2_data ?global:!jbig2_global) s;
  args.remove_duplicate_streams <- true
let encrypt_to_collect = ref 0

let setmethod s =
  detect_duplicate_op Encrypt;
  if args.op = None then args.op <- Some Encrypt; (* Could be additional to -split *)
  match s with
  | "40bit" | "128bit" | "AES" | "AES256" | "AES256ISO" -> args.crypt_method <- s
  | _ -> error ("Unsupported encryption method " ^ s)

let anon_fun s =
  try
    match !encrypt_to_collect with
    | 3 -> setmethod s; decr encrypt_to_collect
    | 2 -> args.owner <- s; decr encrypt_to_collect
    | 1 -> args.user <- s; decr encrypt_to_collect
    | 0 ->
      let before, after = cleavewhile (neq '=') (explode s) in
        begin match implode before with
        | "user" ->
            begin match args.inputs with
            | [] -> ()
            | (a, b, _, e, f, g)::more ->
                args.inputs <- (a, b, implode (tl after), e, f, g)::more
            end
        | "owner" ->
            begin match args.inputs with
            | [] -> ()
            | (a, b, d, _, f, g)::more ->
                args.inputs <- (a, b, d, implode (tl after), f, g)::more
            end
        | _ -> raise Not_found
        end
    | _ -> assert false
  with
    Not_found ->
      try
        ignore (String.index s '.');
        begin match rev (explode s) with
        | a::b::c::d::e::'.'::r when implode (map Char.uppercase_ascii [e; d; c; b; a]) = "JBIG2" -> set_input_jbig2 s
        | a::b::c::d::'.'::r when implode (map Char.uppercase_ascii [d; c; b; a]) = "JPEG" -> set_input_jpeg s
        | a::b::c::'.'::r when implode (map Char.uppercase_ascii [c; b; a]) = "JPG" -> set_input_jpeg s
        | a::b::c::'.'::r when implode (map Char.uppercase_ascii [c; b; a]) = "JP2" -> set_input_jpeg2000 s
        | a::b::c::'.'::r when implode (map Char.uppercase_ascii [c; b; a]) = "JPX" -> set_input_jpeg2000 s
        | a::b::c::'.'::r when implode (map Char.uppercase_ascii [c; b; a]) = "JPF" -> set_input_jpeg2000 s
        | a::b::c::'.'::r when implode (map Char.uppercase_ascii [c; b; a]) = "PNG" -> set_input_png s
        | _ -> args.inputs <- (InFile s, "all", "", "", ref false, None)::args.inputs
        end;
        args.original_filename <- s
      with
        Not_found ->
          match args.inputs with
          | [] ->
              Pdfe.log (Printf.sprintf "Warning: '%s' ignored\n" s)
          | (a, _, d, e, f, g)::t ->
               args.inputs <- (a, fixdashes s, d, e, f, g)::t

(* If a password begins with a dash, we allow -pw=<password> too *)
let setdashpassword = anon_fun

(* Setting operations *)
let setcrop s =
  setop Crop ();
  args.rectangle <- s

let settrim s =
  setop Trim ();
  args.rectangle <- s

let setbleed s =
  setop Bleed ();
  args.rectangle <- s

let setart s =
  setop Art ();
  args.rectangle <- s

let setmediabox s =
  setop MediaBox ();
  args.rectangle <- s

let setrectangle s =
  setop AddRectangle ();
  args.coord <- s

let setrotate i =
  if i = 0 || i = 90 || i = 180 || i = 270
    then setop (Rotate i) ()
    else error "bad rotation"

let setrotateby i =
  if i = 0 || i = 90 || i = 180 || i = 270
    then setop (Rotateby i) ()
    else error "bad rotation"

let hidetoolbar b =
  try setop (HideToolbar (bool_of_string b)) () with
    _ -> failwith "HideToolBar: must use true or false"

let hidemenubar b =
  try setop (HideMenubar (bool_of_string b)) () with
    _ -> failwith "HideMenuBar: must use true or false"

let hidewindowui b =
  try setop (HideWindowUI (bool_of_string b)) () with
    _ -> failwith "HideWindowUI: must use true or false"

let fitwindow b =
  try setop (FitWindow (bool_of_string b)) () with
    _ -> failwith "FitWindow: must use true or false"

let centerwindow b =
  try setop (CenterWindow (bool_of_string b)) () with
    _ -> failwith "CenterWindow: must use true or false"

let displaydoctitle b =
  try setop (DisplayDocTitle (bool_of_string b)) () with
    _ -> failwith "DisplayDocTitle: must use true or false"

let read_file_size s =
  let read_int s' =
    try int_of_string (implode (rev s')) with
      _ -> error (Printf.sprintf "Could not read file size specification %s" s)
  in
    match rev (explode (String.uppercase_ascii s)) with
    | 'B'::'I'::'G'::s -> 1024 * 1024 * 1024 * read_int s
    | 'B'::'G'::s -> 1000 * 1000 * 1000 * read_int s
    | 'B'::'I'::'M'::s -> 1024 * 1024 * read_int s
    | 'B'::'M'::s -> 1000 * 1000 * read_int s
    | 'B'::'I'::'K'::s -> 1024 * read_int s
    | 'B'::'K'::s -> 1000 * read_int s
    | s -> read_int s

let setsplitmax i = setop (SplitMax (read_file_size i)) ()
let setstdout () = args.out <- Stdout
let setstdin () = args.inputs <- [StdIn, "all", "", "", ref false, None]
let settrans s = args.transition <- Some s
let setduration f = args.duration <- Some f
let setvertical () = args.horizontal <- false
let setoutward () = args.inward <- false
let setdirection i =
  args.direction <-
    match i with
    | 0 | 90 | 180 | 270 | 315 -> i
    | _ -> error "Bad direction"
let seteffectduration f = args.effect_duration <- f
let setcopyid s = setop (CopyId s) ()
let setthinlines s = setop (ThinLines (Cpdfcoord.parse_single_number empty s)) ()

let setcopyannotations s = setop (CopyAnnotations s) ()

let setsetannotations s = setop (SetAnnotations s) ()

let setshift s =
  setop Shift ();
  args.coord <- s

let setshiftboxes s =
  setop ShiftBoxes ();
  args.coord <- s

let setscale s =
  setop Scale ();
  args.coord <- s

let setscaletofit s =
  setop ScaleToFit ();
  args.coord <- s

let setstretch s =
  setop Stretch ();
  args.coord <- s

let setcentertofit s =
  setop CenterToFit ();
  args.coord <- s

let setattachfile s =
  match args.op with
  | Some (AttachFile t) ->
      args.op <- Some (AttachFile (s::t))
  | None ->
      setop (AttachFile [s]) ()
  | Some _ -> detect_duplicate_op (AttachFile [s])

let setextracttextfontsize f =
  args.extract_text_font_size <- Some f

let setfontsize s =
  let f = Cpdfcoord.parse_single_number (Pdf.empty ()) s in
    if f > 0. then args.fontsize <- f else error "Negative font size specified"

let setlinewidth s =
  let f = Cpdfcoord.parse_single_number (Pdf.empty ()) s in
    if f > 0. then args.linewidth <- f else error "Negative line width specified"

let setimposemargin s =
  let f = Cpdfcoord.parse_single_number (Pdf.empty ()) s in
    args.impose_margin <- f

let setimposelinewidth s =
  let f = Cpdfcoord.parse_single_number (Pdf.empty ()) s in
    if f > 0. then args.impose_linewidth <- f else error "Negative impose line width specified" 

let setimposespacing s =
  let f = Cpdfcoord.parse_single_number (Pdf.empty ()) s in
    args.impose_spacing <- f

let setleading s =
  let f = Cpdfcoord.parse_single_number (Pdf.empty ()) s in
    Cpdfdrawcontrol.addop (Cpdfdraw.Leading f)

let setcharspace s =
  let f = Cpdfcoord.parse_single_number (Pdf.empty ()) s in
    Cpdfdrawcontrol.addop (Cpdfdraw.CharSpace f)

let setwordspace s =
  let f = Cpdfcoord.parse_single_number (Pdf.empty ()) s in
    Cpdfdrawcontrol.addop (Cpdfdraw.WordSpace f)

let setrise s =
  let f = Cpdfcoord.parse_single_number (Pdf.empty ()) s in
    Cpdfdrawcontrol.addop (Cpdfdraw.Rise f)

let setaddtext s =
  setop (AddText s) ()

let setcolor s =
  args.color <- Cpdfdrawcontrol.parse_colour s

let setopacity o =
  args.opacity <- o

let setaddbookmarks s =
  setop (AddBookmarks s) ()

let setaddbookmarksjson s =
  setop (AddBookmarks s) ();
  args.format_json <- true

let setlistfontsjson () =
  setop Fonts ();
  args.format_json <- true

let setinfojson () =
  setop Info ();
  args.format_json <- true

let setpageinfojson () =
  setop PageInfo ();
  args.format_json <- true

let setprintpagelabelsjson () =
  setop PrintPageLabels ();
  args.format_json <- true

let setlistbookmarksjson () =
  setop ListBookmarks ();
  args.format_json <- true

let setlistannotationsjson () =
  setop ListAnnotations ();
  args.format_json <- true

let setstampon f =
  setop (StampOn f) ();
  (* Due to an earlier bad decision (default position), we have this nasty hack *)
  if args.position = Cpdfposition.TopLeft (100., 100.) then args.position <- Cpdfposition.BottomLeft (0., 0.)

let setstampunder f =
  setop (StampUnder f) ();
  if args.position = Cpdfposition.TopLeft (100., 100.) then args.position <- Cpdfposition.BottomLeft (0., 0.)

let setstampasxobject f =
  setop (StampAsXObject f) ()

let setcombinepages f =
  setop (CombinePages f) ()

let setposcenter s =
  let x, y = Cpdfcoord.parse_coordinate empty s in
    args.position <- Cpdfposition.PosCentre (x, y)

let setposleft s =
  let x, y = Cpdfcoord.parse_coordinate empty s in
    args.position <- Cpdfposition.PosLeft (x, y)

let setposright s =
  let x, y = Cpdfcoord.parse_coordinate empty s in
    args.position <- Cpdfposition.PosRight (x, y)

let settop n =
  args.position <- Cpdfposition.Top (Cpdfcoord.parse_single_number empty n);
  args.justification <- Cpdfaddtext.CentreJustify

let settopleft n =
  let coord =
    match Cpdfcoord.parse_coordinate empty n with
    | (a, b) -> Cpdfposition.TopLeft (a, b)
    | exception _ ->
        let x = Cpdfcoord.parse_single_number empty n in
          Cpdfposition.TopLeft (x, x)
  in
    args.position <- coord;
    args.justification <- Cpdfaddtext.LeftJustify

let settopright n =
  let coord =
    match Cpdfcoord.parse_coordinate empty n with
    | (a, b) -> Cpdfposition.TopRight (a, b)
    | exception _ ->
        let x = Cpdfcoord.parse_single_number empty n in
          Cpdfposition.TopRight (x, x)
  in
    args.position <- coord;
    args.justification <- Cpdfaddtext.RightJustify

let setleft n =
  args.position <- Cpdfposition.Left (Cpdfcoord.parse_single_number empty n);
  args.justification <- Cpdfaddtext.LeftJustify

let setbottomleft n =
  let coord =
    match Cpdfcoord.parse_coordinate empty n with
    | (a, b) -> Cpdfposition.BottomLeft (a, b)
    | exception _ ->
        let x = Cpdfcoord.parse_single_number empty n in
          Cpdfposition.BottomLeft (x, x)
  in
    args.position <- coord;
    args.justification <- Cpdfaddtext.LeftJustify

let setbottom n =
  args.position <- Cpdfposition.Bottom (Cpdfcoord.parse_single_number empty n);
  args.justification <- Cpdfaddtext.CentreJustify

let setbottomright n =
  let coord =
    match Cpdfcoord.parse_coordinate empty n with
    | (a, b) -> Cpdfposition.BottomRight (a, b)
    | exception _ ->
        let x = Cpdfcoord.parse_single_number empty n in
          Cpdfposition.BottomRight (x, x)
  in
    args.position <- coord;
    args.justification <- Cpdfaddtext.RightJustify

let setright n =
  args.position <- Cpdfposition.Right (Cpdfcoord.parse_single_number empty n);
  args.justification <- Cpdfaddtext.RightJustify

let setdiagonal n =
  args.position <- Cpdfposition.Diagonal;
  args.justification <- Cpdfaddtext.CentreJustify

let setreversediagonal n =
  args.position <- Cpdfposition.ReverseDiagonal;
  args.justification <- Cpdfaddtext.CentreJustify

let setcenter n =
  args.position <- Cpdfposition.Centre;
  args.justification <- Cpdfaddtext.CentreJustify

(* Calculate -bates automatically so that n is applied to the first page in the range *)  
let setbatesrange n =
  let first_page =
    let range = Cpdfpagespec.parse_pagespec_without_pdf (get_pagespec ()) in
      fold_left min max_int range
  in
    args.bates <- n + 1 - first_page

let set_input s =
  args.original_filename <- s;
  args.inputs <- (InFile s, "all", "", "", ref false, None)::args.inputs

let set_json_input s =
  args.original_filename <- s;
  args.create_objstm <- true;
  let fh = open_in_bin s in
  let pdf = Cpdfjson.of_input (Pdfio.input_of_channel fh) in
    close_in fh;
    args.inputs <- (AlreadyInMemory (pdf, s), "all", "", "", ref false, None)::args.inputs

let set_input_dir s =
  let names = sort compare (leafnames_of_dir s) in
  let names =
    if args.idir_only_pdfs then
      option_map
        (fun x ->
          if String.length x > 4 && String.lowercase_ascii (String.sub x (String.length x - 4) 4) = ".pdf"
            then Some x else None)
        names
    else
      names
  in
    args.inputs <-
      (rev
        (map
          (fun n -> (InFile (s ^ Filename.dir_sep ^ n), "all", "", "", ref false, None)) names))
  @ args.inputs

let setdebug () =
  set Pdfread.read_debug;
  set Pdfwrite.write_debug;
  set Pdfcrypt.crypt_debug;
  set Pdfops.debug;
  args.debug <- true

let setboxes () =
  args.boxes <- true

let set_no_encrypt_metadata () =
  args.encrypt_metadata <- false

let set_retain_numbering () =
  args.retain_numbering <- true

let set_remove_duplicate_fonts () =
  args.remove_duplicate_fonts <- true

let setencoding enc () =
  args.encoding <- enc

let setscaletofitscale f =
  args.scale <- f

let setscalecontents f =
  detect_duplicate_op (ScaleContents f);
  args.op <- Some (ScaleContents f);
  args.position <- Cpdfposition.Diagonal (* Will be center *)

let setsqueeze () =
  args.squeeze <- true;
  args.create_objstm <- true

let setcreatoraswego s =
  args.creator <- Some s

let setproduceraswego s =
  args.producer <- Some s

let setprepend s =
  args.op <- Some (Prepend s)

let setpostpend s =
  args.op <- Some (Postpend s)

(* Parsing the control file *)
let rec getuntilendquote prev = function
  | [] -> implode (rev prev), []
  | '"'::t -> implode (rev prev), t
  | '\\'::'"'::t -> getuntilendquote ('"'::prev) t
  | h::t -> getuntilendquote (h::prev) t

let rec getarg prev = function
  | [] -> implode (rev prev), []
  | h::t ->
      if Pdf.is_whitespace h
        then implode (rev prev), t
        else getarg (h::prev) t

let rec parse_chars args = function
  | [] -> rev args
  | h::more when Pdf.is_whitespace h ->
      parse_chars args more
  | '"'::more ->
      let this, rest = getuntilendquote [] more in
        parse_chars (this::args) rest
  | h::t ->
      let this, rest = getarg [] (h::t) in
        parse_chars (this::args) rest

let parse_control_file name =
  (parse_chars []
     (charlist_of_bytes (Pdfio.bytes_of_input_channel (open_in_bin name))))

let parse_control_file_json name =
  try
    match Cpdfyojson.Safe.from_file name with
    | `List ls -> map (function `String s -> s | _ -> raise Exit) ls
    | _ -> raise Exit
  with
    Exit -> error "Syntax error in JSON control file."

let setencryptcollect () =
  encrypt_to_collect := 3

let setcopyfont s =
  detect_duplicate_op (CopyFont s);
  args.op <- Some (CopyFont s)

let setfontpage i =
  args.copyfontpage <- i

let setcopyfontname s =
  args.copyfontname <- Some s

let setpadevery i =
  detect_duplicate_op (PadEvery i);
  if i > 0 then
    args.op <- Some (PadEvery i)
  else
    error "PadEvery: must be > 0"

let setpadwith filename =
  args.padwith <- Some filename

let setpadmultiple i =
  detect_duplicate_op (PadMultiple i);
  args.op <- Some (PadMultiple i)

let setpadmultiplebefore i =
  detect_duplicate_op (PadMultipleBefore i);
  args.op <- Some (PadMultipleBefore i)

let setfast () =
  args.fast <- true

(* Explicitly add a range. Parse it and replace the top input file with the range. *)
let setrange spec =
  args.dashrange <- spec;
  match args.inputs with
    (x, _, c, d, e, f)::more ->
      args.inputs <- (x, spec, c, d, e, f) :: more
  | x -> ()

let setrevision n =
  match args.inputs with
    (a, b, c, d, e, _)::more ->
      args.inputs <- (a, b, c, d, e, Some n) :: more
  | [] ->
      Pdfe.log "Warning. -revision ignored. Put it after the filename.\n"

let setimageresolution f =
  detect_duplicate_op (ImageResolution f);
  args.op <- Some (ImageResolution f)

let setimpath p =
  args.path_to_im <- p

let setjbig2encpath p =
  args.path_to_jbig2enc <- p

let setp2ppath p =
  args.path_to_p2p <- p

let setfrombox s =
  detect_duplicate_op CopyBox;
  args.op <- Some CopyBox;
  args.frombox <- Some s

let settobox s =
  args.tobox <- Some s

let setmediaboxifmissing () =
  args.mediabox_if_missing <- true

let settopage s =
  args.topage <- Some s

let setstdinuser u =
  match args.inputs with
  |  (StdIn, x, _, o, f, g)::t -> args.inputs <- (StdIn, x, u, o, f, g)::t
  | _ -> error "-stdin-user: must follow -stdin"

let setstdinowner o =
  match args.inputs with
  |  (StdIn, x, u, _, f, g)::t -> args.inputs <- (StdIn, x, u, o, f, g)::t
  | _ -> error "-stdin-owner: must follow -stdin"

let setopenatpage n =
  detect_duplicate_op (OpenAtPage n);
  args.op <- Some (OpenAtPage n)

let setopenatpagefit n =
  detect_duplicate_op (OpenAtPageFit n);
  args.op <- Some (OpenAtPageFit n)

let setopenatpagecustom n =
  detect_duplicate_op (OpenAtPageCustom n);
  args.op <- Some (OpenAtPageCustom n)

let setlabelstyle s =
  let style =
    match s with
    | "DecimalArabic" -> Pdfpagelabels.DecimalArabic
    | "UppercaseRoman" -> Pdfpagelabels.UppercaseRoman
    | "LowercaseRoman" -> Pdfpagelabels.LowercaseRoman
    | "UppercaseLetters" -> Pdfpagelabels.UppercaseLetters
    | "LowercaseLetters" -> Pdfpagelabels.LowercaseLetters
    | "NoLabelPrefixOnly" -> Pdfpagelabels.NoLabelPrefixOnly
    | _ -> error "Unknown label style"
  in
    args.labelstyle <- style

let setlabelprefix s =
  args.labelprefix <- Some s

let setlabelstartval i =
  args.labelstartval <- i

let setlabelsprogress () =
  args.labelsprogress <- true

let setcpdflin s = 
  args.cpdflin <- Some s

let setrecrypt () =
  args.recrypt <- true

let setremovedictentry s =
  detect_duplicate_op (RemoveDictEntry s);
  args.op <- Some (RemoveDictEntry s)

let logto = ref None

let setsqueezelogto s =
  logto := Some s

let setstayonerror () =
  set stay_on_error

let setembedstd14 s =
  args.embedstd14 <- Some s

let _ =
  Cpdfdrawcontrol.setembedstd14 := (fun b dir -> if b then args.embedstd14 <- Some dir else args.embedstd14 <- None)

let sethardbox box =
  detect_duplicate_op (HardBox box);
  args.op <- Some (HardBox box)

let setalsosetxml () =
  args.alsosetxml <- true

let setjustsetxml () =
  args.justsetxml <- true

let setsetmetadatadate d =
  detect_duplicate_op (SetMetadataDate d);
  args.op <- Some (SetMetadataDate d)

let setgsmalformed () =
  args.gs_malformed <- true

let setmergeaddbookmarks () =
  args.merge_add_bookmarks <- true

let setmergeaddbookmarksusetitles () =
  args.merge_add_bookmarks_use_titles <- true

let setbookmarksopentolevel l =
  detect_duplicate_op (BookmarksOpenToLevel l);
  args.op <- Some (BookmarksOpenToLevel l)

let setcreatepdfpages i =
  args.createpdf_pages <- i

let setcreatepdfpapersize s =
  args.createpdf_pagesize <-
    let w, h = Cpdfcoord.parse_coordinate (Pdf.empty ()) s in
    Pdfpaper.make Pdfunits.PdfPoint w h

let setimpose s =
  setop (Impose true) ();
  args.coord <- s

let setimposexy s =
  setop (Impose false) ();
  args.coord <- s

let setchop s =
  let x, y = Cpdfcoord.parse_coordinate empty s in
  setop (Chop (int_of_float x, int_of_float y)) ()

let setchopv x =
  setop (ChopHV (false, Cpdfcoord.parse_single_number (Pdf.empty ()) x)) ()

let setchoph y =
  setop (ChopHV (true, Cpdfcoord.parse_single_number (Pdf.empty ()) y)) ()

let setreplacedictentry s =
  setop (ReplaceDictEntry s) ()

let setprintdictentry s =
  setop (PrintDictEntry s) ()

let setreplacedictentryvalue s =
  try
    let pdfobj = Cpdfjson.object_of_json (Cpdfyojson.Safe.from_string s) in
      args.replace_dict_entry_value <- pdfobj
  with
    e -> error (Printf.sprintf "Failed to parse replacement value: %s\n" (Printexc.to_string e))

let setdictentrysearch s =
  try
    let pdfobj = Cpdfjson.object_of_json (Cpdfyojson.Safe.from_string s) in
      args.dict_entry_search <- Some pdfobj
  with
    e -> error (Printf.sprintf "Failed to parse search term: %s\n" (Printexc.to_string e))

let setprintfontencoding s =
  setop (PrintFontEncoding s) ()

let settypeset s =
  setop (Typeset s) ()

let setsubformat s =
  args.subformat <- Some (Cpdfua.subformat_of_string s)

let settableofcontentstitle s =
  args.toc_title <- s

let settocnobookmark () =
  args.toc_bookmark <- false

let setidironlypdfs () =
  args.idir_only_pdfs <- true

let setnowarnrotate () =
  args.no_warn_rotate <- true

let whingemalformed () =
  Pdfe.log "Command line must be of exactly the form\ncpdf <infile> -gs <path> -gs-malformed-force -o <outfile>\n";
  exit 1

let addop o =
  begin match o with Cpdfdraw.FontPack _ -> set Cpdfdrawcontrol.fontpack_initialised | _ -> () end;
  begin match args.op with Some Draw -> () | _ -> error "Need to be in drawing mode for this." end;
  Cpdfdrawcontrol.addop o

let embed_font_inner font =
  match font with
  | StandardFont f ->
    (*  Printf.printf "embed_font: StandardFont\n";*)
      begin match args.embedstd14 with
      | Some dirname -> 
          begin try
            let fontfile, fontname = Cpdfembed.load_substitute dirname f in
              Cpdfembed.EmbedInfo {fontfile; fontname; encoding = args.fontencoding}
          with
            e -> error (Printf.sprintf "Can't load font for embedding: %s\n" (Printexc.to_string e))
          end
      | None -> 
          PreMadeFontPack (Cpdfembed.fontpack_of_standardfont (Pdftext.StandardFont (f, args.fontencoding)))
      end
  | OtherFont f ->
      ExistingNamedFont
  | EmbeddedFont name ->
      (*Printf.printf "embed_font: TTF\n";*)
      try
        let fontname, font = Hashtbl.find Cpdfdrawcontrol.ttfs name in
          args.fontname <- fontname;
          font
      with
        Not_found -> error (Printf.sprintf "Font %s not found" name)

let embed_font () = embed_font_inner args.font

let _ = Cpdfdrawcontrol.embed_font := embed_font

let _ = Cpdfdrawcontrol.setdrawing := (fun () -> args.op <- Some Draw)

let setfont f =
  (*Printf.printf "Cpdfcommand.setfont: |%s|\n%!" f;*)
  try
    let fontname, _ = Hashtbl.find Cpdfdrawcontrol.ttfs f in
      args.font <- EmbeddedFont f;
      args.fontname <- fontname
  with
    Not_found ->
      let convert f = (* convert from written PDF representation to internal PDF string e.g # sequences *)
        match Pdfread.lex_name (Pdfio.input_of_string f) with Pdfgenlex.LexName s -> s | _ -> assert false
      in
      args.font <-
        begin match Pdftext.standard_font_of_name ("/" ^ f) with
        | Some x -> StandardFont x
        | None ->
            if f <> "" && hd (explode f) <> '/' then error "Font not found";
            OtherFont (convert f)
        end;
      args.fontname <-
        begin match Pdftext.standard_font_of_name ("/" ^ f) with
        | Some x -> f
        | None -> convert f
        end;
      (* If drawing, add the font pack as an op. *)
      begin match args.op with Some Draw -> addop (Cpdfdraw.FontPack (f, embed_font (), null_hash ())) | _ -> () end

let loadttf n =
  (*Printf.printf "loadttf: %s\n" n;*)
  let name, filename =
    match String.split_on_char '=' n with
    | [name; filename] -> name, filename
    | _ -> error "loadttf: bad file specification. Should be <name>=<filename>"
  in
    try
      let fontfile = Pdfio.bytes_of_string (contents_of_file filename) in
      let fontname = Filename.remove_extension (Filename.basename filename) in 
        Hashtbl.replace
          Cpdfdrawcontrol.ttfs
          name
          (fontname, Cpdfembed.EmbedInfo {fontfile; fontname; encoding = args.fontencoding});
        (* If drawing, add the font pack as an op. *)
        begin match args.op with
          Some Draw -> addop (Cpdfdraw.FontPack (fontname, embed_font_inner (EmbeddedFont name), null_hash ())) | _ -> () end
    with
      _ -> error "addtff: could not load TTF"

let () = Cpdfdrawcontrol.loadttf := loadttf

let setstderrtostdout () =
  Pdfe.logger := (fun s -> print_string s; flush stdout)

let settextwidth s =
  args.op <- Some (TextWidth s)

let setdraw () =
  args.op <- Some Draw

let setdrawstructtree () =
  args.draw_struct_tree <- true

let setextractfontfile s =
  args.op <- Some (ExtractFontFile s)

let () = Cpdfdrawcontrol.getfontname := fun () -> args.fontname
let () = Cpdfdrawcontrol.getfontsize := fun () -> args.fontsize
let () = Cpdfdrawcontrol.setfontname := setfont
let () = Cpdfdrawcontrol.setfontsize := fun s -> args.fontsize <- s
let () = Cpdfdrawcontrol.getindent := fun () -> args.indent

let setlistimagesjson () =
  setop ListImages ();
  args.format_json <- true

let set_jbig2_global f =
  jbig2_global := Some (Pdfio.bytes_of_string (contents_of_file f))

let clear_jbig2_global () =
  jbig2_global := None

let setjpegquality q =
  args.jpegquality <- q
 
let setjpegqualitylossless q =
  args.jpegqualitylossless <- q

let setjpegtojpegscale q =
  args.jpegtojpegscale <- q

let setjpegtojpegdpi q =
  args.jpegtojpegdpi <- q

let set1bppmethod m =
  args.onebppmethod <- m

let setpixelthreshold i =
  args.pixel_threshold <- i

let setlengththreshold i =
  args.length_threshold <- i

let setpercentagethreshold i =
  args.percentage_threshold <- i

let setdpithreshold i =
  args.dpi_threshold <- i

let setlosslessresample i =
  args.resample_factor <- i

let setlosslessresampledpi i =
  args.resample_factor <- -.i

let setresampleinterpolate () =
  args.resample_interpolate <- true

let setjbig2_lossy_threshold f =
  args.jbig2_lossy_threshold <- f

let setprocessimagesinfo () =
  set Cpdfimage.debug_image_processing

let setextractstream s =
  args.op <- Some (ExtractStream s)

let setextractstreamdecomp s =
  args.op <- Some (ExtractStream s);
  args.extract_stream_decompress <- true

let setprintobj s =
  args.op <- Some (PrintObj s)

let setprintobjjson s =
  args.format_json <- true;
  args.op <- Some (PrintObj s)

let setreplaceobj s =
  match String.split_on_char '=' s with
  | [a; b] -> args.op <- Some (ReplaceObj (a, b))
  | _ -> error "replace_obj: bad specification"

let expand_namespace = function
  | "PDF" -> standard_namespace
  | "PDF2" -> pdf2_namespace
  | x -> x

let setreadableops () =
  Pdfops.whitespace := "\n";
  Pdfops.always_add_whitespace := true;
  Pdfops.write_comments := true

let addeltinfo s =
  match String.split_on_char '=' s with
  | h::t ->
      let pdfobj = Pdfread.parse_single_object (String.concat "" t) in
        Cpdfdrawcontrol.eltinfo h pdfobj
  | [] -> error "addeltinfo: bad format"

let specs =
   [("-version",
      Arg.Unit (setop Version),
      " Print the cpdf version number");
   ("-o",
       Arg.String setout,
       " Set the output file, if appropriate");
   ("-i",
       Arg.String set_input,
       " Add an input file");
   ("-png",
       Arg.String set_input_png,
       " Load from a PNG file, converting to PDF");
   ("-jpeg",
       Arg.String set_input_jpeg,
       " Load from a JPEG file, converting to PDF");
   ("-jpeg2000",
       Arg.String set_input_jpeg2000,
       " Load from a JPEG2000 file, converting to PDF");
   ("-jbig2",
       Arg.String set_input_jbig2,
       " Load from a JBIG2 fragment, converting to PDF");
   ("-jbig2-global",
       Arg.String set_jbig2_global,
       " Load a JBIG2 global stream");
   ("-jbig2-global-clear",
       Arg.Unit clear_jbig2_global,
       " Forget any JBIG2 global stream");
   ("-idir",
       Arg.String set_input_dir,
       " Add a directory of files");
   ("-idir-only-pdfs",
       Arg.Unit setidironlypdfs,
       " Have -idir ignore files not ending in .pdf");
   ("-pw",
       Arg.String setdashpassword,
       " Supply a password explicitly -pw=<password>");
   ("-stdin",
       Arg.Unit setstdin,
       " Read input from standard input");
   ("-stdin-owner",
       Arg.String setstdinowner,
       " Owner password for -stdin");
   ("-stdin-user",
       Arg.String setstdinuser,
       " User password for -stdin");
   ("-stdout",
       Arg.Unit setstdout,
       " Send result to standard output");
   ("-error-on-malformed",
       Arg.Set Pdfread.error_on_malformed,
       " Do not try to read malformed files");
   ("-range",
      Arg.String setrange,
      " Explicitly add a range");
   ("-collate",
      Arg.Unit (fun () -> args.collate <- true),
      " Collate ranges when merging");
   ("-revision",
      Arg.Int setrevision,
      "");
   ("-change-id",
      Arg.Unit (setop ChangeId),
      " Change the file's /ID tag");
   ("-no-preserve-objstm",
      Arg.Unit (fun () -> args.preserve_objstm <- false),
      " Don't preserve object streams");
   ("-create-objstm",
      Arg.Unit (fun () -> args.create_objstm <- true),
      " Create object streams anew");
   ("-keep-version",
      Arg.Unit (fun () -> args.keepversion <- true),
      " Don't change the version number");
   ("-l",
       Arg.Unit (fun () -> args.linearize <- true),
       " Linearize output file");
   ("-keep-l",
       Arg.Unit (fun () -> args.keeplinearize <- true),
       " Linearize if the input file was linearized");
   ("-cpdflin",
       Arg.String setcpdflin,
       " Set location of 'cpdflin'");
   ("-recrypt",
       Arg.Unit setrecrypt,
       " Keep this file's encryption when writing");
   ("-raw",
      Arg.Unit (setencoding Cpdfmetadata.Raw),
      " Do not process text");
   ("-stripped",
      Arg.Unit (setencoding Cpdfmetadata.Stripped),
      " Process text by simple stripping to ASCII");
   ("-utf8",
      Arg.Unit (setencoding Cpdfmetadata.UTF8),
      " Process text by conversion to UTF8 Unicode");
   ("-fast",
      Arg.Unit setfast,
      " Speed over correctness with malformed documents");
   ("-args",
      Arg.Unit (fun () -> ()),
      " Get arguments from a file.");
   ("-args-json",
      Arg.Unit (fun () -> ()),
      " Get arguments from a JSON file.");
   ("-merge",
       Arg.Unit (setop Merge),
       " Merge a number of files into one");
   ("-retain-numbering",
       Arg.Unit set_retain_numbering,
       " Don't renumber pages when merging");
   ("-merge-add-bookmarks",
       Arg.Unit setmergeaddbookmarks,
       " Add bookmarks for each file to merged file");
   ("-merge-add-bookmarks-use-titles",
       Arg.Unit setmergeaddbookmarksusetitles,
       " Use title of document rather than filename");
   ("-process-struct-trees",
       Arg.Unit (fun () -> args.process_struct_trees <- true),
       " Process structure trees");
   ("-remove-duplicate-fonts",
       Arg.Unit set_remove_duplicate_fonts,
       " Remove duplicate fonts when merging");
   ("-split",
       Arg.Unit (setop Split),
       " Split a file into individual pages");
   ("-chunk",
       Arg.Int setchunk,
       " Set chunk size for -split (default 1)");
   ("-split-bookmarks",
       Arg.Int (fun i -> setop (SplitOnBookmarks i) ()),
       " Split a file at bookmarks at a given level");
   ("-split-max",
       Arg.String setsplitmax,
       " Split a file to files of a given size");
   ("-spray",
       Arg.Unit (setop Spray),
       " Split a file by alternating pages");
   ("-scale-page",
      Arg.String setscale,
      " -scale-page \"sx sy\" scales by (sx, sy)");
   ("-scale-to-fit",
      Arg.String setscaletofit,
      " -scale-to-fit \"x y\" scales to page size (x, y)");
   ("-stretch",
      Arg.String setstretch,
      " -stretch \"x y\" scales without preserving aspect ratio");
   ("-center-to-fit",
      Arg.String setcentertofit,
      " -center-to-fit \"x y\" centers pages on page size (x, y)");
   ("-scale-contents",
      Arg.Float setscalecontents,
      "  Scale contents by the given factor");
   ("-scale-to-fit-scale",
      Arg.Float setscaletofitscale,
      " -scale-to-fit-scale (1.0 = 100%)");
   ("-shift",
      Arg.String setshift,
      " -shift \"dx dy\" shifts the chosen pages");
   ("-shift-boxes",
      Arg.String setshiftboxes,
      " -shift-boxes \"dx dy\" shifts boxes on the chosen pages");
   ("-rotate",
       Arg.Int setrotate,
       " Set rotation of pages to 0, 90, 180, 270");
   ("-rotateby",
       Arg.Int setrotateby,
       " Rotate pages by 90, 180 or 270 degrees");
   ("-rotate-contents",
       Arg.Float (fun f -> setop (RotateContents f) ()),
       " Rotate contents of pages");
   ("-upright",
       Arg.Unit (setop Upright),
       " Make pages upright");
   ("-prerotate",
      Arg.Unit (fun () -> args.prerotate <- true),
      " Calls -upright on pages before modifying them, if required");
   ("-no-warn-rotate",
      Arg.Unit setnowarnrotate,
      " Do not warn on pages of PDFs which are not upright");
   ("-hflip",
       Arg.Unit (setop HFlip),
       " Flip pages horizontally");
   ("-vflip",
       Arg.Unit (setop VFlip),
       " Flip pages vertically");
   ("-crop",
       Arg.String setcrop,
       " Crop specified pages (synonym for -cropbox)");
   ("-cropbox",
       Arg.String setcrop,
       " Crop specified pages");
   ("-artbox",
       Arg.String setart,
       " Set art box for specified pages");
   ("-bleedbox",
       Arg.String setbleed,
       " Set bleed box for specified pages");
   ("-trimbox",
       Arg.String settrim,
       " Set trim box for specified pages");
   ("-hard-box",
       Arg.String sethardbox,
       " Hard crop specified pages to the given box");
   ("-show-boxes",
       Arg.Unit (setop ShowBoxes),
       " Show boxes by adding rectangles to pages");
   ("-trim-marks",
       Arg.Unit (setop TrimMarks),
       " Add trim marks");
   ("-remove-crop",
       Arg.Unit (setop RemoveCrop),
       " Remove cropping on specified pages");
   ("-remove-cropbox",
       Arg.Unit (setop RemoveCrop),
       " Synonym for -remove-crop");
   ("-remove-trimbox",
       Arg.Unit (setop RemoveTrim),
       " Remove trim box on specified pages");
   ("-remove-bleedbox",
       Arg.Unit (setop RemoveBleed),
       " Remove bleed box on specified pages");
   ("-remove-artbox",
       Arg.Unit (setop RemoveArt),
       " Remove art box on specified pages");
   ("-frombox", Arg.String setfrombox, " Set box to copy from");
   ("-tobox", Arg.String settobox, " Set box to copy to");
   ("-mediabox-if-missing",
    Arg.Unit setmediaboxifmissing,
    " If copy from box missing, substitute media box");
   ("-mediabox",
       Arg.String setmediabox,
       " Set media box on specified pages");
   ("-encrypt",
       Arg.Unit setencryptcollect,
       " Encrypt a document");
   ("-decrypt",
      Arg.Unit (setop Decrypt),
      " Decrypt a file");
   ("-decrypt-force",
      Arg.Unit (fun () -> args.debugforce <- true),
      " Decrypt a file even without password");
   ("-no-edit", Arg.Unit (fun () -> args.no_edit <- true) , " No edits");
   ("-no-print", Arg.Unit (fun () -> args.no_print <- true), " No printing");
   ("-no-copy", Arg.Unit (fun () -> args.no_copy <- true), " No copying");
   ("-no-annot", Arg.Unit (fun () -> args.no_annot <- true), " No annotations");
   ("-no-forms", Arg.Unit (fun () -> args.no_forms <- true), " No forms");
   ("-no-extract", Arg.Unit (fun () -> args.no_extract <- true), " No extracting");
   ("-no-assemble", Arg.Unit (fun () -> args.no_assemble <- true), " No assembling");
   ("-no-hq-print", Arg.Unit (fun () -> args.no_hq_print <- true), " No high quality printing");
   ("-no-encrypt-metadata",
       Arg.Unit set_no_encrypt_metadata,
       " Don't encrypt metadata (AES only)");
   ("-decompress",
       Arg.Unit (setop Decompress),
       " Decompress");
   ("-compress",
       Arg.Unit (setop Compress),
       " Compress streams, leaving metadata alone");
   ("-remove-duplicate-streams",
       Arg.Unit (fun () -> args.remove_duplicate_streams <- true),
       "");
   ("-list-bookmarks",
      Arg.Unit (setop ListBookmarks),
      " List Bookmarks");
   ("-list-bookmarks-json",
      Arg.Unit setlistbookmarksjson,
      " List Bookmarks in JSON format");
   ("-remove-bookmarks",
      Arg.Unit (setop RemoveBookmarks),
      " Remove bookmarks from a file");
   ("-add-bookmarks",
      Arg.String setaddbookmarks,
      " Add bookmarks from the given file");
   ("-add-bookmarks-json",
      Arg.String setaddbookmarksjson,
      " Add bookmarks from the given file in JSON format");
   ("-bookmarks-open-to-level",
      Arg.Int setbookmarksopentolevel,
      " Open bookmarks to this level (0 = all closed)");
   ("-presentation",
      Arg.Unit (setop Presentation),
      " Make a presentation");
   ("-trans",
      Arg.String settrans,
      " Set the transition method for -presentation");
   ("-duration",
      Arg.Float setduration,
      " Set the display duration for -presentation");
   ("-vertical",
      Arg.Unit setvertical,
      " Set dimension for Split and Blinds styles");
   ("-outward",
      Arg.Unit setoutward,
      " Set direction for Split and Box styles");
   ("-direction",
      Arg.Int setdirection,
      " Set direction for Wipe and Glitter styles");
   ("-effect-duration",
      Arg.Float seteffectduration,
      " Set the effect duration in seconds");
   ("-stamp-on",
      Arg.String setstampon,
      " Stamp a file on some pages of another");
   ("-stamp-under",
      Arg.String setstampunder,
      " Stamp a file under some pages of another");
   ("-scale-stamp-to-fit",
      Arg.Unit (fun () -> args.scale_stamp_to_fit <- true),
      " Scale the stamp to fit the page");
   ("-combine-pages",
      Arg.String setcombinepages,
      " Combine two files by merging individual pages");
   ("-add-text",
      Arg.String setaddtext,
      " Superimpose text on the given range of pages");
   ("-remove-text",
      Arg.Unit (setop RemoveText),
      " Remove text previously added by cpdf");
   ("-add-rectangle",
      Arg.String setrectangle,
      " Add a rectangle to the page");
   ("-bates",
      Arg.Int (fun n -> args.bates <- n),
      " Set the base bates number");
   ("-bates-at-range",
      Arg.Int setbatesrange,
      " Set the base bates number at first page in range");
   ("-bates-pad-to",
      Arg.Int (fun n -> args.batespad <- Some n),
      " Pad the bates number with leading zeroes to width");
   ("-font",
      Arg.String setfont,
      " Set the font");
   ("-font-size",
      Arg.String setfontsize,
      " Set the font size");
   ("-load-ttf",
      Arg.String loadttf,
      " Use a TrueType font");
   ("-embed-std14",
      Arg.String setembedstd14,
      " Embed standard 14 fonts");
   ("-color",
      Arg.String setcolor,
      " Set the color");
   ("-opacity",
      Arg.Float setopacity,
      " Set the text opacity");
   ("-outline",
      Arg.Unit (fun () -> args.outline <- true),
      " Use outline mode for text");
   ("-linewidth",
      Arg.String setlinewidth,
      " Set line width for outline text");
   ("-pos-center",
      Arg.String setposcenter,
      " Set position relative to center of baseline");
   ("-pos-left",
      Arg.String setposleft,
      " Set position relative to left of baseline");
   ("-pos-right",
      Arg.String setposright,
      " Set position relative to right of baseline");
   ("-top",
      Arg.String settop,
      " Set position relative to center top of page");
   ("-topleft",
      Arg.String settopleft,
      " Set position relative to top left of page");
   ("-topright",
      Arg.String settopright,
      " Set position relative to top right of page");
   ("-left",
      Arg.String setleft,
      " Set position relative to center left of page");
   ("-bottomleft",
      Arg.String setbottomleft,
      " Set position relative to bottom left of page");
   ("-bottom",
      Arg.String setbottom,
      " Set position relative to center bottom of page");
   ("-bottomright",
      Arg.String setbottomright,
      " Set position relative to bottom right of page");
   ("-right",
      Arg.String setright,
      " Set position relative to center right of page");
   ("-diagonal",
      Arg.Unit setdiagonal,
      " Place text diagonally across page");
   ("-reverse-diagonal",
      Arg.Unit setreversediagonal,
      " Place text diagonally across page from top left");
   ("-center",
      Arg.Unit setcenter,
      " Place text in the center of the page");
   ("-justify-left",
      Arg.Unit (fun () -> args.justification <- Cpdfaddtext.LeftJustify),
      " Justify multiline text left");
   ("-justify-right",
      Arg.Unit (fun () -> args.justification <- Cpdfaddtext.RightJustify),
      " Justify multiline text right");
   ("-justify-center",
      Arg.Unit (fun () -> args.justification <- Cpdfaddtext.CentreJustify),
      " Justify multiline text center");
   ("-underneath",
      Arg.Unit (fun () -> args.underneath <- true),
      " Text stamp is underneath content");
   ("-line-spacing",
      Arg.Float (fun f -> args.linespacing <- f),
      " Line spacing (1 is normal)");
   ("-midline",
      Arg.Unit (fun () -> args.midline <- true),
      " Adjust text to midline rather than baseline");
   ("-topline",
      Arg.Unit (fun () -> args.topline <- true),
      " Adjust text to topline rather than baseline");
   ("-relative-to-cropbox",
      Arg.Unit (fun () -> args.relative_to_cropbox <- true),
      " Add text relative to Crop Box not Media Box");
   ("-embed-missing-fonts",
      Arg.Unit (setop EmbedMissingFonts),
      " Embed missing fonts by calling gs");
   ("-twoup",
      Arg.Unit (setop TwoUp),
      " Put 2 pages onto one");
   ("-twoup-stack",
      Arg.Unit (setop TwoUpStack),
      " Stack 2 pages onto one twice the size");
   ("-impose",
      Arg.String setimpose,
      " Impose onto given page size");
   ("-impose-xy",
      Arg.String setimposexy,
      " Impose x by y (zero means unlimited)");
   ("-impose-columns",
      Arg.Unit (fun () -> args.impose_columns <- true),
      " Impose in columns rather than rows");
   ("-impose-rtl",
      Arg.Unit (fun () -> args.impose_rtl <- true),
      " Impose right-to-left");
   ("-impose-btt",
      Arg.Unit (fun () -> args.impose_btt <- true),
      " Impose bottom-to-top");
   ("-impose-margin",
      Arg.String setimposemargin,
      " Add margin around whole imposed page");
   ("-impose-spacing",
      Arg.String setimposespacing,
      " Add spacing around each imposed page");
   ("-impose-linewidth",
      Arg.String setimposelinewidth,
      " Imposition divider line width (0=none)");
   ("-chop",
      Arg.String setchop,
      " Chop x by y");
   ("-chop-h",
      Arg.String setchoph,
      " Chop horizontally");
   ("-chop-v",
      Arg.String setchopv,
      " Chop horizontally");
   ("-chop-columns",
      Arg.Unit (fun () -> args.impose_columns <- true),
      " Chop in columns rather than rows");
   ("-chop-rtl",
      Arg.Unit (fun () -> args.impose_rtl <- true),
      " Chop right-to-left");
   ("-chop-btt",
      Arg.Unit (fun () -> args.impose_btt <- true),
      " Chop bottom-to-top");
   ("-pad-before",
      Arg.Unit (setop PadBefore),
      " Add a blank page before the given pages");
   ("-pad-after",
      Arg.Unit (setop PadAfter),
      " Add a blank page after the given pages");
   ("-pad-every",
      Arg.Int setpadevery,
      " Add a blank page after every n pages");
   ("-pad-with",
      Arg.String setpadwith,
      " Use a given PDF instead of a blank page");
   ("-pad-multiple",
      Arg.Int setpadmultiple,
      " Pad the document to a multiple of n pages");
   ("-pad-multiple-before",
      Arg.Int setpadmultiplebefore,
      " Pad the document at beginning to a multiple of n pages");
   ("-list-annotations",
      Arg.Unit (setop ListAnnotations),
      " List annotations");
   ("-list-annotations-json",
      Arg.Unit setlistannotationsjson,
      " List annotations in JSON format");
   ("-copy-annotations",
      Arg.String setcopyannotations,
      " Copy annotations from given file");
   ("-remove-annotations",
      Arg.Unit (setop RemoveAnnotations),
      " Remove annotations");
   ("-set-annotations",
      Arg.String setsetannotations,
      " Set annotations from JSON file");
   ("-list-fonts",
       Arg.Unit (setop Fonts),
       " Output font list");
   ("-list-fonts-json",
       Arg.Unit setlistfontsjson,
       " Output font list in JSON format");
   ("-info",
       Arg.Unit (setop Info),
       " Output file information");
   ("-info-json",
       Arg.Unit setinfojson,
       " Output file information in JSON format");
   ("-page-info",
       Arg.Unit (setop PageInfo),
       " Output page information");
   ("-page-info-json",
       Arg.Unit setpageinfojson,
       " Output page information in JSON format");
   ("-set-author",
       Arg.String (fun s -> setop (SetAuthor s) ()),
       " Set Author");
   ("-set-title",
      Arg.String (fun s ->  setop (SetTitle s) ()),
      " Set Title");
   ("-set-subject",
      Arg.String (fun s -> setop (SetSubject s) ()),
      " Set Subject");
   ("-set-keywords",
      Arg.String (fun s -> setop (SetKeywords s) ()),
      " Set Keywords");
   ("-set-create",
      Arg.String (fun s -> setop (SetCreate s) ()),
      " Set Creation date");
   ("-set-modify",
      Arg.String (fun s -> setop (SetModify s) ()),
      " Set Modification date");
   ("-set-creator",
      Arg.String (fun s -> setop (SetCreator s) ()),
      " Set Creator");
   ("-set-producer",
      Arg.String (fun s -> setop (SetProducer s) ()),
      " Set Producer");
   ("-set-trapped",
      Arg.Unit (setop SetTrapped),
      " Mark as trapped");
   ("-set-untrapped",
      Arg.Unit (setop SetUntrapped),
      " Mark as not trapped");
   ("-also-set-xmp",
      Arg.Unit setalsosetxml,
      " Also set XMP metadata");
   ("-just-set-xmp",
      Arg.Unit setjustsetxml,
      " Just set XMP metadata, not old-fashioned metadata");
   ("-create-metadata",
      Arg.Unit (setop CreateMetadata),
      " Create XMP metadata from scratch.");
   ("-set-page-layout",
      Arg.String (fun s -> setop (SetPageLayout s) ()),
      " Set page layout upon document opening");
   ("-set-page-mode",
      Arg.String (fun s -> setop (SetPageMode s) ()),
      " Set page mode upon document opening");
   ("-set-non-full-screen-page-mode",
      Arg.String (fun s -> setop (SetNonFullScreenPageMode s) ()),
      " Set non full screen page mode if page mode is FullScreen");
   ("-open-at-page",
      Arg.String setopenatpage,
      " Set initial page");
   ("-open-at-page-fit",
      Arg.String setopenatpagefit,
      " Set initial page, scaling to fit");
   ("-open-at-page-custom",
      Arg.String setopenatpagecustom,
      " Set initial page, with custom scaling");
   ("-set-metadata",
      Arg.String (fun s -> setop (SetMetadata s) ()),
      " Set metadata to the contents of a file");
   ("-print-metadata",
       Arg.Unit (setop Metadata),
       " Output metadata information");
   ("-remove-metadata",
       Arg.Unit (setop RemoveMetadata),
       " Remove document metadata");
   ("-set-metadata-date",
       Arg.String setsetmetadatadate,
       " Set the XMP metadata date property");
   ("-hide-toolbar",
      Arg.String hidetoolbar,
      " Hide the viewer's toolbar");
   ("-hide-menubar",
      Arg.String hidemenubar,
      " Hide the viewer's menubar");
   ("-hide-window-ui",
      Arg.String hidewindowui,
      " Hide the viewer's scroll bars etc.");
   ("-fit-window",
      Arg.String fitwindow,
      " Resize document's window to fit size of page");
   ("-center-window",
      Arg.String centerwindow,
      " Position window in the center of screen");
   ("-display-doc-title",
      Arg.String displaydoctitle,
      " Display document's title in the title bar");
   ("-set-language",
      Arg.String (fun s -> setop (SetLanguage s) ()),
      " Set the document's language");
   ("-pages",
      Arg.Unit (setop CountPages),
      " Count pages");
   ("-list-attached-files",
      Arg.Unit (setop ListAttachedFiles),
      " List attached files");
   ("-dump-attachments",
      Arg.Unit (setop DumpAttachedFiles),
      " Dump attachments to disk");
   ("-attach-file",
      Arg.String setattachfile,
      " Attach a file");
   ("-to-page",
      Arg.String settopage,
      " Attach file to given page instead of document");
   ("-remove-files",
      Arg.Unit (setop RemoveAttachedFiles),
      " Remove embedded attached document-level files");
   ("-list-images",
      Arg.Unit (setop ListImages),
      " List images");
   ("-list-images-json",
      Arg.Unit setlistimagesjson,
      " List images in JSON format");
   ("-list-images-used",
      Arg.Unit (fun () -> setop (ImageResolution max_float) ()),
      " List images at point of use");
   ("-list-images-used-json",
      Arg.Unit (fun () -> args.format_json <- true; setop (ImageResolution max_float) ()),
      " List images at point of use in JSON format");
   ("-image-resolution",
      Arg.Float setimageresolution,
      " List images at point of use under a given dpi");
   ("-image-resolution-json",
      Arg.Float (fun f -> setimageresolution f; args.format_json <- true),
      " List images at point of use under a given dpi");
   ("-copy-font",
      Arg.String setcopyfont,
      " Copy a named font");
   ("-copy-font-page",
      Arg.Int setfontpage,
      " Set the page a copied font is drawn from");
   ("-copy-font-name",
      Arg.String setcopyfontname,
      " Set the name of the font to copy");
   ("-remove-fonts",
      Arg.Unit (setop RemoveFonts),
      " Remove embedded fonts");
   ("-missing-fonts",
      Arg.Unit (setop MissingFonts),
      " List missing fonts");
   ("-remove-id",
      Arg.Unit (setop RemoveId),
      " Remove the file's /ID tag");
   ("-draft",
      Arg.Unit (setop Draft),
      " Remove images from the file");
   ("-draft-remove-only",
      Arg.String (fun s -> args.removeonly <- Some s),
      " Only remove named image");
   ("-boxes",
      Arg.Unit setboxes,
      " Add crossed boxes to -draft option");
   ("-remove-all-text",
      Arg.Unit (setop RemoveAllText),
      " Remove all text");
   ("-blacktext",
      Arg.Unit (setop BlackText),
      " Blacken document text");
   ("-blacklines",
      Arg.Unit (setop BlackLines),
      " Blacken lines in document");
   ("-blackfills",
      Arg.Unit (setop BlackFills),
      " Blacken fills in document");
   ("-thinlines",
       Arg.String setthinlines,
       " Set minimum line thickness to the given width");
   ("-remove-clipping",
      Arg.Unit (setop RemoveClipping),
      " Remove clipping paths");
   ("-clean",
       Arg.Unit (setop Clean),
       " Garbage-collect a file");
   ("-set-version",
      Arg.Int (fun i -> setop (SetVersion i) ()),
      " Set PDF version number");
   ("-copy-id-from",
      Arg.String setcopyid,
      " Copy one file's ID tag to another");
   ("-print-page-labels",
      Arg.Unit (setop PrintPageLabels),
      " Print page labels");
   ("-print-page-labels-json",
      Arg.Unit setprintpagelabelsjson,
      " Print page labels in JSON format");
   ("-remove-page-labels",
      Arg.Unit (setop RemovePageLabels),
      " Remove page labels");
   ("-add-page-labels",
      Arg.Unit (setop AddPageLabels),
      " Add or replace page labels");
   ("-label-style",
      Arg.String setlabelstyle,
      " Set label style (default DecimalArabic)");
   ("-label-prefix",
      Arg.String setlabelprefix,
      " Set label prefix (default none)");
   ("-label-startval",
      Arg.Int setlabelstartval,
      " Set label start value (default 1)");
   ("-labels-progress",
      Arg.Unit setlabelsprogress,
      " Label start value progresses with multiple ranges");
   ("-remove-dict-entry",
    Arg.String setremovedictentry,
    " Remove an entry from all dictionaries");
   ("-replace-dict-entry",
    Arg.String setreplacedictentry,
    " Remove an entry from all dictionaries");
   ("-replace-dict-entry-value",
    Arg.String setreplacedictentryvalue,
    " Replacement value for -replace-dict-entry");
   ("-dict-entry-search",
    Arg.String setdictentrysearch,
    " Search string for -remove-dict-entry and -replace-dict-entry");
   ("-print-dict-entry",
    Arg.String setprintdictentry,
    " Print dictionary values of a given key");
   ("-producer",
    Arg.String setproduceraswego,
    " Change the /Producer entry in the /Info dictionary");
   ("-creator",
    Arg.String setcreatoraswego,
    " Change the /Creator entry in the /Info dictionary");
   ("-list-spot-colors",
    Arg.Unit (setop ListSpotColours),
    " List spot colors");
   ("-create-pdf",
    Arg.Unit (setop CreatePDF),
    " Create a new PDF");
   ("-create-pdf-ua-1",
    Arg.String (fun s -> args.subformat <- Some Cpdfua.PDFUA1; args.title <- Some s; setop CreatePDF ()),
    " Create a new PDF/UA-1 with the given title");
   ("-create-pdf-ua-2",
    Arg.String (fun s -> args.subformat <- Some Cpdfua.PDFUA2; args.title <- Some s; setop CreatePDF ()),
    " Create a new PDF/UA-2 with the given title");
   ("-create-pdf-pages",
    Arg.Int setcreatepdfpages,
    " Number of pages for new PDF");
   ("-create-pdf-papersize",
    Arg.String setcreatepdfpapersize,
    " Paper size for new PDF");
   ("-prepend-content",
    Arg.String setprepend,
    " Prepend content to page");
   ("-postpend-content",
    Arg.String setpostpend,
    " Postpend content to page");
   ("-gs",
    Arg.String (fun s ->  args.path_to_ghostscript <- s),
    " Path to gs executable");
   ("-gs-malformed",
    Arg.Unit setgsmalformed,
    " Also try to reconstruct malformed files with gs");
   ("-gs-quiet",
    Arg.Unit (fun () -> args.gs_quiet <- true),
    " Make gs go into quiet mode");
   ("-gs-malformed-force",
     Arg.Unit whingemalformed,
     " See manual for usage.");
   ("-im",
    Arg.String setimpath,
    " Path to magick executable");
   ("-p2p",
     Arg.String setp2ppath,
     " Path to pnmtopng executable");
   ("-extract-images",
     Arg.Unit (setop ExtractImages),
     " Extract images to file");
   ("-dedup",
     Arg.Unit (fun () -> args.dedup <- true),
     " Deduplicate extracted images fully");
   ("-dedup-perpage",
     Arg.Unit (fun () -> args.dedup_per_page <- true),
     " Deduplicate extracted images per page only");
   ("-process-images",
     Arg.Unit (setop ProcessImages),
     " Process images within PDF");
   ("-process-images-info",
     Arg.Unit setprocessimagesinfo,
     " Show info when processing images");
   ("-jbig2enc",
     Arg.String setjbig2encpath,
     " Path to jbig2enc executable");
   ("-jpeg-to-jpeg",
     Arg.Float setjpegquality,
     " Set JPEG quality for existing JPEGs");
   ("-jpeg-to-jpeg-scale",
     Arg.Float setjpegtojpegscale,
     " Set the percentage scale for -jpeg-to-jpeg");
   ("-jpeg-to-jpeg-dpi",
     Arg.Float setjpegtojpegdpi,
     " Set the DPI target for -jpeg-to-jpeg");
   ("-lossless-to-jpeg",
     Arg.Float setjpegqualitylossless,
     " Set JPEG quality for existing lossless images");
   ("-1bpp-method",
     Arg.String set1bppmethod,
     " Set 1bpp compression method for existing images");
   ("-jbig2-lossy-threshold",
     Arg.Float setjbig2_lossy_threshold,
     " Set jbig2enc lossy threshold");
   ("-pixel-threshold",
     Arg.Int setpixelthreshold,
     " Only process images with more pixels than this");
   ("-length-threshold",
     Arg.Int setlengththreshold,
     " Only process images with data longer than this");
   ("-percentage-threshold",
     Arg.Float setpercentagethreshold,
     " Only substitute lossy image when smaller than this");
   ("-dpi-threshold",
     Arg.Float setdpithreshold,
     " Only process image when always higher than this dpi");
   ("-lossless-resample",
     Arg.Float setlosslessresample,
     " Resample lossless images to given part of original");
   ("-lossless-resample-dpi",
     Arg.Float setlosslessresampledpi,
     " Resample lossless images to given DPI");
   ("-resample-interpolate",
     Arg.Unit setresampleinterpolate,
     " Interpolate when resampling");
   ("-squeeze",
     Arg.Unit setsqueeze,
     " Squeeze");
   ("-squeeze-log-to",
     Arg.String setsqueezelogto,
     " Squeeze log location");
   ("-squeeze-no-pagedata",
     Arg.Unit (fun () -> args.squeeze_pagedata <- false),
     " Don't recompress pages");
   ("-squeeze-no-recompress",
     Arg.Unit (fun () -> args.squeeze_recompress <- false),
     " Don't recompress streams");
   ("-output-json",
     Arg.Unit (setop OutputJSON),
     " Export PDF file as JSON data");
   ("-output-json-parse-content-streams",
     Arg.Unit (fun () -> args.jsonparsecontentstreams <- true),
     " Parse content streams");
   ("-output-json-no-stream-data",
     Arg.Unit (fun () -> args.jsonnostreamdata <- true),
     " Skip stream data for brevity");
   ("-output-json-decompress-streams",
     Arg.Unit (fun () -> args.jsondecompressstreams <- true),
     " Skip stream data for brevity");
   ("-output-json-clean-strings",
     Arg.Unit (fun () -> args.jsoncleanstrings <- true),
     " Convert UTF16BE strings to PDFDocEncoding when possible");
   ("-j",
     Arg.String set_json_input,
     " Load a PDF JSON file");
   ("-ocg-list",
     Arg.Unit (setop OCGList),
     " List optional content groups");
   ("-ocg-rename",
     Arg.Unit (setop OCGRename),
     " Rename optional content group");
   ("-ocg-rename-from",
     Arg.String (fun s -> args.ocgrenamefrom <- s),
     " Rename from (with -ocg-rename)");
   ("-ocg-rename-to",
     Arg.String (fun s -> args.ocgrenameto <- s),
     " Rename to (with -ocg-rename)");
   ("-ocg-order-all",
     Arg.Unit (setop OCGOrderAll),
     " Repair /Order so all OCGs listed ");
   ("-ocg-coalesce-on-name",
     Arg.Unit (setop OCGCoalesce),
     " Coalesce OCGs with like name");
   ("-stamp-as-xobject",
     Arg.String setstampasxobject,
     " Stamp a file as a form xobject in another");
   ("-print-font-table",
     Arg.String setprintfontencoding,
     " Print the /ToUnicode table for a given font, if present.");
   ("-print-font-table-page",
     Arg.Int setfontpage,
     " Set page for -print-font-table");
   ("-extract-font",
     Arg.String setextractfontfile,
     " Extract a font");
   ("-table-of-contents",
     Arg.Unit (setop TableOfContents),
     " Typeset a table of contents from bookmarks");
   ("-toc-title",
     Arg.String settableofcontentstitle,
     " Set (or clear if empty) the TOC title");
   ("-toc-no-bookmark",
     Arg.Unit settocnobookmark,
     " Don't add the table of contents to the bookmarks");
   ("-typeset",
     Arg.String settypeset,
     " Typeset a text file as a PDF");
   ("-subformat",
     Arg.String setsubformat,
     " Set subformat");
   ("-title",
     Arg.String (fun s -> args.title <- Some s),
     " Set PDF/UA title");
   ("-composition",
     Arg.Unit (setop (Composition false)),
     " Show composition of PDF");
   ("-composition-json",
     Arg.Unit (setop (Composition true)),
     " Show composition of PDF in JSON format");
   ("-text-width",
     Arg.String settextwidth,
     " Find width of a line of text");
   ("-draw", Arg.Unit setdraw, " Begin drawing");
   ("-draw-struct-tree", Arg.Unit setdrawstructtree, " Build structure trees when drawing.");
   ("-tag", Arg.String Cpdfdrawcontrol.addtag, " Begin structure item");
   ("-stag", Arg.String Cpdfdrawcontrol.addstag, " Begin struture branch");
   ("-end-tag", Arg.Unit Cpdfdrawcontrol.endtag, " End structure item");
   ("-end-stag", Arg.Unit Cpdfdrawcontrol.endstag, " End structure branch");
   ("-auto-tags", Arg.Unit (fun _ -> Cpdfdrawcontrol.autotags true), " Auto-tag paragraphs and figures");
   ("-no-auto-tags", Arg.Unit (fun _ -> Cpdfdrawcontrol.autotags false), " Don't auto-tag paragraphs and figures");
   ("-artifact", Arg.Unit (fun _ -> Cpdfdrawcontrol.artifact ()), " Begin an artifact");
   ("-end-artifact", Arg.Unit (fun _ -> Cpdfdrawcontrol.endartifact ()), "End an artifact");
   ("-no-auto-artifacts", Arg.Unit (fun _ -> Cpdfdrawcontrol.autoartifacts false), " Don't mark untagged content as artifacts");
   ("-eltinfo", Arg.String addeltinfo, " Add element information");
   ("-end-eltinfo", Arg.String (fun s -> Cpdfdrawcontrol.endeltinfo s), " Erase element information");
   ("-namespace", Arg.String (fun s -> Cpdfdrawcontrol.addnamespace (expand_namespace s)), " Set the structure tree namespace");
   ("-rolemap", Arg.String (fun s -> Cpdfdrawcontrol.setrolemap s), " Set a role map");
   ("-rect", Arg.String Cpdfdrawcontrol.addrect, " Draw rectangle");
   ("-to", Arg.String Cpdfdrawcontrol.addto, " Move to");
   ("-line", Arg.String Cpdfdrawcontrol.addline, " Add line to");
   ("-bez", Arg.String Cpdfdrawcontrol.addbezier, " Add Bezier curve to path");
   ("-bez23", Arg.String Cpdfdrawcontrol.addbezier23, " Add Bezier v-op to path");
   ("-bez13", Arg.String Cpdfdrawcontrol.addbezier13, " Add Bezier y-op to path");
   ("-circle", Arg.String Cpdfdrawcontrol.addcircle, " Add circle to path");
   ("-strokecol", Arg.String Cpdfdrawcontrol.setstroke, " Set stroke colour");
   ("-fillcol", Arg.String Cpdfdrawcontrol.setfill, " Set fill colour");
   ("-stroke", Arg.Unit Cpdfdrawcontrol.stroke, " Stroke path");
   ("-fill", Arg.Unit Cpdfdrawcontrol.fill, " Fill path");
   ("-filleo", Arg.Unit Cpdfdrawcontrol.fillevenodd, " Fill path, even odd");
   ("-strokefill", Arg.Unit Cpdfdrawcontrol.strokefill, " Stroke and fill path");
   ("-strokefilleo", Arg.Unit Cpdfdrawcontrol.strokefillevenodd, " Stroke and fill path, even odd");
   ("-clip", Arg.Unit Cpdfdrawcontrol.clip, " Clip");
   ("-clipeo", Arg.Unit Cpdfdrawcontrol.clipevenodd, " Clip, even odd");
   ("-close", Arg.Unit Cpdfdrawcontrol.closepath, " Close path");
   ("-thick", Arg.String Cpdfdrawcontrol.setthickness, " Set stroke thickness");
   ("-cap", Arg.String Cpdfdrawcontrol.setcap, " Set cap");
   ("-join", Arg.String Cpdfdrawcontrol.setjoin, " Set join");
   ("-miter", Arg.String Cpdfdrawcontrol.setmiter, " Set miter limit");
   ("-dash", Arg.String Cpdfdrawcontrol.setdash, " Set dash pattern");
   ("-push", Arg.Unit Cpdfdrawcontrol.push, " Push graphics stack");
   ("-pop", Arg.Unit Cpdfdrawcontrol.pop, " Pop graphics stack");
   ("-matrix", Arg.String Cpdfdrawcontrol.setmatrix, " Append to graphics matrix");
   ("-mtrans", Arg.String Cpdfdrawcontrol.setmtranslate, " Translate the graphics matrix");
   ("-mrot", Arg.String Cpdfdrawcontrol.setmrotate, " Rotate the graphics matrix");
   ("-mscale", Arg.String Cpdfdrawcontrol.setmscale, " Scale the graphics matrix");
   ("-mshearx", Arg.String Cpdfdrawcontrol.setmshearx, " Shear the graphics matrix in X");
   ("-msheary", Arg.String Cpdfdrawcontrol.setmsheary, " Shear the graphics matrix in Y");
   ("-xobj-bbox", Arg.String Cpdfdrawcontrol.xobjbbox, " Specify the bounding box for xobjects");
   ("-xobj", Arg.String Cpdfdrawcontrol.startxobj, " Begin saving a sequence of graphics operators");
   ("-end-xobj", Arg.Unit Cpdfdrawcontrol.endxobj, " End saving a sequence of graphics operators");
   ("-use", Arg.String Cpdfdrawcontrol.usexobj, " Use a saved sequence of graphics operators");
   ("-draw-jpeg", Arg.String Cpdfdrawcontrol.addjpeg, " Load a JPEG from file and name it");
   ("-draw-png", Arg.String Cpdfdrawcontrol.addpng, " Load a PNG from file and name it");
   ("-image", Arg.String (fun s -> Cpdfdrawcontrol.addimage s), " Draw an image which has already been loaded");
   ("-fill-opacity", Arg.Float Cpdfdrawcontrol.addopacity, " Set opacity");
   ("-stroke-opacity", Arg.Float Cpdfdrawcontrol.addsopacity, " Set stroke opacity");
   ("-bt", Arg.Unit Cpdfdrawcontrol.addbt, " Begin text");
   ("-et", Arg.Unit Cpdfdrawcontrol.addet, " End text");
   ("-text", Arg.String Cpdfdrawcontrol.addtext, " Draw text");
   ("-stext", Arg.String Cpdfdrawcontrol.addspecialtext, " Draw text with %specials");
   ("-para", Arg.String Cpdfdrawcontrol.addpara, " Add a paragraph of text");
   ("-paras", Arg.String Cpdfdrawcontrol.addparas, " Add paragraphs of text, splitting on newlines");
   ("-indent", Arg.Float (fun f -> args.indent <- Some f), " Set indent for paragraphs");
   ("-leading", Arg.String setleading, " Set leading");
   ("-charspace", Arg.String setcharspace, " Set character spacing");
   ("-wordspace", Arg.String setwordspace, " Set word space");
   ("-textscale", Arg.Float (fun f -> Cpdfdrawcontrol.addop (Cpdfdraw.TextScale f)), " Set text scale");
   ("-rendermode", Arg.Int (fun i -> Cpdfdrawcontrol.addop (Cpdfdraw.RenderMode i)), " Set text rendering mode");
   ("-rise", Arg.String setrise, " Set text rise");
   ("-nl", Arg.Unit (fun () -> Cpdfdrawcontrol.addop Cpdfdraw.Newline), " New line");
   ("-newpage", Arg.Unit Cpdfdrawcontrol.addnewpage, " Move to a fresh page");
   ("-extract-stream", Arg.String setextractstream, " Extract a stream");
   ("-extract-stream-decompress", Arg.String setextractstreamdecomp, " Extract a stream, decompressing");
   ("-obj", Arg.String setprintobj, " Print object");
   ("-obj-json", Arg.String setprintobjjson, " Print object");
   ("-replace-obj", Arg.String setreplaceobj, "Replace object");
   ("-json", Arg.Unit (fun () -> args.format_json <- true), " Format output as JSON");
   ("-verify", Arg.String (fun s -> setop (Verify s) ()), " Verify conformance to a standard");
   ("-verify-single", Arg.String (fun s -> args.verify_single <- Some s), " Verify a single test");
   ("-mark-as", Arg.String (fun s -> setop (MarkAs (Cpdfua.subformat_of_string s)) ()), " Mark as conforming to a standard");
   ("-remove-mark", Arg.String (fun s -> setop (RemoveMark (Cpdfua.subformat_of_string s)) ()), " Remove conformance mark");
   ("-print-struct-tree", Arg.Unit (fun () -> setop PrintStructTree ()), " Print structure tree");
   ("-extract-struct-tree", Arg.Unit (fun () -> setop ExtractStructTree ()), " Extract structure tree in JSON format");
   ("-replace-struct-tree", Arg.String (fun s -> setop (ReplaceStructTree s) ()), " Replace structure tree from JSON");
   ("-redact", Arg.Unit (fun () -> setop Redact ()), " Redact entire pages");
   (* These items are undocumented *)
   ("-debug", Arg.Unit setdebug, "");
   ("-debug-crypt", Arg.Unit (fun () -> args.debugcrypt <- true), "");
   ("-debug-force", Arg.Unit (fun () -> args.debugforce <- true), "");
   ("-debug-malformed", Arg.Set Pdfread.debug_always_treat_malformed, "");
   ("-debug-stderr-to-stdout", Arg.Unit setstderrtostdout, "");
   ("-debug-readable-ops", Arg.Unit setreadableops, "");
   ("-stay-on-error", Arg.Unit setstayonerror, "");
   (* These items are unfinished *)
   ("-extract-text", Arg.Unit (setop ExtractText), "");
   ("-extract-text-font-size", Arg.Float setextracttextfontsize, "");
  ]

and usage_msg =
"Syntax: cpdf [<operation>] <input files> [-o <output file>]\n\n\
Copyright Coherent Graphics Ltd.\n\n\
Version " ^ (if agpl then "AGPLv3-licensed " else "") ^ string_of_int major_version ^ "." ^ string_of_int minor_version ^ "." ^ string_of_int minor_minor_version ^ " " ^ version_date ^ "\n\n\
https://www.coherentpdf.com/\n\n\
Input names are distinguished by containing a '.' and may be\n\
followed by a page range specification, for instance \"1,2,3\"\n\
or \"1-6,9-end\" or \"even\" or \"odd\" or \"reverse\".\n\nOperations (See \
manual for full details):\n"

(* Reading and writing *)
let filesize name =
  try
   let x = open_in_bin name in
     let r = in_channel_length x in
       close_in x;
       r
  with
    _ -> 0

(* Mend PDF file with Ghostscript. We use this if a file is malformed and CPDF
 * cannot mend it. It is copied to a temporary file, fixed, then we return None or Some (pdf). *)
let mend_pdf_file_with_ghostscript filename =
  match args.path_to_ghostscript with
  | "" ->
      Pdfe.log "Please supply path to gs with -gs\n";
      exit 2
  | _ ->
      Pdfe.log "CPDF could not mend. Attempting to mend file with gs\n";
      let tmpout = Filename.temp_file "cpdf" ".pdf" in
        tempfiles := tmpout::!tempfiles;
        let gscall =
          Filename.quote_command args.path_to_ghostscript
            ((if args.gs_quiet then ["-dQUIET"] else []) @
            ["-dNOPAUSE"; "-sDEVICE=pdfwrite"; "-sOUTPUTFILE=" ^ tmpout; "-dBATCH"; filename])
        in
          match Sys.command gscall with
          | 0 -> Pdfe.log "Succeeded!\n"; tmpout
          | _ -> Pdfe.log "Could not fix malformed PDF file, even with gs\n"; exit 2

exception StdInBytes of bytes

let pdf_of_stdin ?revision user_pw owner_pw =
  let rbytes = ref (mkbytes 0) in
  try 
    let user_pw = Some user_pw
    and owner_pw = if owner_pw = "" then None else Some owner_pw in
      let o, bytes = Pdfio.input_output_of_bytes 16384 in
        try
          while true do o.Pdfio.output_char (input_char stdin) done;
          Pdf.empty ()
        with
          End_of_file ->
            let thebytes = Pdfio.extract_bytes_from_input_output o bytes in
            rbytes := thebytes;
            let i = Pdfio.input_of_bytes thebytes in
              pdfread_pdf_of_input ?revision user_pw owner_pw i
   with
     _ -> raise (StdInBytes !rbytes)

let rec get_single_pdf ?(decrypt=true) ?(fail=false) op read_lazy =
  let failout () =
    if fail then begin
      (* Reconstructed with ghostscript, but then we couldn't read it even then. Do not loop. *)
      Pdfe.log "Failed to read gs-reconstructed PDF even though gs succeeded\n";
      exit 2
    end
  in
  let warn_gs () =
    begin match args.inputs with
      (InFile inname, _, _, _, _, _)::_ ->
        begin try ignore (close_in (open_in_bin inname)) with _ ->
          Pdfe.log (Printf.sprintf "File %s does not exist\n" inname);
          exit 2
        end
    | _ -> ()
    end;
    Pdfe.log "get_single_pdf: failed to read malformed PDF file. Consider using -gs-malformed\n";
    exit 2
  in
  match args.inputs with
  | (InFile inname, x, u, o, y, revision) as input::more ->
      if args.squeeze then
        Printf.printf "Initial file size is %i bytes\n" (filesize inname);
      let pdf =
        try 
          if read_lazy then
            pdfread_pdf_of_channel_lazy ?revision (optstring u) (optstring o) (open_in_bin inname)
          else
            pdfread_pdf_of_file ?revision (optstring u) (optstring o) inname
        with
        | Cpdferror.SoftError _ as e -> raise e (* Bad owner or user password *)
        | _ ->
            if args.gs_malformed then
              begin
                failout ();
                let newname = mend_pdf_file_with_ghostscript inname in
                  args.inputs <- (InFile newname, x, u, o, y, revision)::more;
                  get_single_pdf ~fail:true op read_lazy 
              end
            else
              warn_gs ()
      in
        args.was_encrypted <- Pdfcrypt.is_encrypted pdf;
        if decrypt then decrypt_if_necessary input op pdf else pdf
  | (StdIn, x, u, o, y, revision) as input::more ->
      let pdf =
        try pdf_of_stdin ?revision u o with
          StdInBytes b ->
            if args.gs_malformed then
              begin
                failout ();
                let inname = Filename.temp_file "cpdf" ".pdf" in
                tempfiles := inname::!tempfiles;
                let fh = open_out_bin inname in
                Pdfio.bytes_to_output_channel fh b;
                close_out fh;
                let newname = mend_pdf_file_with_ghostscript inname in
                args.inputs <- (InFile newname, x, u, o, y, revision)::more;
                get_single_pdf ~fail:true op read_lazy
              end
            else
              warn_gs ()
      in
        args.was_encrypted <- Pdfcrypt.is_encrypted pdf;
        if decrypt then decrypt_if_necessary input op pdf else pdf
  | (AlreadyInMemory (pdf, s), _, _, _, _, _)::_ -> pdf
  | _ ->
      raise (Arg.Bad "cpdf: No input specified.\n")

let filenames = null_hash ()

let squeeze_logto filename x =
  let fh = open_out_gen [Open_wronly; Open_creat] 0o666 filename in
    seek_out fh (out_channel_length fh);
    output_string fh x;
    close_out fh

(* This now memoizes on the name of the file to make sure we only load each
file once *)
let rec get_pdf_from_input_kind ?(read_lazy=false) ?(decrypt=true) ?(fail=false) ((_, x, u, o, y, revision) as input) op ik =
  let failout () =
    if fail then begin
      (* Reconstructed with ghostscript, but then we couldn't read it even then. Do not loop. *)
      Pdfe.log "Failed to read gs-reconstructed PDF even though gs succeeded\n";
      exit 2
    end
  in
  let warn_gs () =
    begin match input with
      (InFile inname, _, _, _, _, _) ->
        begin try ignore (close_in (open_in_bin inname)) with _ ->
          Pdfe.log (Printf.sprintf "File %s does not exist\n" inname);
          exit 2
        end
    | _ -> ()
    end;
    Pdfe.log "get_pdf_from_input_kind: failed to read malformed PDF file. Consider using -gs-malformed\n";
    exit 2
  in
  match ik with
  | AlreadyInMemory (pdf, _) -> pdf
  | InFile s ->
      if args.squeeze then
        begin
          let size = filesize s in
            initial_file_size := size;
            let str = Printf.sprintf "Initial file size is %i bytes\n" size in
            begin match !logto with
            | None -> print_string str
            | Some filename -> squeeze_logto filename str
            end
        end;
      begin try Hashtbl.find filenames s with
        Not_found ->
          let pdf =
            try
              if read_lazy then
                pdfread_pdf_of_channel_lazy ?revision (optstring u) (optstring o) (open_in_bin s)
              else
                pdfread_pdf_of_file ?revision (optstring u) (optstring o) s
            with
            | Cpdferror.SoftError _ as e -> raise e (* Bad owner or user password *)
            | e ->
                Printf.printf "%s\n" (Printexc.to_string e);
                if args.gs_malformed then
                  begin
                    failout ();
                    let newname = mend_pdf_file_with_ghostscript s in
                      get_pdf_from_input_kind ~fail:true (InFile newname, x, u, o, y, revision) op (InFile newname);
                  end
                else
                  warn_gs ()
          in
            args.was_encrypted <- Pdfcrypt.is_encrypted pdf;
            let pdf = if decrypt then decrypt_if_necessary input op pdf else pdf in
              Hashtbl.add filenames s pdf; pdf
      end
  | StdIn ->
      let pdf =
        try pdf_of_stdin ?revision u o with
          StdInBytes b ->
            if args.gs_malformed then
              begin
                failout ();
                let inname = Filename.temp_file "cpdf" ".pdf" in
                tempfiles := inname::!tempfiles;
                let fh = open_out_bin inname in
                Pdfio.bytes_to_output_channel fh b;
                close_out fh;
                let newname = mend_pdf_file_with_ghostscript inname in
                get_pdf_from_input_kind ~fail:true (InFile newname, x, u, o, y, revision) op (InFile newname);
              end
            else
              warn_gs ()
      in
        args.was_encrypted <- Pdfcrypt.is_encrypted pdf;
        if decrypt then decrypt_if_necessary input op pdf else pdf

let rec unescape_octals prev = function
  | [] -> rev prev
  | '\\'::('0'..'9' as a)::('0'..'9' as b)::('0'..'9' as c)::t ->
       let chr = char_of_int (int_of_string ("0o" ^ implode [a;b;c])) in
         unescape_octals (chr::prev) t
  | '\\'::'\\'::t -> unescape_octals ('\\'::prev) t
  | h::t -> unescape_octals (h::prev) t

let unescape_octals s =
  implode (unescape_octals [] (explode s))

let process s = 
  if args.encoding <> Cpdfmetadata.Raw
    then Pdftext.pdfdocstring_of_utf8 s
    else unescape_octals s

let set_producer s pdf =
  ignore (Cpdfmetadata.set_pdf_info ("/Producer", Pdf.String (process s), 0) pdf)

let set_creator s pdf =
  ignore (Cpdfmetadata.set_pdf_info ("/Creator", Pdf.String (process s), 0) pdf)

let really_write_pdf ?(encryption = None) ?(is_decompress=false) mk_id pdf outname =
  if args.producer <> None then set_producer (unopt args.producer) pdf;
  if args.creator <> None then set_creator (unopt args.creator) pdf;
  if args.debugcrypt then Printf.printf "really_write_pdf\n";
  let will_linearize =
    args.linearize || args.keeplinearize && pdf.Pdf.was_linearized
  in
  let outname' =
    if will_linearize then Filename.temp_file "cpdflin" ".pdf" else outname
  in
    if args.debugcrypt then
      Printf.printf "args.recrypt = %b, args.was_encrypted = %b\n"
        args.recrypt args.was_encrypted;
    begin
      if args.recrypt && args.was_encrypted then
        begin
          if args.debugcrypt then
            Printf.printf "Recrypting in really_write_pdf\n";
          match args.inputs with
            [] -> raise (Pdf.PDFError "no input in recryption")
          | (_, _, user_pw, owner_pw, _, _)::_ ->
              let best_password =
                if owner_pw <> "" then owner_pw else user_pw
              in
                Pdfwrite.pdf_to_file_options
                  ~preserve_objstm:args.preserve_objstm
                  ~generate_objstm:args.create_objstm
                  ~compress_objstm:(not is_decompress)
                  ~recrypt:(Some best_password)
                  None mk_id pdf outname'
        end
      else
        begin
          if args.debugforce || not args.was_encrypted || args.was_decrypted_with_owner then
            begin
              if args.debugcrypt then
                Printf.printf "Pdf to file in really_write_pdf\n";
              Pdfwrite.pdf_to_file_options
                ~preserve_objstm:args.preserve_objstm
                ~generate_objstm:args.create_objstm
                ~compress_objstm:(not is_decompress)
                encryption mk_id pdf outname'
            end
          else
            soft_error
              "You must supply -recrypt here, or add -decrypt-force, or provide the owner password."
        end
    end;
    begin
      if will_linearize then
        let cpdflin = find_cpdflin args.cpdflin in
          match args.inputs with
            [] -> raise (Pdf.PDFError "no input in recryption")
          | (_, _, user_pw, owner_pw, _, _)::_ ->
              let best_password =
                if owner_pw <> "" then owner_pw else user_pw
              in
                let code =
                  call_cpdflin cpdflin outname' outname best_password
                in
                  if code > 0 then
                    begin
                      begin try Sys.remove outname with _ -> () end;
                      Sys.rename outname' outname;
                      soft_error
                        "Linearizer failed with above error. \
                        File written without linearization."
                    end
                  else
                    begin try Sys.remove outname' with _ -> () end;
    end;
    if args.squeeze then
      let s = filesize outname in
        begin
          let str =
            Printf.sprintf
              "Final file size is %i bytes, %.2f%% of original.\n"
              s
              ((float s /. float !initial_file_size) *. 100.)
          in
          match !logto with
          | None -> print_string str
          | Some filename -> squeeze_logto filename str
        end

let write_pdf ?(encryption = None) ?(is_decompress=false) mk_id pdf =
  if args.debugcrypt then Printf.printf "write_pdf\n";
  if args.create_objstm && not (args.keepversion || pdf.Pdf.major > 1)
    then pdf.Pdf.minor <- max pdf.Pdf.minor 5;
    match args.out with
    | NoOutputSpecified ->
        output_pdfs =| pdf
    | File outname ->
        begin match encryption with
          None ->
            if not is_decompress then
              begin
                ignore (Cpdfsqueeze.recompress_pdf pdf);
                if args.squeeze then Cpdfsqueeze.squeeze ~pagedata:args.squeeze_pagedata ?logto:!logto pdf;
              end;
            Pdf.remove_unreferenced pdf;
            really_write_pdf ~is_decompress mk_id pdf outname
        | Some _ ->
            really_write_pdf ~encryption ~is_decompress mk_id pdf outname
        end
    | Stdout ->
        let temp = Filename.temp_file "cpdfstdout" ".pdf" in
          begin match encryption with
            None -> 
              if not is_decompress then
                begin
                  ignore (Cpdfsqueeze.recompress_pdf pdf);
                  if args.squeeze then Cpdfsqueeze.squeeze ~pagedata:args.squeeze_pagedata ?logto:!logto pdf;
                  Pdf.remove_unreferenced pdf
                end;
                really_write_pdf ~encryption ~is_decompress mk_id pdf temp;
          | Some _ ->
              really_write_pdf ~encryption ~is_decompress mk_id pdf temp
          end;
          let temp_file = open_in_bin temp in
            try
              while true do output_char stdout (input_char temp_file) done;
              assert false
            with
              End_of_file ->
                begin try close_in temp_file; Sys.remove temp with
                  e -> Pdfe.log (Printf.sprintf "Failed to remove temp file %s (%s)\n" temp (Printexc.to_string e))
                end;
                flush stdout (*r For Windows *)

(* Find the stem of a filename *)
let stem s =
  implode
    (rev (tail_no_fail
      (dropwhile
        (neq '.') (rev (explode (Filename.basename s))))))

let fast_write_split_pdfs
  ?(names=[]) enc splitlevel original_filename sq spec main_pdf pagenums pdf_pages
=
  let marks = Pdfmarks.read_bookmarks main_pdf in
    iter2
      (fun number pagenums ->
         let pdf = Pdfpage.pdf_of_pages ~retain_numbering:args.retain_numbering ~process_struct_tree:args.process_struct_trees main_pdf pagenums in
           let startpage, endpage = extremes pagenums in
             let name =
               if names <> [] then List.nth names (number - 1) else
                 Cpdfbookmarks.name_of_spec
                   args.encoding marks main_pdf splitlevel spec number
                   (stem original_filename) startpage endpage
             in
               Pdf.remove_unreferenced pdf;
               if sq then Cpdfsqueeze.squeeze ~pagedata:args.squeeze_pagedata ?logto:!logto pdf;
               really_write_pdf ~encryption:enc (not (enc = None)) pdf name)
      (indx pagenums)
      pagenums

(* Return list, in order, a *set* of page numbers of bookmarks at a given level *)
let bookmark_pages level pdf =
  let refnums = Pdf.page_reference_numbers pdf in
  let fastrefnums = hashtable_of_dictionary (combine refnums (indx refnums)) in
    setify_preserving_order
      (option_map
        (function
           l when l.Pdfmarks.level = level ->
             Some (Pdfpage.pagenumber_of_target ~fastrefnums pdf l.Pdfmarks.target)
         | _ -> None)
        (Pdfmarks.read_bookmarks pdf))

let split_at_bookmarks
  enc original_filename ~squeeze level spec pdf
=
  let pdf_pages = Pdfpage.pages_of_pagetree pdf in
    let points = bookmark_pages level pdf in
      let points =
        lose (fun x -> x <= 0 || x > Pdfpage.endpage pdf) (map pred points)
      in
        let pts = splitat points (indx pdf_pages) in
          fast_write_split_pdfs
            enc level original_filename squeeze spec pdf pts pdf_pages

let split_pdf
  enc original_filename
  chunksize linearize ~cpdflin ~preserve_objstm ~create_objstm ~squeeze
  spec pdf
=
  let pdf_pages = Pdfpage.pages_of_pagetree pdf in
    fast_write_split_pdfs
      enc 0 original_filename squeeze spec pdf
      (splitinto chunksize (indx pdf_pages)) pdf_pages

(* Given a PDF, write the split as if we had selected pages, and return its filesize. Delete it. *)
let split_max_fits pdf s p q =
  if q < p then error "split_max_fits" else
  let filename = Filename.temp_file "cpdf" "sm" in
  let range = ilist p q in
  let newpdf = Pdfpage.pdf_of_pages ~process_struct_tree:args.process_struct_trees ~retain_numbering:args.retain_numbering pdf range in
    let r = args.out in
      args.out <- File filename;
      write_pdf false newpdf;
      args.out <- r;
      let fh = open_in_bin filename in
      let size = in_channel_length fh in
        begin try close_in fh; Sys.remove filename with _ -> () end;
        size <= s

(* Binary search on q from current value down to p to find max which fits. Returns q. Upon failure, returns -1 *)
let rec split_max_search pdf s b p q =
  if p = q then
    if split_max_fits pdf s b q then q else -1
  else
    let half = (q + p) / 2 in
      if split_max_fits pdf s b (half + 1)
        then split_max_search pdf s b (half + 1) q
        else split_max_search pdf s b p half

let split_max enc original_filename ~squeeze output_spec s pdf =
  let outs = ref [] in
  let p = ref 1 in
  let endpage = Pdfpage.endpage pdf in
  let q = ref endpage in
    while !p < !q || !p = endpage do
      let newq = split_max_search pdf s !p !p !q in
        if newq = -1 then (Printf.eprintf "Failed to make small enough split at page %i. No files written.\n" !p; exit 2) else
          begin
            (*Printf.printf "Pages %i-%i will fit...\n%!" !p newq;*)
            outs := ilist !p newq::!outs;
            p := newq + 1;
            q := endpage
          end
    done;
    fast_write_split_pdfs enc 0 original_filename squeeze output_spec pdf (rev !outs) (Pdfpage.pages_of_pagetree pdf)

let getencryption pdf =
  match Pdfread.what_encryption pdf with
  | None | Some Pdfwrite.AlreadyEncrypted -> "Not encrypted"
  | Some Pdfwrite.PDF40bit -> "40bit"
  | Some Pdfwrite.PDF128bit -> "128bit"
  | Some (Pdfwrite.AES128bit true) -> "128bit AES, Metadata encrypted"
  | Some (Pdfwrite.AES128bit false) -> "128bit AES, Metadata not encrypted"
  | Some (Pdfwrite.AES256bit true) -> "256bit AES, Metadata encrypted"
  | Some (Pdfwrite.AES256bit false) -> "256bit AES, Metadata not encrypted"
  | Some (Pdfwrite.AES256bitISO true) -> "256bit AES ISO, Metadata encrypted"
  | Some (Pdfwrite.AES256bitISO false) -> "256bit AES ISO, Metadata not encrypted"

let write_json output pdf =
  match output with
  | NoOutputSpecified ->
      error "-output-json: no output name specified"
  | Stdout ->
      Cpdfjson.to_output
        (Pdfio.output_of_channel stdout)
        ~utf8:(args.encoding = Cpdfmetadata.UTF8)
        ~parse_content:args.jsonparsecontentstreams
        ~no_stream_data:args.jsonnostreamdata
        ~decompress_streams:args.jsondecompressstreams
        ~clean_strings:args.jsoncleanstrings
        pdf
  | File filename ->
      let f = open_out filename in
        Cpdfjson.to_output
          (Pdfio.output_of_channel f)
          ~utf8:(args.encoding = Cpdfmetadata.UTF8)
          ~parse_content:args.jsonparsecontentstreams
          ~no_stream_data:args.jsonnostreamdata
          ~decompress_streams:args.jsondecompressstreams
          ~clean_strings:args.jsoncleanstrings
          pdf;
        close_out f

let json_to_output json = function
  | NoOutputSpecified ->
      error "no output name specified"
  | Stdout ->
      output_string stdout (Cpdfyojson.Safe.pretty_to_string json);
  | File filename ->
      let f = open_out filename in
        output_string f (Cpdfyojson.Safe.pretty_to_string json);
        close_out f

let collate (names, pdfs, ranges) =
  let ois = map ref (combine3 names pdfs ranges) in
  let nis = ref [] in
    while flatten (map (fun {contents = (_, _, r)} -> r) ois) <> [] do
      iter
        (fun ({contents = (name, pdf, range)} as r) ->
           match range with
           | [] -> ()
           | h::t ->
               nis := (name, pdf, [h])::!nis;
               r := (name, pdf, t))
        ois
    done;
    split3 (rev !nis)

let warn_prerotate range pdf =
  if not args.prerotate && not (Cpdfpage.alluprightonly range pdf) then
    Pdfe.log "Some pages in the range have non-zero rotation. \
              Consider adding -prerotate or pre-processing with -upright. \
              To silence this warning use -no-warn-rotate\n"

let prerotate range pdf =
  Cpdfpage.upright ~fast:args.fast range pdf

let check_bookmarks_mistake () =
  if args.merge_add_bookmarks_use_titles && not args.merge_add_bookmarks then
    Pdfe.log "Warning: -merge-add-bookmarks-use-titles is for use with -merge-add-bookmarks\n"

let check_clashing_output_name () =
  match args.out with
  | File s ->
      if (List.exists (function (InFile s', _, _, _, _, _) when s = s' -> true | _ -> false) args.inputs) then
        Pdfe.log "Warning: output file name clashes with input file name. Malformed file may result.\n"
  | _ -> ()

let build_enc () =
  match args.crypt_method with
  | "" -> None
  | _ ->
    Some
      {Pdfwrite.encryption_method =
         (match args.crypt_method with
         | "40bit" -> Pdfwrite.PDF40bit
         | "128bit" -> Pdfwrite.PDF128bit
         | "AES" -> Pdfwrite.AES128bit args.encrypt_metadata
         | "AES256" -> Pdfwrite.AES256bit args.encrypt_metadata
         | "AES256ISO" -> Pdfwrite.AES256bitISO args.encrypt_metadata
         | _ -> assert false (* Pre-checked *));
       Pdfwrite.owner_password = args.owner;
       Pdfwrite.user_password = args.user;
       Pdfwrite.permissions = banlist_of_args ()}

let extract_stream pdf decomp objnum =
  let objnum = int_of_string objnum in (* maybe objspec in the future... *)
  let obj = Pdf.lookup_obj pdf objnum in
  Pdf.getstream obj;
  if decomp then Pdfcodec.decode_pdfstream_until_unknown pdf obj;
  let data =
    match obj with
    | Pdf.Stream {contents = (_, Pdf.Got x)} -> x
    | _ -> mkbytes 0
  in
    match args.out with
    | NoOutputSpecified ->
        ()
    | File outname ->
        let fh = open_out_bin outname in
          output_string fh (Pdfio.string_of_bytes data);
          close_out fh
    | Stdout ->
        output_string stdout (Pdfio.string_of_bytes data)

(* Empty string is trailerdict. Begins with / and it's a chain separated by
   commas. Begins with P and it's a page number then a (possibly empty) chain.
   Otherwise it's an object number (0 = trailerdict). *)
let split_chain str = map (fun x -> "/" ^ x) (tl (String.split_on_char '/' str))

let print_obj json pdf objspec =
  let write obj =
    if json then
      print_string (Cpdfyojson.Safe.pretty_to_string (Cpdfjson.json_of_object ~utf8:true pdf (fun _ -> ()) ~no_stream_data:false ~parse_content:false obj))
    else
      Printf.printf "%S\n" (Pdfwrite.string_of_pdf obj)
  in
  let simple_obj obj =
    write (if obj = 0 then pdf.Pdf.trailerdict else Pdf.lookup_obj pdf obj)
  in
  let chain_obj objnum chain =
    let obj = if objnum = 0 then pdf.Pdf.trailerdict else Pdf.lookup_obj pdf objnum in
    match Pdf.lookup_chain pdf obj chain with
    | Some x -> write x
    | None -> ()
  in
    match explode objspec with
    | 'P'::more ->
        let number, chain =
          let digits, rest = cleavewhile isdigit more in
            List.nth (Pdf.page_reference_numbers pdf) (int_of_string (implode digits) - 1),
            begin match split_chain (implode rest) with [""] -> [] | x -> x end
        in
          chain_obj number chain
    | '/'::more -> chain_obj 0 (split_chain (implode ('/'::more)))
    | [] -> simple_obj 0
    | _ -> simple_obj (int_of_string objspec)

let print_version () =
  flprint
    ("cpdf " ^ (if agpl then "AGPL " else "") ^ "Version " ^ string_of_int major_version ^ "." ^ string_of_int minor_version ^ "." ^ string_of_int minor_minor_version ^ " " ^ version_date ^ "\n")

let replace_obj pdf objspec obj =
  let split_chain str = map (fun x -> "/" ^ x) (tl (String.split_on_char '/' str)) in
  let chain = split_chain objspec in
    Pdf.replace_chain pdf chain obj
    
(* Main function *)
let go () =
  check_bookmarks_mistake ();
  check_clashing_output_name ();
  match args.op with
  | Some Version -> print_version ()
  | None | Some Merge ->
      begin match args.out, args.inputs with
      | _, (_::_ as inputs) ->
          let op = match inputs with [_] -> None | _ -> Some Merge in
            let names, ranges, rotations, _, _, _ = split6 inputs in
              let pdfs = map2 (fun i -> get_pdf_from_input_kind i op) inputs names in
                (* If at least one file had object streams and args.preserve_objstm is true, set -objstm-create *)
                if args.preserve_objstm then
                  iter
                    (fun pdf ->
                       if Hashtbl.length pdf.Pdf.objects.Pdf.object_stream_ids > 0
                         then args.create_objstm <- true)
                    pdfs;
                begin match pdfs with
                | [pdf] ->
                    if hd ranges <> "all" then
                      let range = parse_pagespec pdf (hd ranges) in
                        let newpdf = Pdfpage.pdf_of_pages ~process_struct_tree:args.process_struct_trees ~retain_numbering:args.retain_numbering pdf range in
                          write_pdf false newpdf
                    else
                        write_pdf false pdf
                | _ ->
                    (* We check permissions. A merge is allowed if each file
                     included was (a) not encrypted (detected by the absence of
                     saved encryption information in the PDF, or (b) decrypted using
                     the owner password (stored in the input) *)
                    if
                      (not args.debugforce) &&
                      (not
                        (fold_left ( && ) true
                          (map2
                            (fun (_, _, _, _, was_dec_with_owner, _) pdf ->
                               !was_dec_with_owner || pdf.Pdf.saved_encryption = None)
                            inputs
                            pdfs)))
                    then
                      soft_error "Merge requires the owner password for all encrypted files, or -decrypt-force."
                    else
                      let pdfs =
                        if args.merge_add_bookmarks then
                          map2
                            (fun filename pdf -> Cpdfbookmarks.add_bookmark_title filename args.merge_add_bookmarks_use_titles pdf)
                            (map (function InFile s -> s | StdIn -> "" | AlreadyInMemory (_, s) -> s) names)
                            pdfs
                        else
                          pdfs
                      in
                      (* If args.keep_this_id is set, change the ID to the one from the kept one *)
                      let rangenums = map2 parse_pagespec pdfs ranges in
                        (* At this point, we have the information for collation. *)
                        let names = map string_of_input_kind names in
                        let names, pdfs, rangenums =
                          (if args.collate then collate else Fun.id) (names, pdfs, rangenums)
                        in
                        let outpdf =
                          Pdfmerge.merge_pdfs
                            args.retain_numbering args.remove_duplicate_fonts ~process_struct_trees:args.process_struct_trees
                            ~add_toplevel_document:(args.subformat = Some Cpdfua.PDFUA2) names pdfs rangenums
                        in
                          if args.remove_duplicate_streams then Pdfmerge.remove_duplicate_fonts outpdf; (* JBIG2 Globals *)
                          write_pdf false outpdf
                  end
      | _ ->
          match args.op with
          | Some Merge ->
              error "Merge: Must specify one output and at least one input"
          | None ->
              error "Must specify one output and at least one input"
          | _ -> assert false
      end
  | Some (CopyFont fromfile) ->
      begin match args.inputs, args.out with
      | (_, pagespec, u, o, _, _)::_, _ ->
          let pdf = get_single_pdf (Some (CopyFont fromfile)) false
          and frompdf = pdfread_pdf_of_file (optstring u) (optstring o) fromfile in
            let range = parse_pagespec_allow_empty pdf pagespec in
              let copyfontname =
                match args.copyfontname with
                | Some x -> x
                | None -> failwith "copy_font: no font name given"
              in
                let outpdf = Cpdffont.copy_font frompdf copyfontname args.copyfontpage range pdf in
                  write_pdf true outpdf
      | _ -> error "copyfont: bad command line"
      end
  | Some RemoveFonts ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf (Some RemoveFonts) false in
            write_pdf true (Cpdffont.remove_fonts pdf)
      | _ -> error "remove fonts: bad command line"
      end
  | Some (ExtractFontFile spec) ->
      begin match args.inputs, args.out with
      | (_, pagespec, u, o, _, _)::_, File filename ->
          let pdf = get_single_pdf (Some (ExtractFontFile spec)) false in
            begin match String.split_on_char ',' spec with
            | [pnum; name] ->
                begin try Cpdffont.extract_fontfile (int_of_string pnum) name filename pdf with
                  Failure _ (*"int_of_string"*) -> error "extract font: bad page number"
                end
            | _ -> error "extract font: bad specification"
            end
      | _ -> error "extract fontfile: bad command line"
      end 
  | Some CountPages ->
      begin match args.inputs with
       [(ik, _, _, _, _, _) as input] ->
         let pdf = get_pdf_from_input_kind ~read_lazy:true ~decrypt:false input (Some CountPages) ik in
           output_page_count pdf
      | _ -> raise (Arg.Bad "CountPages: must have a single input file only")
      end
  | Some Clean ->
      let pdf' = get_single_pdf (Some Clean) false in
        write_pdf false pdf'
  | Some Info ->
      let pdf, inname, input =
        match args.inputs with
        | (InFile inname, _, u, o, _, _) as input::_ ->
             pdfread_pdf_of_channel_lazy (optstring u) (optstring o) (open_in_bin inname), inname, input
        | (StdIn, _, u, o, _, _) as input::_ -> pdf_of_stdin u o, "", input
        | (AlreadyInMemory (pdf, _), _, _, _, _, _) as input::_ -> pdf, "", input
        | _ -> raise (Arg.Bad "cpdf: No input specified.\n")
      in
        let json = ref [] in
        if args.format_json
          then json =| ("Encryption", `String (getencryption pdf))
          else Printf.printf "Encryption: %s\n" (getencryption pdf);
        if args.format_json
          then json =| ("Permissions", `List (map (fun p -> `String (string_of_permission p)) (Pdfread.permissions pdf)))
          else Printf.printf "Permissions: %s\n" (getpermissions pdf);
        if inname <> "" then
          let lin = Pdfread.is_linearized (Pdfio.input_of_channel (open_in_bin inname)) in
            if args.format_json then
              json =| ("Linearized", `Bool lin) else Printf.printf "Linearized: %b\n" lin;
        let objstm = length (list_of_hashtbl pdf.Pdf.objects.Pdf.object_stream_ids) > 0 in
        if args.format_json
          then json =| ("Object streams", `Bool objstm)
          else Printf.printf "Object streams: %b\n" objstm;
        let ida, idb =
          match Pdf.lookup_direct pdf "/ID" pdf.Pdf.trailerdict with
          | Some (Pdf.Array [Pdf.String s; Pdf.String s']) ->
              (Pdfwrite.make_hex_pdf_string s, Pdfwrite.make_hex_pdf_string s')
          | _ -> "", ""
        in
        let fixid s = implode (rev (tl (rev (tl (explode s))))) in
        if args.format_json
          then json =| ("ID", if ida ^ idb = "" then `Null else `List [`String (fixid ida); `String (fixid idb)])
          else (if ida ^ idb = "" then Printf.printf "ID: None\n" else Printf.printf "ID: %s %s\n" ida idb);
        let pdf = decrypt_if_necessary input (Some Info) pdf in
          if args.format_json then
            begin
              Cpdfmetadata.output_info ~json Cpdfmetadata.UTF8 pdf;
              Cpdfmetadata.output_xmp_info ~json Cpdfmetadata.UTF8 pdf;
              flprint (Cpdfyojson.Safe.pretty_to_string (`Assoc (rev !json)))
            end
          else
            begin
              Cpdfmetadata.output_info args.encoding pdf;
              Cpdfmetadata.output_xmp_info args.encoding pdf
            end
  | Some PageInfo ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf args.op true in
            let range = parse_pagespec_allow_empty pdf pagespec in
              Cpdfpage.output_page_info ~json:args.format_json pdf range
      | _ -> error "list-bookmarks: bad command line"
      end
  | Some Metadata ->
      Cpdfmetadata.print_metadata (get_single_pdf (Some Metadata) true)
  | Some Fonts ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf (Some Fonts) true in
          let range = parse_pagespec_allow_empty pdf pagespec in
            Cpdffont.print_fonts ~json:args.format_json pdf range
      | _ -> error "-list-fonts: bad command line"
      end
  | Some ListBookmarks ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
        let pdf = get_single_pdf args.op true in
          let range = parse_pagespec_allow_empty pdf pagespec in
            Cpdfbookmarks.list_bookmarks ~json:args.format_json args.encoding range pdf (Pdfio.output_of_channel stdout);
            flush stdout
      | _ -> error "list-bookmarks: bad command line"
      end
  | Some Crop ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf (Some Crop) false in
            let xywhlist = Cpdfcoord.parse_rectangles pdf args.rectangle in
              let range = parse_pagespec_allow_empty pdf pagespec in
                let pdf = Cpdfpage.crop_pdf xywhlist pdf range in
                  write_pdf false pdf
      | _ -> error "crop: bad command line"
      end
  | Some Art ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf (Some Art) false in
            let xywhlist = Cpdfcoord.parse_rectangles pdf args.rectangle in
              let range = parse_pagespec_allow_empty pdf pagespec in
                let pdf = Cpdfpage.crop_pdf ~box:"/ArtBox" xywhlist pdf range in
                  write_pdf false pdf
      | _ -> error "art: bad command line"
      end
  | Some Bleed ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf (Some Bleed) false in
            let xywhlist = Cpdfcoord.parse_rectangles pdf args.rectangle in
              let range = parse_pagespec_allow_empty pdf pagespec in
                let pdf = Cpdfpage.crop_pdf ~box:"/BleedBox" xywhlist pdf range in
                  write_pdf false pdf
      | _ -> error "bleed: bad command line"
      end
  | Some Trim ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf (Some Trim) false in
            let xywhlist = Cpdfcoord.parse_rectangles pdf args.rectangle in
              let range = parse_pagespec_allow_empty pdf pagespec in
                let pdf = Cpdfpage.crop_pdf ~box:"/TrimBox" xywhlist pdf range in
                  write_pdf false pdf
      | _ -> error "trim: bad command line"
      end
  | Some MediaBox ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf (Some MediaBox) false in
            let xywhlist = Cpdfcoord.parse_rectangles pdf args.rectangle in
              let range = parse_pagespec_allow_empty pdf pagespec in
                let pdf = Cpdfpage.set_mediabox xywhlist pdf range in
                  write_pdf false pdf
      | _ -> error "set media box: bad command line"
      end
  | Some (HardBox box) ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf (Some (HardBox box)) false in
          let range = parse_pagespec_allow_empty pdf pagespec in
            let pdf = Cpdfpage.hard_box pdf range box args.mediabox_if_missing args.fast in
              write_pdf false pdf
      | _ -> error "hard box: bad command line"
      end
  | Some CopyBox ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf (Some CopyBox) false in
            let range = parse_pagespec_allow_empty pdf pagespec in
              let f, t =
                begin match args.frombox, args.tobox with
                | Some f, Some t -> f, t
                | _ -> error "Copy box: no tobox or no frombox specified"
                end
              in
                let pdf = Cpdfpage.copy_box f t args.mediabox_if_missing pdf range in
                  write_pdf false pdf
      | _ -> error "Copy Box: bad command line"
      end
  | Some Decompress ->
      let pdf = get_single_pdf (Some Decompress) false in
        Pdf.iter_stream
          (function stream ->
             try Pdfcodec.decode_pdfstream_until_unknown pdf stream with
               e -> Pdfe.log (Printf.sprintf "Decode failure: %s. Carrying on...\n" (Printexc.to_string e)); ())
          pdf;
        write_pdf ~is_decompress:true false pdf
  | Some Compress ->
      let pdf = get_single_pdf (Some Compress) false in
        if args.remove_duplicate_streams then
          Pdfmerge.remove_duplicate_fonts pdf;
        write_pdf false (Cpdfsqueeze.recompress_pdf pdf)
  | Some RemoveCrop ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf (Some RemoveCrop) false in
            let range = parse_pagespec_allow_empty pdf pagespec in
              let pdf = Cpdfpage.remove_cropping_pdf pdf range in
                write_pdf false pdf
      | _ -> error "remove-crop: bad command line"
      end
  | Some RemoveArt ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf (Some RemoveArt) false in
            let range = parse_pagespec_allow_empty pdf pagespec in
              let pdf = Cpdfpage.remove_art_pdf pdf range in
                write_pdf false pdf
      | _ -> error "remove-crop: bad command line"
      end
  | Some RemoveTrim ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf (Some RemoveTrim) false in
            let range = parse_pagespec_allow_empty pdf pagespec in
              let pdf = Cpdfpage.remove_trim_pdf pdf range in
                write_pdf false pdf
      | _ -> error "remove-crop: bad command line"
      end
  | Some RemoveBleed ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf (Some RemoveBleed) false in
            let range = parse_pagespec_allow_empty pdf pagespec in
              let pdf = Cpdfpage.remove_bleed_pdf pdf range in
                write_pdf false pdf
      | _ -> error "remove-crop: bad command line"
      end
  | Some (Rotate _)  | Some (Rotateby _) ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf args.op false in
            let range = parse_pagespec_allow_empty pdf pagespec in
              let rotate =
                match args.op with
                | Some (Rotate i) -> Cpdfpage.rotate_pdf i
                | Some (Rotateby i) -> Cpdfpage.rotate_pdf_by i
                | _ -> assert false
              in
                let pdf = rotate pdf range in
                  write_pdf false pdf
      | _ -> error "rotate: bad command line"
      end
  | Some (RotateContents a) ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf args.op false in
            let range = parse_pagespec_allow_empty pdf pagespec in
              let pdf = Cpdfpage.rotate_contents ~fast:args.fast a pdf range in
                write_pdf false pdf
      | _ -> error "rotate-contents: bad command line"
      end
  | Some Upright ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf args.op false in
            let range = parse_pagespec_allow_empty pdf pagespec in
              let pdf = Cpdfpage.upright ~fast:args.fast range pdf in
                write_pdf false pdf
      | _ -> error "rotate-contents: bad command line"
      end
  | Some ((VFlip | HFlip) as flip) ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf args.op false in
            let range = parse_pagespec_allow_empty pdf pagespec in
              let pdf = 
                if flip = VFlip
                  then Cpdfpage.vflip_pdf ~fast:args.fast pdf range
                  else Cpdfpage.hflip_pdf ~fast:args.fast pdf range
              in
                write_pdf false pdf
      | _ -> error "flip: bad command line"
      end
  | Some ((SetAuthor _ | SetTitle _ | SetSubject _ | SetKeywords _
          | SetCreate _ | SetModify _ | SetCreator _ | SetProducer _
          | SetTrapped | SetUntrapped) as op) ->
      let key, value, version  =
        let f s = if args.encoding <> Cpdfmetadata.Raw then Pdftext.pdfdocstring_of_utf8 s else unescape_octals s in
          match op with
          | SetAuthor s -> "/Author", Pdf.String (f s), 0
          | SetTitle s -> "/Title", Pdf.String (f s), 1
          | SetSubject s -> "/Subject", Pdf.String (f s), 1
          | SetKeywords s -> "/Keywords", Pdf.String (f s), 1
          | SetCreate s -> "/CreationDate", Pdf.String (Cpdfmetadata.expand_date s), 0
          | SetModify s -> "/ModDate", Pdf.String (Cpdfmetadata.expand_date s), 0
          | SetCreator s -> "/Creator", Pdf.String (f s), 0
          | SetProducer s -> "/Producer", Pdf.String (f s), 0
          | SetTrapped -> "/Trapped", Pdf.Boolean true, 3
          | SetUntrapped -> "/Trapped", Pdf.Boolean false, 3
          | _ -> assert false
      in
        let pdf = get_single_pdf args.op false in
          let version = if args.keepversion || pdf.Pdf.major > 1 then pdf.Pdf.minor else version in
            write_pdf false
              (Cpdfmetadata.set_pdf_info 
                 ~xmp_also:args.alsosetxml
                 ~xmp_just_set:args.justsetxml
                 (key, value, version) pdf)
  | Some (SetMetadataDate date) ->
      write_pdf false (Cpdfmetadata.set_metadata_date (get_single_pdf args.op false) date)
  | Some ((HideToolbar _ | HideMenubar _ | HideWindowUI _
          | FitWindow _ | CenterWindow _ | DisplayDocTitle _) as op) ->
      begin match args.out with
      | _ ->
        let key, value, version =
          match op with
          | HideToolbar s -> "/HideToolbar", Pdf.Boolean s, 0
          | HideMenubar s -> "/HideMenubar", Pdf.Boolean s, 0
          | HideWindowUI s -> "/HideWindowUI", Pdf.Boolean s, 0
          | FitWindow s -> "/FitWindow", Pdf.Boolean s, 0
          | CenterWindow s -> "/CenterWindow", Pdf.Boolean s, 0
          | DisplayDocTitle s -> "/DisplayDocTitle", Pdf.Boolean s, 4
          | _ -> assert false
        in
      let pdf = get_single_pdf args.op false in
     let version = if args.keepversion || pdf.Pdf.major > 1 then pdf.Pdf.minor else version in
          write_pdf false (Cpdfmetadata.set_viewer_preference (key, value, version) pdf)
      end
  | Some (OpenAtPage str) ->
      let pdf = get_single_pdf args.op false in
      let range = parse_pagespec_allow_empty pdf str in
      let n = match range with [x] -> x | _ -> error "open_at_page: range does not specify single page" in
        write_pdf false (Cpdfmetadata.set_open_action pdf false n)
  | Some (OpenAtPageFit str) ->
      let pdf = get_single_pdf args.op false in
      let range = parse_pagespec_allow_empty pdf str in
      let n = match range with [x] -> x | _ -> error "open_at_page_fit: range does not specify single page" in
        write_pdf false (Cpdfmetadata.set_open_action pdf true n)
  | Some (OpenAtPageCustom dest) ->
      let pdf = get_single_pdf args.op false in
        write_pdf false (Cpdfmetadata.set_open_action ~dest pdf true 1)
  | Some (SetMetadata metadata_file) ->
      write_pdf false (Cpdfmetadata.set_metadata args.keepversion metadata_file (get_single_pdf args.op false))
  | Some (SetVersion v) ->
      let pdf = get_single_pdf args.op false in
      let pdf =
        if v >= 10
          then {pdf with Pdf.major = 2; Pdf.minor = v - 10}
          else {pdf with Pdf.major = 1; Pdf.minor = v}
      in
         write_pdf false pdf
  | Some (SetPageLayout s) ->
      write_pdf false (Cpdfmetadata.set_page_layout (get_single_pdf args.op false) s)
  | Some (SetPageMode s) ->
      write_pdf false (Cpdfmetadata.set_page_mode (get_single_pdf args.op false) s)
  | Some (SetNonFullScreenPageMode s) ->
      write_pdf false (Cpdfmetadata.set_non_full_screen_page_mode (get_single_pdf args.op false) s)
  | Some Split ->
      begin match args.inputs, args.out with
        | [(f, ranges, _, _, _, _)], File output_spec ->
            let pdf = get_single_pdf args.op true in
            let enc = build_enc () in
              args.create_objstm <- args.preserve_objstm;
              split_pdf
                enc args.original_filename args.chunksize args.linearize ~cpdflin:args.cpdflin
                ~preserve_objstm:args.preserve_objstm ~create_objstm:args.preserve_objstm (*yes--always create if preserving *)
                ~squeeze:args.squeeze output_spec pdf
        | _, Stdout -> error "Can't split to standard output"
        | _, NoOutputSpecified -> error "Split: No output format specified"
        | _ -> error "Split: bad parameters"
      end
  | Some (SplitOnBookmarks level) ->
      begin match args.out with
        | File output_spec ->
            let pdf = get_single_pdf args.op false in
            let enc = build_enc () in
              args.create_objstm <- args.preserve_objstm;
              split_at_bookmarks
                enc args.original_filename ~squeeze:args.squeeze level output_spec pdf
        | Stdout -> error "Can't split to standard output"
        | NoOutputSpecified -> error "Split: No output format specified"
      end
  | Some (SplitMax s) ->
      begin match args.out with
        | File output_spec ->
            let pdf = get_single_pdf args.op false in
            let enc = build_enc () in
              args.create_objstm <- args.preserve_objstm;
              split_max enc args.original_filename ~squeeze:args.squeeze output_spec s pdf
        | Stdout -> error "Can't split to standard output"
        | NoOutputSpecified -> error "Split: No output format specified"
      end
  | Some Spray ->
      begin match args.inputs, args.out with
        | (_, pagespec, _, _, _, _)::_, File output_spec ->
            let pdf = get_single_pdf args.op false in
            let range = ref (parse_pagespec pdf pagespec) in
            let enc = build_enc () in
            let pagenums = map ref (many [] (length !spray_outputs)) in
            let n = ref 0 in
            while !range <> [] do
              List.nth pagenums (!n mod (length !spray_outputs)) =| hd !range;
              range := tl !range;
              n += 1;
            done;
            let names = rev !spray_outputs in
              iter (fun x -> if !x = [] then error "Spray: must have at least one page for each output") pagenums;
              args.create_objstm <- args.preserve_objstm;
              fast_write_split_pdfs ~names enc 0 args.original_filename args.squeeze output_spec pdf (map rev (map (!) pagenums)) (Pdfpage.pages_of_pagetree pdf)
        | _, Stdout -> error "Can't spray to standard output"
        | _, NoOutputSpecified -> error "Spray: No output format specified"
        | _, _ -> error "Spray: no input"
      end
  | Some Presentation ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
          let pdf' =
            Cpdfpresent.presentation
              range
              args.transition args.duration args.horizontal
              args.inward args.direction args.effect_duration pdf
          in
            pdf.Pdf.minor <- if args.keepversion || pdf.Pdf.major > 1 then pdf.Pdf.minor else max pdf.Pdf.minor 1;
            write_pdf false pdf'
  | Some ChangeId ->
      if args.recrypt then
        soft_error "Cannot recrypt with change id: an id is part of encryption information";
      begin match args.inputs, args.out with
      | [(k, _, _, _, _, _) as input], File s ->
          let pdf = get_pdf_from_input_kind input args.op k in
            write_pdf true pdf
      | [(k, _, _, _, _, _) as input], Stdout ->
          let pdf = get_pdf_from_input_kind input args.op k in
            write_pdf true pdf
      | _ -> error "ChangeId: exactly one input file and output file required."
      end
  | Some RemoveId ->
      if args.recrypt then
        soft_error "Cannot recrypt with remove id: an id is part of encryption information";
      let pdf = get_single_pdf args.op false in
        pdf.Pdf.trailerdict <- Pdf.remove_dict_entry pdf.Pdf.trailerdict "/ID";
        write_pdf false pdf
  | Some (CopyId getfrom) ->
      if args.recrypt then
        soft_error "Cannot recrypt with copy id: an id is part of encryption information";
      begin match args.inputs with
      | [(k, _, u, o, _, _) as input] ->
          let pdf =
            Cpdfmetadata.copy_id
              args.keepversion
              (pdfread_pdf_of_file (optstring u) (optstring o) getfrom)
              (get_pdf_from_input_kind input args.op k)
          in
            write_pdf false pdf
      | _ -> error "copy-id: No input file specified"
      end
  | Some (ThinLines w) ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
          write_pdf false (Cpdftweak.thinlines range w pdf)
  | Some BlackText ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
          write_pdf false (Cpdftweak.blacktext args.color range pdf)
  | Some BlackLines ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
          write_pdf false (Cpdftweak.blacklines args.color range pdf)
  | Some BlackFills ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
          write_pdf false (Cpdftweak.blackfills args.color range pdf)
  | Some RemoveAnnotations ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
          write_pdf false (Cpdfannot.remove_annotations range pdf)
  | Some (CopyAnnotations getfrom) ->
      begin match args.inputs with
      | [(k, _, u, o, _, _) as input] ->
          let input_pdf = get_pdf_from_input_kind input args.op k in
          let range = parse_pagespec_allow_empty input_pdf (get_pagespec ()) in
            Cpdfannot.copy_annotations
              range
              (pdfread_pdf_of_file (optstring u) (optstring o) getfrom)
              input_pdf;
            write_pdf false input_pdf
      | _ -> error "copy-annotations: No input file specified"
      end
  | Some (SetAnnotations json) ->
      let data = Pdfio.input_of_channel (open_in_bin json) in
      let pdf = get_single_pdf args.op false in
        Cpdfannot.set_annotations_json pdf data;
        write_pdf false pdf
  | Some ListAnnotations ->
      let pdf = get_single_pdf args.op true in
      let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
        if args.format_json then
          flprint (Pdfio.string_of_bytes (Cpdfannot.get_annotations_json pdf range))
        else
          Cpdfannot.list_annotations range args.encoding pdf
  | Some Shift ->
      let pdf = get_single_pdf args.op false in
      let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
      let dxdylist = Cpdfcoord.parse_coordinates pdf args.coord in
        write_pdf false (Cpdfpage.shift_pdf ~fast:args.fast dxdylist pdf range)
  | Some ShiftBoxes ->
      let pdf = get_single_pdf args.op false in
      let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
      let dxdylist = Cpdfcoord.parse_coordinates pdf args.coord in
        write_pdf false (Cpdfpage.shift_boxes dxdylist pdf range)
  | Some Scale ->
      let pdf = get_single_pdf args.op false in
      let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
      let sxsylist = Cpdfcoord.parse_coordinates pdf args.coord in
        write_pdf false (Cpdfpage.scale_pdf ~fast:args.fast sxsylist pdf range)
  | Some ScaleToFit ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
          warn_prerotate range pdf;
          let pdf = if args.prerotate then prerotate range pdf else pdf in
          let xylist = Cpdfcoord.parse_coordinates pdf args.coord
          and scale = args.scale in
            write_pdf false (Cpdfpage.scale_to_fit_pdf ~fast:args.fast args.position scale xylist args.op pdf range)
  | Some Stretch ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
          warn_prerotate range pdf;
          let pdf = if args.prerotate then prerotate range pdf else pdf in
          let xylist = Cpdfcoord.parse_coordinates pdf args.coord in
            write_pdf false (Cpdfpage.stretch ~fast:args.fast xylist pdf range)
  | Some CenterToFit ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
          warn_prerotate range pdf;
          let pdf = if args.prerotate then prerotate range pdf else pdf in
          let xylist = Cpdfcoord.parse_coordinates pdf args.coord in
            write_pdf false (Cpdfpage.center_to_fit xylist pdf range)
  | Some (ScaleContents scale) ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
          write_pdf false (Cpdfpage.scale_contents ~fast:args.fast args.position scale pdf range)
  | Some ListAttachedFiles ->
      let pdf = get_single_pdf args.op false in
        let attachments = Cpdfattach.list_attached_files pdf in
        iter
          (fun a -> Printf.printf "%i %s\n" a.Cpdfattach.pagenumber a.Cpdfattach.name)
          attachments;
        flprint ""
  | Some DumpAttachedFiles ->
      let pdf = get_single_pdf args.op false in
        begin match args.out with
        | NoOutputSpecified -> Cpdfattach.dump_attached_files pdf ""
        | File n -> Cpdfattach.dump_attached_files pdf n
        | Stdout -> error "Can't dump attachments to stdout"
        end
  | Some RemoveAttachedFiles ->
      write_pdf false (Cpdfattach.remove_attached_files (get_single_pdf args.op false))
  | Some (AttachFile files) ->
      begin match args.inputs with
      | [(k, _, _, _, _, _) as input] ->
          let pdf = get_pdf_from_input_kind input args.op k in
            let topage =
              try
               match args.topage with
               | None -> None
               | Some "end" -> Some (Pdfpage.endpage pdf)
               | Some s -> Some (int_of_string s)
              with _ -> error "Bad -to-page"
            in
            let pdf = fold_left (Cpdfattach.attach_file args.keepversion topage) pdf (rev files) in
              write_pdf false pdf
      | _ -> error "attach file: No input file specified"
      end
  | Some PadBefore ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
          let padwith =
            match args.padwith with
              None -> None
            | Some filename -> Some (pdfread_pdf_of_file None None filename)
          in
            write_pdf false (Cpdfpad.padbefore ?padwith range pdf)
  | Some PadAfter ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
          let padwith =
            match args.padwith with
              None -> None
            | Some filename -> Some (pdfread_pdf_of_file None None filename)
          in
            write_pdf false (Cpdfpad.padafter ?padwith range pdf)
  | Some (PadEvery n) ->
      let pdf = get_single_pdf args.op false in
        let range =
          match keep (function m -> m mod n = 0) (ilist 1 (Pdfpage.endpage pdf)) with
          | [] -> []
          | l -> if last l = Pdfpage.endpage pdf then all_but_last l else l
        in
          let padwith =
            match args.padwith with
              None -> None
            | Some filename -> Some (pdfread_pdf_of_file None None filename)
          in
            write_pdf false (Cpdfpad.padafter ?padwith range pdf)
  | Some (PadMultiple n) ->
      let pdf = get_single_pdf args.op false in
        write_pdf false (Cpdfpad.padmultiple n pdf)
  | Some (PadMultipleBefore n) ->
      let pdf = get_single_pdf args.op false in
        write_pdf false (Cpdfpad.padmultiple (-n) pdf)
  | Some Draft ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
          write_pdf false (Cpdfdraft.draft args.removeonly args.boxes range pdf)
  | Some (AddText text) ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
          let cpdffont = embed_font () in
            warn_prerotate range pdf;
            let pdf =
              if args.prerotate then prerotate range pdf else pdf
            and filename =
              match args.inputs with
              | (InFile inname, _, _, _, _, _)::_ -> inname
              | _ -> ""
            in
              write_pdf false
                (Cpdfaddtext.addtexts
                   args.linewidth args.outline args.fast args.fontname
                   cpdffont args.bates args.batespad args.color args.position
                   args.linespacing args.fontsize args.underneath text range
                   args.relative_to_cropbox args.opacity
                   args.justification args.midline args.topline filename
                   args.extract_text_font_size args.coord ~raw:(args.encoding = Raw) pdf)
  | Some RemoveText ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
          write_pdf false (Cpdfremovetext.removetext range pdf)
  | Some AddRectangle ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
          write_pdf false
            (Cpdfaddtext.addrectangle
               args.fast (Cpdfcoord.parse_coordinate pdf args.coord)
               args.color args.outline args.linewidth args.opacity args.position
               args.relative_to_cropbox args.underneath range pdf)
  | Some (AddBookmarks file) ->
      write_pdf false
        (Cpdfbookmarks.add_bookmarks ~json:args.format_json true (Pdfio.input_of_channel (open_in_bin file))
          (get_single_pdf args.op false))
  | Some RemoveBookmarks ->
      write_pdf false (Pdfmarks.remove_bookmarks (get_single_pdf args.op false))
  | Some TwoUp ->
      write_pdf false (Cpdfimpose.twoup args.fast (get_single_pdf args.op false))
  | Some TwoUpStack ->
      write_pdf false (Cpdfimpose.twoup_stack args.fast (get_single_pdf args.op false))
  | Some Impose fit ->
      let pdf = get_single_pdf args.op false in
      let x, y = Cpdfcoord.parse_coordinate pdf args.coord in
        if not fit && (x < 0.0 || y < 0.0) then error "Negative imposition parameters not allowed." else
        write_pdf false
          (Cpdfimpose.impose ~x ~y ~fit ~columns:args.impose_columns ~rtl:args.impose_rtl ~btt:args.impose_btt ~center:args.impose_center
                      ~margin:args.impose_margin ~spacing:args.impose_spacing ~linewidth:args.impose_linewidth ~fast:args.fast pdf)
  | Some (StampOn over) ->
      let overpdf =
        match over with
        | "stamp_use_stdin" -> pdf_of_stdin "" ""
        | x -> pdfread_pdf_of_file None None x
      in
        let pdf = get_single_pdf args.op false in
          let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
            let pdf =
              Cpdfpage.stamp
                args.relative_to_cropbox args.position args.topline args.midline args.fast
                args.scale_stamp_to_fit true range overpdf pdf
            in
              write_pdf false pdf
  | Some (StampUnder under) ->
      let underpdf =
        match under with
        | "stamp_use_stdin" -> pdf_of_stdin "" ""
        | x -> pdfread_pdf_of_file None None x
      in
        let pdf = get_single_pdf args.op false in
          let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
            let pdf =
              Cpdfpage.stamp
                args.relative_to_cropbox args.position args.topline args.midline args.fast
                args.scale_stamp_to_fit false range underpdf pdf
            in
              write_pdf false pdf
  | Some (CombinePages over) ->
      let underpdf = get_single_pdf args.op false in
      let overpdf = pdfread_pdf_of_file None None over in
        warn_prerotate (parse_pagespec underpdf "all") underpdf;
        warn_prerotate (parse_pagespec overpdf "all") overpdf;
        write_pdf false
          (Cpdfpage.combine_pages
             args.fast
               (prerotate (parse_pagespec underpdf "all") underpdf)
               (prerotate (parse_pagespec overpdf "all") overpdf)
               false false true)
  | Some Encrypt ->
      let pdf = get_single_pdf args.op false in
        let pdf = Cpdfsqueeze.recompress_pdf pdf
        and encryption = build_enc () in
          Pdf.remove_unreferenced pdf;
          if not args.keepversion then
            begin
              let newversion =
                match args.crypt_method with
                "40bit" -> 1 | "128bit" -> 4 | "AES" -> 6 | "AES256" | "AES256ISO" -> 7 | _ -> 0
              in
                let newversion = if args.create_objstm then 5 else newversion in
                  if pdf.Pdf.major = 1 then pdf.Pdf.minor <- max pdf.Pdf.minor newversion
            end;
            write_pdf ~encryption false pdf
  | Some Decrypt ->
      args.recrypt <- false;
      write_pdf false (get_single_pdf args.op false)
  | Some RemoveMetadata ->
      write_pdf false (Cpdfmetadata.remove_metadata (get_single_pdf args.op false))
  | Some ExtractImages ->
      let output_spec =
        begin match args.out with
        | File output_spec -> output_spec
        | _ -> ""
        end
      in
        let pdf = get_single_pdf args.op true in
          let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
            Cpdfimage.extract_images ~raw:(args.encoding = Cpdfmetadata.Raw) ?path_to_p2p:(match args.path_to_p2p with "" -> None | x -> Some x) ?path_to_im:(match args.path_to_im with "" -> None | x -> Some x) args.encoding args.dedup args.dedup_per_page pdf range output_spec
  | Some (ImageResolution f) ->
      let pdf = get_single_pdf args.op true in
      let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
        if args.format_json then
          flprint (Pdfio.string_of_bytes (Cpdfimage.image_resolution_json pdf range f))
        else
          let images = Cpdfimage.image_resolution pdf range f in
            iter
              (function (pagenum, xobject, w, h, wdpi, hdpi, objnum) ->
                 Printf.printf "%i, %s, %i, %i, %f, %f, %i\n" pagenum xobject w h wdpi hdpi objnum)
               images
  | Some ListImages ->
      let pdf = get_single_pdf args.op true in
      let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
      let json = Cpdfimage.images pdf range in
        if args.format_json then
          flprint (Cpdfyojson.Safe.pretty_to_string json)
        else
          begin match json with
          | `List l ->
              iter
                (function (`Assoc [(_, `Int i); (_, `List pages); (_, `String name); (_, `Int w); (_, `Int h); (_, `Int size); (_, bpc); (_, cs); (_, `String filter)]) ->
                   let pages = combine_with_spaces (map (function `Int i -> string_of_int i | _ -> "") pages) in
                   let bpc = match bpc with `Int bpc -> string_of_int bpc | _ -> "none" in
                   let cs = match cs with `String cs -> cs | _ -> "none" in
                     flprint (Printf.sprintf "%i, %s, %s, %i, %i, %i, %s, %s, %s\n" i pages name w h size bpc cs filter)
                 | _ -> ())
                l
          | _ -> ()
          end
  | Some MissingFonts ->
      let pdf = get_single_pdf args.op true in
        let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
          Cpdffont.missing_fonts pdf range
  | Some ExtractText ->
      let pdf = get_single_pdf args.op true in
        let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
          let text = Cpdfextracttext.extract_text args.extract_text_font_size pdf range in
            begin match args.out with
            | File filename ->
                let fh = open_out_bin filename in
                  output_string fh text;
                  close_out fh
            | NoOutputSpecified | Stdout ->
                print_string text;
                print_newline ()
            end
  | Some AddPageLabels ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec pdf (get_pagespec ()) in
          Cpdfpagelabels.add_page_labels
            pdf args.labelsprogress args.labelstyle args.labelprefix args.labelstartval range;
          write_pdf false pdf
  | Some RemovePageLabels ->
      let pdf = get_single_pdf args.op false in
        Pdfpagelabels.remove pdf;
        write_pdf false pdf
  | Some PrintPageLabels ->
      let pdf = get_single_pdf args.op true in
        if args.format_json then
          let json_of_pagelabel l =
            `Assoc
              [("labelstyle", `String (Pdfpagelabels.string_of_labelstyle l.Pdfpagelabels.labelstyle));
               ("labelprefix", begin match l.Pdfpagelabels.labelprefix with None -> `Null | Some s -> `String s end);
               ("startpage", `Int l.Pdfpagelabels.startpage);
               ("startvalue", `Int l.Pdfpagelabels.startvalue)]
          in
            flprint (Cpdfyojson.Safe.pretty_to_string (`List (map json_of_pagelabel (Pdfpagelabels.read pdf))))
        else
          iter
            print_string
            (map Pdfpagelabels.string_of_pagelabel (Pdfpagelabels.read pdf))
  | Some (RemoveDictEntry key) ->
      let pdf = get_single_pdf args.op true in
        Cpdftweak.remove_dict_entry pdf key args.dict_entry_search;
        write_pdf false pdf
  | Some (ReplaceDictEntry key) ->
      let pdf = get_single_pdf args.op true in
        Cpdftweak.replace_dict_entry pdf key args.replace_dict_entry_value args.dict_entry_search;
        write_pdf false pdf
  | Some (PrintDictEntry key) ->
      let pdf = get_single_pdf args.op true in
        Cpdftweak.print_dict_entry ~utf8:(args.encoding = Cpdfmetadata.UTF8) pdf key
  | Some ListSpotColours ->
      let pdf = get_single_pdf args.op false in
        Cpdfspot.list_spot_colours pdf
  | Some RemoveClipping ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
          write_pdf false (Cpdftweak.remove_clipping pdf range)
  | Some CreateMetadata ->
      let pdf = get_single_pdf args.op false in
        write_pdf false (Cpdfmetadata.create_metadata pdf)
  | Some EmbedMissingFonts ->
      let fi =
        match args.inputs with
          [(InFile fi, _, _, _, _, _)] -> fi
        | _ -> error "Input method not supported for -embed-missing-fonts"
      in
      let fo =
        match args.out with
          File fo -> fo
        | _ -> error "Output method not supported for -embed-missing-fonts"
      in
        Cpdffont.embed_missing_fonts args.path_to_ghostscript args.gs_quiet fi fo
  | Some (BookmarksOpenToLevel n) ->
      let pdf = get_single_pdf args.op false in
        write_pdf false (Cpdfbookmarks.bookmarks_open_to_level n pdf)
  | Some CreatePDF ->
      begin match args.subformat with
      | Some Cpdfua.PDFUA1 ->
          begin match args.title with None -> error "Provide -title" | _ -> () end;
          let pdf = Cpdfua.create_pdfua1 (unopt args.title) args.createpdf_pagesize args.createpdf_pages in
            write_pdf false pdf
      | Some Cpdfua.PDFUA2 ->
          begin match args.title with None -> error "Provide -title" | _ -> () end;
          let pdf = Cpdfua.create_pdfua2 (unopt args.title) args.createpdf_pagesize args.createpdf_pages in
            write_pdf false pdf
      | None ->
          let pdf = Cpdfcreate.blank_document_paper args.createpdf_pagesize args.createpdf_pages in
            write_pdf false pdf
      end
  | Some RemoveAllText ->
      let pdf = get_single_pdf args.op false in
      let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
        write_pdf false (Cpdfremovetext.remove_all_text range pdf)
  | Some ShowBoxes ->
      let pdf = get_single_pdf args.op false in
      let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
        write_pdf false (Cpdfpage.show_boxes pdf range)
  | Some TrimMarks ->
      let pdf = get_single_pdf args.op false in
      let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
        write_pdf false (Cpdfpage.trim_marks pdf range)
  | Some (Postpend s | Prepend s as x) ->
      let pdf = get_single_pdf args.op false in
      let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
      let before = match x with Prepend _ -> true | _ -> false in
        write_pdf false (Cpdftweak.append_page_content s before args.fast range pdf)
  | Some OutputJSON ->
      let pdf = get_single_pdf args.op false in
        write_json args.out pdf
  | Some OCGCoalesce ->
      let pdf = get_single_pdf args.op false in
        Cpdfocg.ocg_coalesce pdf;
        write_pdf false pdf
  | Some OCGList ->
      let pdf = get_single_pdf args.op true in
        Cpdfocg.ocg_list pdf
  | Some OCGRename ->
      let pdf = get_single_pdf args.op false in
        Cpdfocg.ocg_rename args.ocgrenamefrom args.ocgrenameto pdf;
        write_pdf false pdf
  | Some OCGOrderAll ->
      let pdf = get_single_pdf args.op false in
        Cpdfocg.ocg_order_all pdf;
        write_pdf false pdf
  | Some (StampAsXObject stamp) ->
      let stamp_pdf =
        match stamp with
        | "stamp_use_stdin" -> pdf_of_stdin "" ""
        | x -> pdfread_pdf_of_file None None x
      in
        let pdf = get_single_pdf args.op false in
        let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
          let pdf, xobj_name =
            Cpdfxobject.stamp_as_xobject pdf range stamp_pdf
          in
            Printf.printf "%s\n" xobj_name;
            flush stdout;
            write_pdf false pdf
  | Some (PrintFontEncoding fontname) ->
      let pdf = get_single_pdf args.op true in
        Cpdffont.print_font_table pdf fontname args.copyfontpage
  | Some TableOfContents ->
      let pdf = get_single_pdf args.op false in
      let cpdffont = embed_font () in
      let pdf =
        Cpdftoc.typeset_table_of_contents
          ~font:cpdffont ~fontsize:args.fontsize ~title:args.toc_title ~bookmark:args.toc_bookmark pdf
      in
        write_pdf false pdf
  | Some (Typeset filename) ->
      let text = Pdfio.bytes_of_input_channel (open_in_bin filename) in
      let cpdffont = embed_font () in
      let pdf = Cpdftexttopdf.typeset ~process_struct_tree:args.process_struct_trees 
      ?subformat:args.subformat ?title:args.title ~font:cpdffont ~papersize:args.createpdf_pagesize ~fontsize:args.fontsize text in
        write_pdf false pdf
  | Some (TextWidth s) ->
      let rawwidth =
        match args.font with
        | StandardFont f ->
            Pdfstandard14.textwidth false WinAnsiEncoding f s
        | _ ->
            error "-text-width only works for the standard 14 fonts"
      in
        let w = (float rawwidth *. args.fontsize) /. 1000. in
          Printf.printf "%f\n" w
  | Some Draw ->
      let pdf = get_single_pdf args.op false in
      let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
      let ops = match !Cpdfdrawcontrol.drawops with [("_MAIN", ops)] -> rev ops | _ -> error "not enough -end-xobj or -et" in
        write_pdf
          false
          (Cpdfdraw.draw ~struct_tree:args.draw_struct_tree ~fast:args.fast ~underneath:args.underneath ~filename:args.original_filename ~bates:args.bates ~batespad:args.batespad range pdf ops)
  | Some (Composition json) ->
      let pdf = get_single_pdf args.op false in
      let filesize =
        match args.inputs with
        | (InFile inname, _, _, _, _, _)::_ -> filesize inname
        | _ -> 0
      in
        Cpdfcomposition.show_composition filesize json pdf
  | Some (Chop (x, y)) ->
      let pdf = get_single_pdf args.op false in
      let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
        write_pdf false (Cpdfchop.chop ~x ~y ~columns:args.impose_columns ~btt:args.impose_btt ~rtl:args.impose_rtl pdf range)
  | Some (ChopHV (is_h, line)) ->
      let pdf = get_single_pdf args.op false in
      let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
        write_pdf false (Cpdfchop.chop_hv ~is_h ~line ~columns:args.impose_columns pdf range)
  | Some ProcessImages ->
      let pdf = get_single_pdf args.op false in
      let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
        Cpdfimage.process
          ~q:args.jpegquality ~qlossless:args.jpegqualitylossless ~onebppmethod:args.onebppmethod ~jbig2_lossy_threshold:args.jbig2_lossy_threshold
          ~length_threshold:args.length_threshold ~percentage_threshold:args.percentage_threshold ~pixel_threshold:args.pixel_threshold 
          ~dpi_threshold:args.dpi_threshold ~factor:args.resample_factor ~interpolate:args.resample_interpolate
          ~jpeg_to_jpeg_scale:args.jpegtojpegscale ~jpeg_to_jpeg_dpi:args.jpegtojpegdpi
          ~path_to_jbig2enc:args.path_to_jbig2enc ~path_to_convert:args.path_to_im range pdf;
        write_pdf false pdf
  | Some (ExtractStream s) ->
      let pdf = get_single_pdf args.op true in
        extract_stream pdf args.extract_stream_decompress s
  | Some (PrintObj s) ->
      let pdf = get_single_pdf args.op true in
        print_obj args.format_json pdf s
  | Some (ReplaceObj (a, b)) ->
      let pdf = get_single_pdf args.op false in
      let pdfobj = Cpdfjson.object_of_json (Cpdfyojson.Safe.from_string b) in
        replace_obj pdf a pdfobj;
        write_pdf false pdf
  | Some (Verify standard) ->
      begin match standard with
      | "PDF/UA-1(matterhorn)" ->
          let pdf = get_single_pdf args.op false in
          let testname = match args.verify_single with None -> "" | Some x -> x in
            if args.format_json
              then flprint (Cpdfyojson.Safe.pretty_to_string (Cpdfua.test_matterhorn_json pdf testname))
              else Cpdfua.test_matterhorn_print pdf testname
      | _ -> error "Unknown verification type."
      end
  | Some (MarkAs standard) ->
      begin match standard with
      | Cpdfua.PDFUA1 ->
          let pdf = get_single_pdf args.op false in
            Cpdfua.mark pdf;
            write_pdf false pdf
      | Cpdfua.PDFUA2 ->
          let pdf = get_single_pdf args.op false in
            Cpdfua.mark2 2024 pdf;
            write_pdf false pdf
      end
  | Some (RemoveMark standard) ->
      begin match standard with
      | Cpdfua.PDFUA1 | Cpdfua.PDFUA2 ->
          let pdf = get_single_pdf args.op false in
            Cpdfua.remove_mark pdf;
            write_pdf false pdf
      end
  | Some PrintStructTree ->
      let pdf = get_single_pdf args.op true in
        Cpdfua.print_struct_tree pdf
  | Some ExtractStructTree ->
      let pdf = get_single_pdf args.op true in
      let json = Cpdfua.extract_struct_tree pdf in
        json_to_output json args.out
  | Some (ReplaceStructTree s) ->
      let pdf = get_single_pdf args.op false in
      let json = Cpdfyojson.Safe.from_file s in
        Cpdfua.replace_struct_tree pdf json;
        write_pdf false pdf
  | Some (SetLanguage s) ->
      let pdf = get_single_pdf args.op false in
        Cpdfmetadata.set_language pdf s;
        write_pdf false pdf
  | Some Redact ->
      let pdf = get_single_pdf args.op false in
      let range = parse_pagespec_allow_empty pdf (get_pagespec ()) in
        write_pdf false (Cpdfpage.redact ~process_struct_tree:args.process_struct_trees pdf range)

(* Advise the user if a combination of command line flags makes little sense,
or error out if it make no sense at all. *)
let check_command_line () =
  if args.gs_malformed && !Pdfread.error_on_malformed then
    error "Setting both -gs-malformed and -error-on-malformed makes no sense"

let parse_argv () s specs anon_fun usage_msg =
  if args.debug then
    Array.iter (fun s -> Pdfe.log (Printf.sprintf "arg: %s\n" s)) Sys.argv;
  Arg.parse_argv ~current:(ref 0) s specs anon_fun usage_msg;
  check_command_line ()

let align_specs s =
  Arg.align s

(* The old -control mechanism clashed with AND, but must be retained for
backwards compatibility. There is a new mechanism -args file which performs
direct textual substitution of the file, before any expansion of ANDs *)
let rec expand_args_inner prev = function
    [] -> rev prev
  | "-args"::filename::r ->
      expand_args_inner (rev (parse_control_file filename) @ prev) r
  | "-args-json"::filename::r ->
      expand_args_inner (rev (parse_control_file_json filename) @ prev) r
  | h::t -> expand_args_inner (h::prev) t

let expand_args argv =
  let l = Array.to_list argv in
    Array.of_list (expand_args_inner [] l)

let gs_malformed_force fi fo =
  if args.path_to_ghostscript = "" then begin
    Pdfe.log "Please supply path to gs with -gs\n";
    exit 2
  end;
    let gscall =
      Filename.quote_command args.path_to_ghostscript
        ((if args.gs_quiet then ["-dQUIET"] else []) @
        ["-dNOPAUSE"; "-sDEVICE=pdfwrite"; "-sOUTPUTFILE=" ^ fo; "-dBATCH"; fi])
    in
      match Sys.command gscall with
      | 0 -> exit 0
      | _ -> Pdfe.log "Failed to mend file.\n"; exit 2

let process_env_vars () =
  match Sys.getenv_opt "CPDF_DEBUG" with
  | Some "true" -> args.debug <- true
  | Some "false" -> args.debug <- false
  | _ -> ()

(* Main function. *)
let go_withargv argv =
  (* Check for the standalone -gs-malformed-force special command line. This
   * has exactly one file input and exactly one output and just -gs <gs>
   * -gs-malformed-force between.  *)
  match argv with
  | [|_|] -> print_version ()
  | [|_; inputfilename; "-gs"; gslocation; "-gs-malformed-force"; "-o"; outputfilename|] ->
    args.path_to_ghostscript <- gslocation;
    ignore (gs_malformed_force inputfilename outputfilename);
    exit 0
  | [|_; inputfilename; "-gs"; gslocation; "-gs-malformed-force"; "-o"; outputfilename; "-gs-quiet"|] ->
    args.path_to_ghostscript <- gslocation;
    args.gs_quiet <- true;
    ignore (gs_malformed_force inputfilename outputfilename);
    exit 0
  | _ -> 
  Hashtbl.clear filenames;
  if demo then
    flprint "This demo functions normally, but is for evaluation only. https://www.coherentpdf.com/\n";
  try
    (* Pre-expand -args *)
    let argv = expand_args argv in
    (* Split the arguments into sets either side of ANDs *)
    let sets =
      let args =
        (map (fun l -> "cpdf"::l) (split_around (eq "AND") (tl (Array.to_list argv))))
      in
        match args with
        | [] -> []
        | _ -> combine (map Array.of_list args) (map (eq (length args)) (ilist 1 (length args)))
    in
      iter
        (fun (s, islast) ->
           (*Printf.printf "AND:%b, %s\n" islast (Array.fold_left (fun x y -> x  ^ " " ^ y) "" s);
           flprint "\n";*)
           reset_arguments ();
           Cpdfdrawcontrol.drawops := [("_MAIN", [])];
           process_env_vars ();
           parse_argv () s (align_specs specs) anon_fun usage_msg;
           let addrange pdf = AlreadyInMemory (pdf, "fromAND"), args.dashrange, "", "", ref false, None in
             args.inputs <- rev (map addrange !output_pdfs) @ rev args.inputs;
             output_pdfs := [];
             go ())
         sets;
      flush stdout; (*r for Windows *)
      exit 0
  with
  | Arg.Bad s ->
      Pdfe.log
        (implode (takewhile (neq '\n') (explode s)) ^ " Use -help for help.\n\n");
      if not !stay_on_error then exit 2 else raise StayOnError
  | Arg.Help _ ->
      Arg.usage (align_specs specs) usage_msg;
      flush stderr (*r for Windows *)
  | Sys_error s as e ->
      Pdfe.log (s ^ "\n\n");
      if not !stay_on_error then
        (if args.debug then raise e else exit 2)
      else raise StayOnError
  | Pdf.PDFError s as e ->
      Pdfe.log
        ("cpdf encountered an error. Technical details follow:\n\n" ^ s ^ "\n\n");
      if not !stay_on_error then
        if args.debug then raise e else exit 2
      else
        raise StayOnError
  | Cpdferror.SoftError s -> soft_error s
  | Cpdferror.HardError s -> error s
  | e ->
      Pdfe.log
        ("cpdf encountered an unexpected error. Technical Details follow:\n" ^
         Printexc.to_string e ^ "\n\n");
      if not !stay_on_error then
        (if args.debug then raise e else exit 2) else raise StayOnError

let go () =
  go_withargv Sys.argv
