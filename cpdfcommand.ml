(* Added -relative-to-cropbox for stamps *)
(* cpdf command line tools *)
let demo = false
let noncomp = false
let major_version = 2
let minor_version = 2
let version_date = "(build of 30th July 2016)"

open Pdfutil
open Pdfio

let initial_file_size = ref 0

let empty = Pdf.empty ()
let emptypage = Pdfpage.blankpage Pdfpaper.a4

(* Wrap up the file reading functions to exit with code 1 when an encryption
problem occurs. This happens when object streams are in an encrypted document
and so it can't be read without the right password... The existing error
handling only dealt with the case where the document couldn't be decrypted once
it had been loaded. *)
let pdfread_pdf_of_input ?revision a b c =
  try Pdfread.pdf_of_input ?revision a b c with
    Pdf.PDFError s when String.length s >=10 && String.sub s 0 10 = "Encryption" ->
      raise (Cpdf.SoftError "Bad owner or user password when reading document")

let pdfread_pdf_of_channel_lazy ?revision ?source b c d =
  try Pdfread.pdf_of_channel_lazy ?revision ?source b c d with
    Pdf.PDFError s when String.length s >=10 && String.sub s 0 10 = "Encryption" ->
      raise (Cpdf.SoftError "Bad owner or user password when reading document")

let pdfread_pdf_of_file ?revision a b c =
  try Pdfread.pdf_of_file ?revision a b c with
    Pdf.PDFError s when String.length s >=10 && String.sub s 0 10 = "Encryption" ->
      raise (Cpdf.SoftError "Bad owner or user password when reading document")

let optstring = function
  | "" -> None
  | x -> Some x

(* To prevent problems when piping on Windows *)
let _ =
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true

let stay_on_error = ref false

exception StayOnError

(* Fatal error reporting. *)
let error s =
  prerr_string (s ^ "\nUse -help for help.\n");
  flush stderr;
  if not !stay_on_error then exit 2 else raise StayOnError

let soft_error s =
  Printf.eprintf "%s\n" s;
  flush stderr;
  if not !stay_on_error then exit 1 else raise StayOnError

let parse_pagespec pdf spec =
  try Cpdf.parse_pagespec pdf spec with
    Failure x -> error x

(* Operations. *)
type op =
  | CopyFont of string
  | CSP1
  | CSP2 of float
  | CSP3
  | CountPages
  | Version
  | Encrypt
  | Decrypt
  | StampOn of string
  | StampUnder of string
  | CombinePages of string
  | TwoUp
  | TwoUpStack
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
  | Shift
  | Scale
  | ScaleToFit
  | ScaleContents of float
  | AttachFile of string list
  | RemoveAttachedFiles
  | ListAttachedFiles
  | DumpAttachedFiles
  | RemoveAnnotations
  | ListAnnotations
  | ListAnnotationsMore
  | CopyAnnotations of string
  | Merge
  | Split
  | SplitOnBookmarks of int
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
  | RemoveCrop
  | CopyCropBoxToMediaBox
  | CopyBox
  | MediaBox
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
  | ImageResolution of float
  | MissingFonts
  | RemoveUnusedResources
  | ExtractFontFile
  | ExtractText
  | OpenAtPage of int
  | OpenAtPageFit of int
  | AddPageLabels
  | RemovePageLabels
  | PrintPageLabels
  | Revisions
  | RemoveDictEntry of string
  | ListSpotColours

let string_of_op = function
  | CopyFont _ -> "CopyFont"
  | CSP1 -> "CSP1"
  | CSP2 _ -> "CSP2"
  | CSP3 -> "CSP3"
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
  | Shift -> "Shift"
  | Scale -> "Scale"
  | ScaleToFit -> "ScaleToFit"
  | ScaleContents _ -> "ScaleContents"
  | AttachFile _ -> "AttachFile"
  | RemoveAttachedFiles -> "RemoveAttachedFiles"
  | ListAttachedFiles -> "ListAttachedFiles"
  | DumpAttachedFiles -> "DumpAttachedFiles"
  | RemoveAnnotations -> "RemoveAnnotations"
  | ListAnnotations -> "ListAnnotations"
  | ListAnnotationsMore -> "ListAnnotationsMore"
  | CopyAnnotations _ -> "CopyAnnotations"
  | Merge -> "Merge"
  | Split -> "Split"
  | SplitOnBookmarks _ -> "SplitOnBookmarks"
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
  | CopyCropBoxToMediaBox -> "CopyCropBoxToMediaBox"
  | CopyBox -> "CopyBox"
  | MediaBox -> "MediaBox"
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
  | ImageResolution _ -> "ImageResolution"
  | MissingFonts -> "MissingFonts"
  | RemoveUnusedResources -> "RemoveUnusedResources"
  | ExtractFontFile -> "ExtractFontFile"
  | ExtractText -> "ExtractText"
  | OpenAtPage _ -> "OpenAtPage"
  | OpenAtPageFit _ -> "OpenAtPageFit"
  | AddPageLabels -> "AddPageLabels"
  | RemovePageLabels -> "RemovePageLabels"
  | PrintPageLabels -> "PrintPageLabels"
  | Revisions -> "Revisions"
  | RemoveDictEntry _ -> "RemoveDictEntry"
  | ListSpotColours -> "ListSpotColours"

(* Inputs: filename, pagespec. *)
type input_kind =
  | AlreadyInMemory of Pdf.t
  | InFile of string
  | StdIn

let string_of_input_kind = function
  | AlreadyInMemory _ -> "AlreadyInMemory"
  | InFile s -> s
  | StdIn -> "Stdin"

type input =
  input_kind * string * string * string * bool ref * int option
  (* input kind, range, user_pw, owner_pw, was_decrypted_with_owner, revision *)

type output_method =
  | NoOutputSpecified
  | Stdout
  | File of string

(* A list of PDFs to be output, if no output method was specified. *)
let output_pdfs : Pdf.t list ref = ref []

type font =
  | StandardFont of Pdftext.standard_font
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
   mutable fontsize : float;
   mutable color : float * float * float;
   mutable opacity : float;
   mutable position : Cpdf.position;
   mutable underneath : bool;
   mutable linespacing : float;
   mutable midline : bool;
   mutable topline : bool;
   mutable justification : Cpdf.justification;
   mutable bates : int;
   mutable batespad : int option;
   mutable prerotate : bool;
   mutable orientation : Cpdf.orientation;
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
   mutable boxes : bool;
   mutable encrypt_metadata : bool;
   mutable retain_numbering : bool;
   mutable remove_duplicate_fonts : bool;
   mutable remove_duplicate_streams : bool;
   mutable encoding : Cpdf.encoding;
   mutable scale : float;
   mutable copyfontpage : int;
   mutable copyfontname : string option;
   mutable fast : bool;
   mutable dashrange : string;
   mutable outline : bool;
   mutable linewidth : float;
   mutable path_to_ghostscript : string;
   mutable frombox : string option;
   mutable tobox : string option;
   mutable mediabox_if_missing : bool;
   mutable topage : string option;
   mutable scale_stamp_to_fit : bool;
   mutable keep_this_id : string option;
   mutable makenewid : bool;
   mutable labelstyle : Pdfpagelabels.labelstyle;
   mutable labelprefix : string option;
   mutable labelstartval : int;
   mutable squeeze : bool;
   mutable original_filename : string;
   mutable was_encrypted : bool;
   mutable cpdflin : string option;
   mutable recrypt : bool;
   mutable was_decrypted_with_owner : bool;
   mutable creator : string option;
   mutable producer : string option;
   mutable embedfonts : bool}

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
   color = 0., 0., 0.;
   opacity = 1.;
   position = Cpdf.TopLeft 100.;
   underneath = false;
   linespacing = 1.;
   midline = false;
   topline = false;
   justification = Cpdf.LeftJustify;
   bates = 0;
   batespad = None;
   prerotate = false;
   orientation = Cpdf.Horizontal;
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
   boxes = false;
   encrypt_metadata = true;
   retain_numbering = false;
   remove_duplicate_fonts = false;
   remove_duplicate_streams = false;
   encoding = Cpdf.Stripped;
   scale = 1.;
   copyfontpage = 1;
   copyfontname = None;
   fast = false;
   dashrange = "all";
   outline = false;
   linewidth = 1.0;
   path_to_ghostscript = "";
   frombox = None;
   tobox = None;
   mediabox_if_missing = false;
   topage = None;
   scale_stamp_to_fit = false;
   keep_this_id = None;
   makenewid = false;
   labelstyle = Pdfpagelabels.DecimalArabic;
   labelprefix = None;
   labelstartval = 1;
   squeeze = false;
   original_filename = "";
   was_encrypted = false;
   cpdflin = None;
   recrypt = false;
   was_decrypted_with_owner = false;
   producer = None;
   creator = None;
   embedfonts = true}

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
  args.color <- 0., 0., 0.;
  args.opacity <- 1.;
  args.position <- Cpdf.TopLeft 100.;
  args.underneath <- false;
  args.linespacing <- 1.;
  args.midline <- false;
  args.topline <- false;
  args.justification <- Cpdf.LeftJustify;
  args.bates <- 0;
  args.batespad <- None;
  args.prerotate <- false;
  args.orientation <- Cpdf.Horizontal;
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
  args.remove_duplicate_fonts <- false;
  args.remove_duplicate_streams <- false;
  args.encoding <- Cpdf.Stripped;
  args.scale <- 1.;
  args.copyfontpage <- 1;
  args.copyfontname <- None;
  args.fast <- false;
  args.dashrange <- "all";
  args.outline <- false;
  args.linewidth <- 1.0;
  args.path_to_ghostscript <- "";
  args.frombox <- None;
  args.tobox <- None;
  args.mediabox_if_missing <- false;
  args.topage <- None;
  args.scale_stamp_to_fit <- false;
  args.keep_this_id <- None;
  args.makenewid <- false;
  args.labelstyle <- Pdfpagelabels.DecimalArabic;
  args.labelprefix <- None;
  args.labelstartval <- 1;
  args.squeeze <- false;
  args.producer <- None;
  args.creator <- None;
  args.embedfonts <- true
  (* Do not reset original_filename or cpdflin or was_encrypted or
   * was_decrypted_with_owner or recrypt, since we want these to work across ANDs. *)

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
  | Fonts | Info | Metadata | PageInfo | Revisions | CountPages
  | ListAttachedFiles | ListAnnotationsMore | ListAnnotations
  | ListBookmarks | ImageResolution _ | MissingFonts
  | PrintPageLabels | Clean | Compress | Decompress
  | RemoveUnusedResources | ChangeId | CopyId _ | ListSpotColours | Version
  | DumpAttachedFiles -> false (* Always allowed *)
  (* Combine pages is not allowed because we would not know where to get the
  -recrypt from -- the first or second file? *)
  | Decrypt | Encrypt | CombinePages _ -> true (* Never allowed *)
  | ExtractText | ExtractImages | ExtractFontFile -> mem Pdfcrypt.NoExtract banlist
  | AddBookmarks _ | PadBefore | PadAfter | PadEvery _ | PadMultiple _
  | Merge | Split | SplitOnBookmarks _ | RotateContents _ | Rotate _
  | Rotateby _ | Upright | VFlip | HFlip | SetPageLayout _
  | SetPageMode _ | HideToolbar _ | HideMenubar _ | HideWindowUI _
  | FitWindow _ | CenterWindow _ | DisplayDocTitle _
  | RemoveId | OpenAtPageFit _ | OpenAtPage _
  | AddPageLabels | RemovePageLabels -> mem Pdfcrypt.NoAssemble banlist
  | CSP1|CSP3|TwoUp|TwoUpStack|RemoveBookmarks|AddRectangle|RemoveText|
    Draft|Shift|Scale|ScaleToFit|RemoveAttachedFiles|
    RemoveAnnotations|RemoveMetadata|RemoveFonts|Crop|RemoveCrop|
    CopyCropBoxToMediaBox|CopyBox|MediaBox|SetTrapped|SetUntrapped|Presentation|
    BlackText|BlackLines|BlackFills|CopyFont _|CSP2 _|StampOn _|StampUnder _|
    AddText _|ScaleContents _|AttachFile _|CopyAnnotations _|SetMetadata _|
    ThinLines _|SetAuthor _|SetTitle _|SetSubject _|SetKeywords _|SetCreate _|
    SetModify _|SetCreator _|SetProducer _|SetVersion _|RemoveDictEntry _ ->
      mem Pdfcrypt.NoEdit banlist

let operation_allowed pdf banlist op =
  match op with
  | None ->
      if args.debugcrypt then Printf.printf "operation is None, so allowed!\n";
      true (* Merge *) (* changed to allow it *)
  | Some op ->
      if args.debugcrypt then Printf.printf "operation_allowed: op = %s\n" (string_of_op op);
      if args.debugcrypt then Printf.printf "Permissions: %s\n" (getpermissions pdf);
      not (banned banlist op)

let rec decrypt_if_necessary (_, _, user_pw, owner_pw, was_dec_with_owner, _) op pdf =
  if args.debugcrypt then
    begin match op with
      None -> flprint "decrypt_if_necessary: op = None\n"
    | Some x -> Printf.printf "decrypt_if_necessary: op = %s\n" (string_of_op x)
    end;
  if not (Pdfcrypt.is_encrypted pdf) then pdf else
    match op with Some (CombinePages _) ->
      (* This is a hack because we don't have support for recryption on combine
       * pages. This is pervented by permissions above, but in the case that the
       * owner password is blank (e.g christmas_tree_lights.pdf), we would end
       * up here. *)
      soft_error "Combine pages: both files must be unencrypted for this operation."
    | _ -> 
      match Pdfcrypt.decrypt_pdf_owner owner_pw pdf with
      | Some pdf ->
          args.was_decrypted_with_owner <- true;
          was_dec_with_owner := true;
          if args.debugcrypt then Printf.printf "Managed to decrypt with owner password\n";
          pdf
      | _ ->
        if args.debugcrypt then Printf.printf "Couldn't decrypt with owner password %s\n" owner_pw;
        match Pdfcrypt.decrypt_pdf user_pw pdf with
        | Some pdf, permissions ->
            if args.debugcrypt then Printf.printf "Managed to decrypt with user password\n";
            if operation_allowed pdf permissions op
              then pdf
              else soft_error "User password cannot give permission for this operation"
        | _ ->
           soft_error "Failed to decrypt file: wrong password?"

let nobble pdf =
  if not demo then pdf else
    Cpdf.process_pages (Cpdf.nobble_page pdf) pdf (ilist 1 (Pdfpage.endpage pdf))

(* Output Page Count *)
let output_page_count pdf =
  Printf.printf "%i\n" (Pdfpage.endpage pdf)

let setop op () =
  args.op <- Some op

let setout name =
  args.out <- File name

let setchunk c =
  if c > 0
    then args.chunksize <- c
    else error "invalid chunk size"

let setlinearize () =
  args.linearize <- true

let setkeeplinearize () =
  args.keeplinearize <- true

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

let encrypt_to_collect = ref 0

let setmethod s =
  if args.op = None then args.op <- Some Encrypt; (* Could be additional to -split *)
  match s with
  | "40bit" | "128bit" | "AES" | "AES256" | "AES256ISO" -> args.crypt_method <- s
  | _ -> error "Unsupported encryption method"

let setowner s =
  args.owner <- s

let setuser s =
  args.user <- s

let anon_fun s =
  try
    match !encrypt_to_collect with
    | 3 -> setmethod s; decr encrypt_to_collect
    | 2 -> setowner s; decr encrypt_to_collect
    | 1 -> setuser s; decr encrypt_to_collect
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
        args.inputs <- (InFile s, "all", "", "", ref false, None)::args.inputs;
        args.original_filename <- s
      with
        Not_found ->
          match args.inputs with
          | [] -> ()
          | (a, _, d, e, f, g)::t ->
               args.inputs <- (a, fixdashes s, d, e, f, g)::t

(* Unit conversions to points. *)
let mm x = ((x /. 10.) /. 2.54) *. 72.

let cm x = (x /. 2.54) *. 72.

let inch x = x *. 72.

let points_of_papersize p =
  let unit = Pdfpaper.unit p
  and w = Pdfpaper.width p
  and h = Pdfpaper.height p in
    let c  = Pdfunits.convert 0. unit Pdfunits.PdfPoint in
      c w, c h

(*let firstpage pdf =
  List.hd (Pdfpage.pages_of_pagetree pdf)*)

let cropbox pdf page =
  match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
  | Some pdfobject -> Pdf.direct pdf pdfobject
  | None -> page.Pdfpage.mediabox

let width box = let minx, miny, maxx, maxy = Pdf.parse_rectangle box in maxx -. minx
let height box = let minx, miny, maxx, maxy = Pdf.parse_rectangle box in maxy -. miny
let minx box = let minx, miny, maxx, maxy = Pdf.parse_rectangle box in minx
let miny box = let minx, miny, maxx, maxy = Pdf.parse_rectangle box in miny
let maxx box = let minx, miny, maxx, maxy = Pdf.parse_rectangle box in maxx
let maxy box = let minx, miny, maxx, maxy = Pdf.parse_rectangle box in maxy

let find_page_width pdf page = width page.Pdfpage.mediabox
let find_page_height pdf page = height page.Pdfpage.mediabox
let find_page_crop_width pdf page = width (cropbox pdf page)
let find_page_crop_height pdf page = height (cropbox pdf page)
let find_page_minx pdf page = minx page.Pdfpage.mediabox
let find_page_miny pdf page = miny page.Pdfpage.mediabox
let find_page_maxx pdf page = maxx page.Pdfpage.mediabox
let find_page_maxy pdf page = maxy page.Pdfpage.mediabox
let find_page_crop_minx pdf page = minx (cropbox pdf page)
let find_page_crop_miny pdf page = miny (cropbox pdf page)
let find_page_crop_maxx pdf page = maxx (cropbox pdf page)
let find_page_crop_maxy pdf page = maxy (cropbox pdf page)

let find_page_characteristic pdf page = function
  | Pdfgenlex.LexName "PW" -> find_page_width pdf page
  | Pdfgenlex.LexName "PH" -> find_page_height pdf page
  | Pdfgenlex.LexName "CW" -> find_page_crop_width pdf page
  | Pdfgenlex.LexName "CH" -> find_page_crop_height pdf page
  | Pdfgenlex.LexName "PMINX" -> find_page_minx pdf page
  | Pdfgenlex.LexName "PMINY" -> find_page_miny pdf page
  | Pdfgenlex.LexName "PMAXX" -> find_page_maxx pdf page
  | Pdfgenlex.LexName "PMAXY" -> find_page_maxy pdf page
  | Pdfgenlex.LexName "CMINX" -> find_page_crop_minx pdf page
  | Pdfgenlex.LexName "CMINY" -> find_page_crop_miny pdf page
  | Pdfgenlex.LexName "CMAXX" -> find_page_crop_maxx pdf page
  | Pdfgenlex.LexName "CMAXY" -> find_page_crop_maxy pdf page
  | _ -> failwith "find_page_characteristic"

let make_num pdf page unt num =
  let f =
    match num with
    | Pdfgenlex.LexInt i -> float_of_int i
    | Pdfgenlex.LexReal r -> r
    | Pdfgenlex.LexName
      ( "PW" | "PH" | "CW" | "CH" | "PMINX" | "PMINY" | "PMAXX" | "PMAXY"
      | "CMINX" | "CMINY" | "CMAXX" | "CMAXY") as page_characteristic ->
        find_page_characteristic pdf page page_characteristic
    | _ -> failwith "make_num"
  in
    match unt with
    | Pdfgenlex.LexName "pt" -> f
    | Pdfgenlex.LexName "cm" -> cm f
    | Pdfgenlex.LexName "mm" -> mm f
    | Pdfgenlex.LexName "in" -> inch f
    | _ -> failwith "make_num"

let update_last_number pdf page unt op num = function
  [] -> []
| h::t ->
    let final_num = make_num pdf page unt num in
      let h' =
        match op with
          Pdfgenlex.LexName "add" -> h +. final_num
        | Pdfgenlex.LexName "sub" -> h -. final_num
        | Pdfgenlex.LexName "mul" -> h *. final_num
        | Pdfgenlex.LexName "div" -> h /. final_num
        | _ -> failwith "update_last_number"
      in
        h'::t

let rec parse_units_again pdf page numbers papersize more =
  let w, h = points_of_papersize papersize in
    parse_units pdf page (h::w::numbers) more

and parse_units pdf page numbers = function
  | Pdfgenlex.LexName "a10portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a10 more
  | Pdfgenlex.LexName "a9portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a9 more
  | Pdfgenlex.LexName "a8portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a8 more
  | Pdfgenlex.LexName "a7portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a7 more
  | Pdfgenlex.LexName "a6portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a6 more
  | Pdfgenlex.LexName "a5portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a5 more
  | Pdfgenlex.LexName "a4portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a4 more
  | Pdfgenlex.LexName "a3portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a3 more
  | Pdfgenlex.LexName "a2portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a2 more
  | Pdfgenlex.LexName "a1portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a1 more
  | Pdfgenlex.LexName "a0portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a0 more
  | Pdfgenlex.LexName "a10landscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a10) more
  | Pdfgenlex.LexName "a9landscape"::more -> 
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a9) more
  | Pdfgenlex.LexName "a8landscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a8) more
  | Pdfgenlex.LexName "a7landscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a7) more
  | Pdfgenlex.LexName "a6landscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a6) more
  | Pdfgenlex.LexName "a5landscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a5) more
  | Pdfgenlex.LexName "a4landscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a4) more
  | Pdfgenlex.LexName "a3landscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a3) more
  | Pdfgenlex.LexName "a2landscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a2) more
  | Pdfgenlex.LexName "a1landscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a1) more
  | Pdfgenlex.LexName "a0landscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a0) more
  | Pdfgenlex.LexName "uslegalportrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.uslegal more
  | Pdfgenlex.LexName "usletterportrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.usletter more
  | Pdfgenlex.LexName "uslegallandscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.uslegal) more
  | Pdfgenlex.LexName "usletterlandscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.usletter) more
  | Pdfgenlex.LexInt x::Pdfgenlex.LexName "mm"::more ->
      parse_units pdf page ((mm <| float_of_int x)::numbers) more
  | Pdfgenlex.LexReal x::Pdfgenlex.LexName "mm"::more ->
      parse_units pdf page (mm x::numbers) more
  | Pdfgenlex.LexInt x::Pdfgenlex.LexName "cm"::more ->
      parse_units pdf page ((cm <| float_of_int x)::numbers) more
  | Pdfgenlex.LexReal x::Pdfgenlex.LexName "cm"::more ->
      parse_units pdf page (cm x::numbers) more
  | Pdfgenlex.LexInt x::Pdfgenlex.LexName "in"::more ->
      parse_units pdf page ((inch <| float_of_int x)::numbers) more
  | Pdfgenlex.LexReal x::Pdfgenlex.LexName "in"::more ->
      parse_units pdf page (inch x::numbers) more
  | Pdfgenlex.LexInt x::more ->
      parse_units pdf page (float_of_int x::numbers) more
  | Pdfgenlex.LexReal x::more ->
      parse_units pdf page (x::numbers) more
  | Pdfgenlex.LexName "pt"::more ->
      parse_units pdf page numbers more
  | Pdfgenlex.LexName
      ( "PW" | "PH" | "CW" | "CH" | "PMINX" | "PMINY" | "PMAXX" | "PMAXY"
       | "CMINX" | "CMINY" | "CMAXX" | "CMAXY") as page_characteristic::more ->
         parse_units
           pdf
           page
           ((find_page_characteristic pdf page page_characteristic)::numbers)
           more
  | Pdfgenlex.LexName ("add" | "sub" | "mul" | "div") as op::
    ((Pdfgenlex.LexInt _ | Pdfgenlex.LexReal _ |  Pdfgenlex.LexName
      ( "PW" | "PH" | "CW" | "CH" | "PMINX" | "PMINY" | "PMAXX" | "PMAXY"
       | "CMINX" | "CMINY" | "CMAXX" | "CMAXY")) as num)::
    (Pdfgenlex.LexName ("pt" | "mm" | "cm" | "in") as unt)::more ->
      parse_units pdf page (update_last_number pdf page unt op num numbers) more
  | Pdfgenlex.LexName ("add" | "sub" | "mul" | "div") as op::
    ((Pdfgenlex.LexInt _ | Pdfgenlex.LexReal _ |  Pdfgenlex.LexName
      ( "PW" | "PH" | "CW" | "CH" | "PMINX" | "PMINY" | "PMAXX" | "PMAXY"
       | "CMINX" | "CMINY" | "CMAXX" | "CMAXY")) as num)::more ->
      parse_units pdf page (update_last_number pdf page (Pdfgenlex.LexName "pt") op num numbers) more
  | _ -> rev numbers

let rec space_units_inner = function
  | [] -> []
  | 'm'::'m'::t -> ' '::'m'::'m'::' '::space_units_inner t
  | 'c'::'m'::t -> ' '::'c'::'m'::' '::space_units_inner t
  | 'i'::'n'::t -> ' '::'i'::'n'::' '::space_units_inner t
  | 'p'::'t'::t -> ' '::'p'::'t'::' '::space_units_inner t
  | h::t -> h::space_units_inner t

let space_units s =
  implode (space_units_inner (explode s))

let parse_units_string pdf page s =
  let fs = parse_units pdf page [] (Pdfgenlex.lex_string <| space_units s) in
    (*(List.fold_left (fun x y -> x ^ " " ^ y) "" (List.map string_of_float
     * fs));*)
    fs

let parse_rectangle pdf s =
  try
    match parse_units_string pdf emptypage s with
    | [x; y; w; h] -> x, y, w, h
    | _ -> error "Bad rectangle specification"
  with
    _ -> error "Bad rectangle specification"

let parse_rectangles pdf s =
  try
    let pages = Pdfpage.pages_of_pagetree pdf in
    let groups = List.map (fun page -> parse_units_string pdf page s) pages in
      List.map
        (function
         | [x; y; w; h] -> (x, y, w, h)
         | _ -> error "Bad rectangle specification")
        groups
  with
    _ -> error "Bad rectangle specification"

let parse_coordinate pdf s =
  try
    match parse_units_string pdf emptypage s with
    | [dx; dy] -> dx, dy
    | _ -> error "Bad coordinate specification"
  with
    _ -> error "Bad coordinate specification"

let parse_coordinates pdf s =
  try
    let pages = Pdfpage.pages_of_pagetree pdf in
      let groups = List.map (fun page -> parse_units_string pdf page s) pages in
        List.map
          (function
            | [dx; dy] -> (dx, dy)
           | _ -> error "Bad coordinate specification")
          groups
  with
    _ -> error "Bad coordinate specification"

let parse_single_number pdf s =
  try
    match parse_units_string pdf emptypage s with
    | [x] -> x
    | _ -> error "Bad number Argument"
  with
    _ -> error "Bad number argument"

(* Setting operations *)
let setcrop s =
  setop Crop ();
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

let setrotatecontents f =
  setop (RotateContents f) ()

let setauthor s = setop (SetAuthor s) ()
let settitle s = setop (SetTitle s) ()
let setsubject s = setop (SetSubject s) ()
let setkeywords s = setop (SetKeywords s) ()
let setcreate s = setop (SetCreate s) ()
let setmodify s = setop (SetModify s) ()
let setcreator s = setop (SetCreator s) ()
let setproducer s = setop (SetProducer s) ()
let setmetadata s = setop (SetMetadata s) ()
let setversion i = setop (SetVersion i) ()
let setpagelayout s = setop (SetPageLayout s) ()
let setpagemode s = setop (SetPageMode s) ()

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



let setsplitbookmarks i = setop (SplitOnBookmarks i) ()
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
let setthinlines s = setop (ThinLines (parse_single_number empty s)) ()

let setcopyannotations s = setop (CopyAnnotations s) ()

let setshift s =
  setop Shift ();
  args.coord <- s

let setscale s =
  setop Scale ();
  args.coord <- s

let setscaletofit s =
  setop ScaleToFit ();
  args.coord <- s

let setattachfile s =
  match args.op with
  | Some (AttachFile t) ->
      args.op <- Some (AttachFile (s::t))
  | _ ->
      setop (AttachFile [s]) ()

let setfont f =
  args.font <-
    begin match Pdftext.standard_font_of_name ("/" ^ f) with
    | Some x -> StandardFont x
    | None -> OtherFont f
    end;
  args.fontname <- f

let setfontsize f =
  if f > 0.
    then args.fontsize <-f
    else error "Negative font size specified" 

let setaddtext s =
  setop (AddText s) ()

let setcolor s =
  let r, g, b =
    match String.lowercase s with
    | "white" -> 1., 1., 1.
    | "black" -> 0., 0., 0.
    | "red" -> 1., 0., 0.
    | "green" -> 0., 1., 0.
    | "blue" -> 0., 0., 1.
    | _ ->
        let getnum = function
          | Pdfgenlex.LexInt i -> float i
          | Pdfgenlex.LexReal f -> f
          | _ -> error "Bad color"
        in
          match Pdfgenlex.lex_string s with
          | [a;b;c] -> getnum a, getnum b, getnum c
          | _ -> error "Bad color"
  in
    args.color <- r, g, b

let setopacity o =
  args.opacity <- o

let setaddbookmarks s =
  setop (AddBookmarks s) ()

let setstampon f =
  setop (StampOn f) ();
  (* Due to an earlier bad decision (default position), we have this nasty hack *)
  if args.position = Cpdf.TopLeft 100. then args.position <- Cpdf.BottomLeft 0.

let setstampunder f =
  setop (StampUnder f) ();
  if args.position = Cpdf.TopLeft 100. then args.position <- Cpdf.BottomLeft 0.

let setcombinepages f =
  setop (CombinePages f) ()

let setposcenter s =
  let x, y = parse_coordinate empty s in
    args.position <- Cpdf.PosCentre (x, y)

let setposleft s =
  let x, y = parse_coordinate empty s in
    args.position <- Cpdf.PosLeft (x, y)

let setposright s =
  let x, y = parse_coordinate empty s in
    args.position <- Cpdf.PosRight (x, y)

let settop n =
  args.position <- Cpdf.Top (parse_single_number empty n);
  args.justification <- Cpdf.CentreJustify

let settopleft n =
  args.position <- Cpdf.TopLeft (parse_single_number empty n);
  args.justification <- Cpdf.LeftJustify

let settopright n =
  args.position <- Cpdf.TopRight (parse_single_number empty n);
  args.justification <- Cpdf.RightJustify

let setleft n =
  args.position <- Cpdf.Left (parse_single_number empty n);
  args.justification <- Cpdf.LeftJustify

let setbottomleft n =
  args.position <- Cpdf.BottomLeft (parse_single_number empty n);
  args.justification <- Cpdf.LeftJustify

let setbottom n =
  args.position <- Cpdf.Bottom (parse_single_number empty n);
  args.justification <- Cpdf.CentreJustify

let setbottomright n =
  args.position <- Cpdf.BottomRight (parse_single_number empty n);
  args.justification <- Cpdf.RightJustify

let setright n =
  args.position <- Cpdf.Right (parse_single_number empty n);
  args.justification <- Cpdf.RightJustify

let setdiagonal n =
  args.position <- Cpdf.Diagonal;
  args.justification <- Cpdf.CentreJustify

let setreversediagonal n =
  args.position <- Cpdf.ReverseDiagonal;
  args.justification <- Cpdf.CentreJustify

let setcenter n =
  args.position <- Cpdf.Centre;
  args.justification <- Cpdf.CentreJustify

let setscalecenter n =
  args.position <- Cpdf.ReverseDiagonal;
  args.justification <- Cpdf.CentreJustify

let setbatespad n =
  args.batespad <- Some n

let setbates n =
  args.bates <- n

(* Calculate -bates automatically so that n is applied to the first page in the range *)  
let setbatesrange n =
  let first_page =
    let range = Cpdf.parse_pagespec_without_pdf (get_pagespec ()) in
      fold_left min max_int range
  in
    args.bates <- n + 1 - first_page

let setkeepversion () =
  args.keepversion <- true

let setbycolumns () =
  args.bycolumns <- true

let setpagerotation r =
  match r with
  | 90 | 270 -> args.pagerotation <- r
  | _ -> error "Bad Page rotation. Try 90 or 270."

let set_no_edit () =
  args.no_edit <- true

let set_no_print () =
  args.no_print <- true

let set_no_copy () =
  args.no_copy <- true

let set_no_annot () =
  args.no_annot <- true

let set_no_forms () =
  args.no_forms <- true

let set_no_extract () =
  args.no_extract <- true

let set_no_assemble () =
  args.no_assemble <- true

let set_no_hq_print () =
  args.no_hq_print <- true

let set_input s =
  args.original_filename <- s;
  args.inputs <- (InFile s, "all", "", "", ref false, None)::args.inputs

let set_input_dir s =
  let names = sort compare (leafnames_of_dir s) in
    args.inputs <-
      (rev
        (map
          (fun n -> (InFile (s ^ Filename.dir_sep ^ n), "all", "", "", ref false, None)) names))
  @ args.inputs

let setdebug () =
  set Pdfread.read_debug;
  set Pdfwrite.write_debug;
  set Pdfcrypt.crypt_debug;
  set Cpdf.debug;
  args.debug <- true

let setdebugcrypt () =
  args.debugcrypt <- true

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

let setlinespacing f =
  args.linespacing <- f

let setmidline () =
  args.midline <- true

let settopline () =
  args.topline <- true

let setscaletofitscale f =
  args.scale <- f

let setscalecontents f =
  args.op <- Some (ScaleContents f);
  args.position <- Cpdf.Diagonal (* Will be center *)

let setsqueeze () =
  args.squeeze <- true;
  args.create_objstm <- true

let setcreator s =
  args.creator <- Some s

let setproducer s =
  args.producer <- Some s

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

let control_args = ref []

let parse_control_file name =
  (parse_chars []
     (charlist_of_bytes (Pdfio.bytes_of_input_channel (open_in_bin name))))

let setencryptcollect () =
  encrypt_to_collect := 3

let setcopyfont s =
  args.op <- Some (CopyFont s)

let setfontpage i =
  args.copyfontpage <- i

let setcopyfontname s =
  args.copyfontname <- Some s

let setpadevery i =
  if i > 0 then
    args.op <- Some (PadEvery i)
  else
    error "PadEvery: must be > 0"

let setpadmultiple i =
  args.op <- Some (PadMultiple i)

let setfast () =
  args.fast <- true

let setcsp2 f =
  args.op <- Some (CSP2 f)

let setextractimages () =
  args.op <- Some ExtractImages

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
      Printf.eprintf "Warning. -revision ignored. Put it after the filename.\n"

let setoutline () =
  args.outline <- true

let setlinewidth l =
  args.linewidth <- l

let setunderneath () =
  args.underneath <- true

let setimageresolution f =
  args.op <- Some (ImageResolution f)

let setgspath p =
  args.path_to_ghostscript <- p

let setvertical () =
  args.orientation <- Cpdf.Vertical

let setverticaldown () =
  args.orientation <- Cpdf.VerticalDown

let setfrombox s =
  args.op <- Some CopyBox;
  args.frombox <- Some s

let settobox s =
  args.tobox <- Some s

let setmediaboxifmissing () =
  args.mediabox_if_missing <- true

let setrelativetocropbox () =
  args.relative_to_cropbox <- true

let setprerotate () =
  args.prerotate <- true

let setflatkids () =
  Pdfpage.flat_pagetrees := true

let settopage s =
  args.topage <- Some s

let setscalestamptofit () =
  args.scale_stamp_to_fit <- true

let setkeepthisid () =
  match args.inputs with
  | (InFile s, _, _, _, _, _)::_ -> args.keep_this_id <- Some s
  | _ -> ()

let setmakenewid () =
  args.makenewid <- true

let setjustifyleft () =
  args.justification <- Cpdf.LeftJustify

let setjustifyright () =
  args.justification <- Cpdf.RightJustify

let setjustifycenter () =
  args.justification <- Cpdf.CentreJustify

let setremoveduplicatestreams () =
  args.remove_duplicate_streams <- true

let setnopreserveobjstm () =
  args.preserve_objstm <- false

let setcreateobjstm () =
  args.create_objstm <- true

let setstdinuser u =
  match args.inputs with
  |  (StdIn, x, _, o, f, g)::t -> args.inputs <- (StdIn, x, u, o, f, g)::t
  | _ -> error "-stdin-user: must follow -stdin"

let setstdinowner o =
  match args.inputs with
  |  (StdIn, x, u, _, f, g)::t -> args.inputs <- (StdIn, x, u, o, f, g)::t
  | _ -> error "-stdin-owner: must follow -stdin"

let setopenatpage n =
  args.op <- Some (OpenAtPage n)

let setopenatpagefit n =
  args.op <- Some (OpenAtPageFit n)

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

let setcpdflin s = 
  args.cpdflin <- Some s

let setrecrypt () =
  args.recrypt <- true

let setremovedictentry s =
  args.op <- Some (RemoveDictEntry s)

let logto = ref None

let setsqueezelogto s =
  logto := Some s

let setstayonerror () =
  set stay_on_error

let setnoembedfont () =
  args.embedfonts <- false

(* Parse a control file, make an argv, and then make Arg parse it. *)
let rec make_control_argv_and_parse filename =
  control_args := !control_args @ parse_control_file filename

and specs =
   [("-version",
      Arg.Unit (setop Version),
      " Print the cpdf version number");
   ("-o",
       Arg.String setout,
       " Set the output file, if appropriate");
   ("-i",
       Arg.String set_input,
       " Add an input file");
   ("-idir",
       Arg.String set_input_dir,
       " Add a directory of files");
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
   ("-range",
      Arg.String setrange,
      " Explicitly add a range");
   ("-revision",
      Arg.Int setrevision,
      " Set the revision to read for the file just named.");
   ("-change-id",
      Arg.Unit (setop ChangeId),
      " Change the file's /ID tag");
   ("-no-preserve-objstm",
      Arg.Unit setnopreserveobjstm,
      " Don't preserve object streams");
   ("-create-objstm",
      Arg.Unit setcreateobjstm,
      " Create object streams anew");
   ("-keep-version",
      Arg.Unit setkeepversion,
      " Don't change the version number");
   ("-l",
       Arg.Unit setlinearize,
       " Linearize output file");
   ("-keep-l",
       Arg.Unit setkeeplinearize,
       " Linearize if the input file was linearized");
   ("-cpdflin",
       Arg.String setcpdflin,
       " Set location of 'cpdflin'");
   ("-recrypt",
       Arg.Unit setrecrypt,
       " Keep this file's encryption when writing");
   ("-raw",
      Arg.Unit (setencoding Cpdf.Raw),
      " Do not process text");
   ("-stripped",
      Arg.Unit (setencoding Cpdf.Stripped),
      " Process text by simple stripping to ASCII");
   ("-utf8",
      Arg.Unit (setencoding Cpdf.UTF8),
      " Process text by conversion to UTF8 Unicode");
   ("-fast",
      Arg.Unit setfast,
      " Speed over correctness with malformed documents");
   ("-control",
      Arg.String make_control_argv_and_parse,
      " Use a control file. Deprecated. Use -args.");
   ("-merge",
       Arg.Unit (setop Merge),
       " Merge a number of files into one");
   ("-retain-numbering",
       Arg.Unit set_retain_numbering,
       " Don't renumber pages when merging");
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
       Arg.Int setsplitbookmarks,
       " Split a file at bookmarks at a given level");
   ("-scale-page",
      Arg.String setscale,
      " -scale-page \"sx sy\" scales by (sx, sy)");
   ("-scale-to-fit",
      Arg.String setscaletofit,
      " -scale-to-fit \"x y\" scales to page size (x, y)");
   ("-scale-contents",
      Arg.Float setscalecontents,
      "  Scale Contents by the given factor");
   ("-scale-center",
      Arg.Float setscalecenter,
      "  Scale contents around center");
   ("-scale-to-fit-scale",
      Arg.Float setscaletofitscale,
      " -scale-to-fit-scale (1.0 = 100%)");
   ("-shift",
      Arg.String setshift,
      " -shift \"dx dy\" shifts the chosen pages");
   ("-rotate",
       Arg.Int setrotate,
       " Set rotation of pages to 0, 90, 180, 270");
   ("-rotateby",
       Arg.Int setrotateby,
       " Rotate pages by 90, 180 or 270 degrees");
   ("-rotate-contents",
       Arg.Float setrotatecontents,
       " Rotate contents of pages");
   ("-upright",
       Arg.Unit (setop Upright),
       " Make pages upright");
   ("-hflip",
       Arg.Unit (setop HFlip),
       " Flip pages horizontally");
   ("-vflip",
       Arg.Unit (setop VFlip),
       " Flip pages vertically");
   ("-crop",
       Arg.String setcrop,
       " Crop specified pages");
   ("-remove-crop",
       Arg.Unit (setop RemoveCrop),
       " Remove cropping on specified pages");
   ("-copy-cropbox-to-mediabox",
       Arg.Unit (setop CopyCropBoxToMediaBox),
       ""); (* Undocumented now, since /frombox, /tobox now used *)
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
   ("-no-edit", Arg.Unit set_no_edit, " No edits");
   ("-no-print", Arg.Unit set_no_print, " No printing");
   ("-no-copy", Arg.Unit set_no_copy, " No copying");
   ("-no-annot", Arg.Unit set_no_annot, " No annotations");
   ("-no-forms", Arg.Unit set_no_forms, " No forms");
   ("-no-extract", Arg.Unit set_no_extract, " No extracting");
   ("-no-assemble", Arg.Unit set_no_assemble, " No assembling");
   ("-no-hq-print", Arg.Unit set_no_hq_print, " No high quality printing");
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
       Arg.Unit setremoveduplicatestreams,
       "");
   ("-list-bookmarks",
      Arg.Unit (setop ListBookmarks),
      " List Bookmarks");
   ("-remove-bookmarks",
      Arg.Unit (setop RemoveBookmarks),
      " Remove bookmarks from a file");
   ("-add-bookmarks",
      Arg.String setaddbookmarks,
      " Add bookmarks from the given file");
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
      Arg.Unit setscalestamptofit,
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
      "");
   ("-bates",
      Arg.Int setbates,
      " Set the base bates number");
   ("-bates-at-range",
      Arg.Int setbatesrange,
      " Set the base bates number at first page in range");
   ("-bates-pad-to",
      Arg.Int setbatespad,
      " Pad the bates number with leading zeroes to width");
   ("-font",
      Arg.String setfont,
      " Set the font");
   ("-font-size",
      Arg.Float setfontsize,
      " Set the font size");
   ("-no-embed-font",
      Arg.Unit setnoembedfont,
      "Do not embed fonts");
   ("-color",
      Arg.String setcolor,
      " Set the color");
   ("-opacity",
      Arg.Float setopacity,
      " Set the text opacity");
   ("-outline",
      Arg.Unit setoutline,
      " Use outline mode for text");
   ("-linewidth",
      Arg.Float setlinewidth,
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
      Arg.Unit setjustifyleft,
      " Justify multiline text left");
   ("-justify-right",
      Arg.Unit setjustifyright,
      " Justify multiline text right");
   ("-justify-center",
      Arg.Unit setjustifycenter,
      " Justify multiline text centre");
   ("-underneath",
      Arg.Unit setunderneath,
      " Text stamp is underneath content");
   ("-line-spacing",
      Arg.Float setlinespacing,
      " Line spacing (1 is normal)");
   ("-midline",
      Arg.Unit setmidline,
      " Adjust text to midline rather than baseline");
   ("-topline",
      Arg.Unit settopline,
      " Adjust text to topline rather than baseline");
   ("-relative-to-cropbox",
      Arg.Unit setrelativetocropbox,
      " Add text relative to Crop Box not Media Box");
   ("-prerotate",
      Arg.Unit setprerotate,
      " Calls -upright on pages before adding text");
   ("-twoup",
      Arg.Unit (setop TwoUp),
      " Put 2 pages onto one");
   ("-twoup-stack",
      Arg.Unit (setop TwoUpStack),
      " Stack 2 pages onto one twice the size");
   ("-pad-before",
      Arg.Unit (setop PadBefore),
      " Add a blank page before the given pages");
   ("-pad-after",
      Arg.Unit (setop PadAfter),
      " Add a blank page after the given pages");
   ("-pad-every",
      Arg.Int setpadevery,
      " Add a blank page after every n pages");
   ("-pad-multiple",
      Arg.Int setpadmultiple,
      " Pad the document to a multiple of n pages");
   ("-list-annotations",
      Arg.Unit (setop ListAnnotations),
      " List annotations");
   ("-copy-annotations",
      Arg.String setcopyannotations,
      " Copy annotations from given file");
   ("-remove-annotations",
      Arg.Unit (setop RemoveAnnotations),
      " Remove annotations");
   ("-list-fonts",
       Arg.Unit (setop Fonts),
       " Output font list");
   ("-info",
       Arg.Unit (setop Info),
       " Output file information");
   ("-page-info",
       Arg.Unit (setop PageInfo),
       " Output file information");
   ("-set-author",
       Arg.String setauthor,
       " Set Author");
   ("-set-title",
      Arg.String settitle,
      " Set Title");
   ("-set-subject",
      Arg.String setsubject,
      " Set Subject");
   ("-set-keywords",
      Arg.String setkeywords,
      " Set Keywords");
   ("-set-create",
      Arg.String setcreate,
      " Set Creation date");
   ("-set-modify",
      Arg.String setmodify,
      " Set Modification date");
   ("-set-creator",
      Arg.String setcreator,
      " Set Creator");
   ("-set-producer",
      Arg.String setproducer,
      " Set Producer");
   ("-set-trapped",
      Arg.Unit (setop SetTrapped),
      " Mark as trapped");
   ("-set-untrapped",
      Arg.Unit (setop SetUntrapped),
      " Mark as not trapped");
   ("-set-page-layout",
      Arg.String setpagelayout,
      " Set page layout upon document opening");
   ("-set-page-mode",
      Arg.String setpagemode,
      " Set page mode upon document opening");
   ("-open-at-page",
      Arg.Int setopenatpage,
      " Set initial page");
   ("-open-at-page-fit",
      Arg.Int setopenatpagefit,
      " Set inital page, scaling to fit");
   ("-set-metadata",
      Arg.String setmetadata,
      " Set metadata to the contents of a file");
   ("-print-metadata",
       Arg.Unit (setop Metadata),
       " Output metadata information");
   ("-remove-metadata",
       Arg.Unit (setop RemoveMetadata),
       " Remove document metadata");
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
   ("-pages",
      Arg.Unit (setop CountPages),
      " Count pages");
   ("-revisions",
      Arg.Unit (setop Revisions),
      " Count number of revisions");
   ("-list-attached-files",
      Arg.Unit (setop ListAttachedFiles),
      " List attached files");
   ("-dump-attachments",
      Arg.Unit (setop DumpAttachedFiles),
      "");
   ("-attach-file",
      Arg.String setattachfile,
      " Attach a file");
   ("-to-page",
      Arg.String settopage,
      " Attach file to given page instead of document");
   ("-remove-files",
      Arg.Unit (setop RemoveAttachedFiles),
      " Remove embedded attached document-level files");
   ("-image-resolution",
      Arg.Float setimageresolution,
      " List images under a given dpi");
   ("-copy-font",
      Arg.String setcopyfont,
      " Copy a named font");
   ("-copy-font-page",
      Arg.Int setfontpage,
      " Set the page a copied font is drawn from");
   ("-remove-fonts",
      Arg.Unit (setop RemoveFonts),
      " Remove embedded fonts");
   ("-copy-font-name",
      Arg.String setcopyfontname,
      " Set the name of the font to copy");
   ("-missing-fonts",
      Arg.Unit (setop MissingFonts),
      " List missing fonts");
   ("-remove-id",
      Arg.Unit (setop RemoveId),
      " Remove the file's /ID tag");
   ("-draft",
      Arg.Unit (setop Draft),
      " Remove images from the file");
   ("-boxes",
      Arg.Unit setboxes,
      " Add crossed boxes to -draft option");
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
   ("-clean",
       Arg.Unit (setop Clean),
       " Garbage-collect a file");
   ("-set-version",
      Arg.Int setversion,
      " Set PDF version number");
   ("-copy-id-from",
      Arg.String setcopyid,
      " Copy one file's ID tag to another");
   ("-print-page-labels",
      Arg.Unit (setop PrintPageLabels),
      " Print page labels");
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
   ("-remove-dict-entry",
    Arg.String setremovedictentry,
    " Remove an entry from all dictionaries");
   ("-producer",
    Arg.String setproducer,
    " Change the /Producer entry in the /Info dictionary");
   ("-creator",
    Arg.String setcreator,
    " Change the /Creator entry in the /Info dictionary");
   ("-list-spot-colours",
    Arg.Unit (setop ListSpotColours),
    " List spot colours");
   (*These items are undocumented *)
   ("-stay-on-error", Arg.Unit setstayonerror, "");
   ("-extract-fontfile", Arg.Unit (setop ExtractFontFile), "");
   ("-extract-images", Arg.Unit setextractimages, "");
   ("-csp1", Arg.Unit (setop CSP1), "");
   ("-csp2", Arg.Float setcsp2, "");
   ("-csp3", Arg.Unit (setop CSP3), "");
   ("-text-vertical", Arg.Unit setvertical, "");
   ("-text-vertical-down", Arg.Unit setverticaldown, "");
   ("-flat-kids", Arg.Unit setflatkids, "");
   ("-gs", Arg.String setgspath, "");
   ("-debug", Arg.Unit setdebug, "");
   ("-debug-crypt", Arg.Unit setdebugcrypt, "");
   ("-fix-prince", Arg.Unit (setop RemoveUnusedResources), "");
   ("-squeeze", Arg.Unit setsqueeze, "");
   ("-squeeze-log-to", Arg.String setsqueezelogto, "");
   ("-extract-text", Arg.Unit (setop ExtractText), "")]

and usage_msg =
"Syntax: cpdf <op> <op-specific arguments> [-o <output file>] <input files>\n\n\
This is a copyrighted, commercial program, and may NOT be freely copied.\n\n\
Version " ^ string_of_int major_version ^ "." ^ string_of_int minor_version ^ " " ^ version_date ^ "\n\n\
To buy, visit http://www.coherentpdf.com/\n\n\
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

let pdf_of_stdin ?revision user_pw owner_pw =
  let user_pw = Some user_pw
  and owner_pw = if owner_pw = "" then None else Some owner_pw in
    let o, bytes = Pdfio.input_output_of_bytes 16384 in
      try
        while true do o.Pdfio.output_char (input_char stdin) done;
        Pdf.empty ()
      with
        End_of_file ->
          let i = Pdfio.input_of_bytes (Pdfio.extract_bytes_from_input_output o bytes) in
            pdfread_pdf_of_input ?revision user_pw owner_pw i

let filenames = null_hash ()

(* This now memoizes on the name of the file to make sure we only load each
file once *)
let get_pdf_from_input_kind ((_, _, u, o, _, revision) as input) op = function
  | AlreadyInMemory pdf -> pdf
  | InFile s ->
      if args.squeeze then
        begin
          let size = filesize s in
            initial_file_size := size;
            if !logto = None then Printf.printf "Initial file size is %i bytes\n" size
        end;
      begin try Hashtbl.find filenames s with
        Not_found ->
          let pdf = pdfread_pdf_of_file ?revision (optstring u) (optstring o) s in
            args.was_encrypted <- Pdfcrypt.is_encrypted pdf;
            let pdf = decrypt_if_necessary input op pdf in
              Hashtbl.add filenames s pdf; pdf
      end
  | StdIn ->
      decrypt_if_necessary input op (pdf_of_stdin ?revision u o)

let get_single_pdf op read_lazy =
  match args.inputs with
  | (InFile inname, _, u, o, _, revision) as input::_ ->
      if args.squeeze then
        Printf.printf "Initial file size is %i bytes\n" (filesize inname);
      let pdf =
        if read_lazy then
          pdfread_pdf_of_channel_lazy ?revision (optstring u) (optstring o) (open_in_bin inname)
        else
          pdfread_pdf_of_file ?revision (optstring u) (optstring o) inname
      in
        args.was_encrypted <- Pdfcrypt.is_encrypted pdf;
        decrypt_if_necessary input op pdf
  | (StdIn, _, u, o, _, revision) as input::_ ->
      decrypt_if_necessary input op (pdf_of_stdin ?revision u o)
  | (AlreadyInMemory pdf, _, _, _, _, _)::_ -> pdf
  | _ ->
      raise (Arg.Bad "cpdf: No input specified.\n")

let get_single_pdf_nodecrypt read_lazy =
  match args.inputs with
  | (InFile inname, _, u, o, _, revision)::_ ->
      if args.squeeze then
        Printf.printf "Initial file size is %i bytes\n" (filesize inname);
        if read_lazy then
          pdfread_pdf_of_channel_lazy ?revision (optstring u) (optstring o) (open_in_bin inname)
        else
          pdfread_pdf_of_file ?revision (optstring u) (optstring o) inname
  | (StdIn, _, u, o, _, revision)::_ -> pdf_of_stdin ?revision u o
  | (AlreadyInMemory pdf, _, _, _, _, _)::_ -> pdf
  | _ ->
      raise (Arg.Bad "cpdf: No input specified.\n")

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
  if args.encoding <> Cpdf.Raw
    then Pdftext.pdfdocstring_of_utf8 s
    else unescape_octals s

let set_producer s pdf =
  ignore (Cpdf.set_pdf_info ("/Producer", Pdf.String (process s), 0) pdf)

let set_creator s pdf =
  ignore (Cpdf.set_pdf_info ("/Creator", Pdf.String (process s), 0) pdf)

let really_write_pdf ?(encryption = None) ?(is_decompress=false) mk_id pdf outname =
  if args.producer <> None then set_producer (unopt args.producer) pdf;
  if noncomp then
    set_producer "cpdf non-commercial use only. To buy: http://coherentpdf.com/"
    pdf;
  if args.creator <> None then set_creator (unopt args.creator) pdf;
  if args.debugcrypt then Printf.printf "really_write_pdf\n%!";
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
                  false None mk_id pdf outname'
        end
      else
        begin
          if not args.was_encrypted || args.was_decrypted_with_owner then
            begin
              if args.debugcrypt then
                Printf.printf "Pdf to file in really_write_pdf\n";
              Pdfwrite.pdf_to_file_options
                ~preserve_objstm:args.preserve_objstm
                ~generate_objstm:args.create_objstm
                ~compress_objstm:(not is_decompress)
                false encryption mk_id pdf outname'
            end
          else
            soft_error
              "You must supply -recrypt here, or provide the owner password."
        end
    end;
    begin
      if will_linearize then
        let cpdflin = Cpdf.find_cpdflin args.cpdflin in
          match args.inputs with
            [] -> raise (Pdf.PDFError "no input in recryption")
          | (_, _, user_pw, owner_pw, _, _)::_ ->
              let best_password =
                if owner_pw <> "" then owner_pw else user_pw
              in
                let code =
                  Cpdf.call_cpdflin cpdflin outname' outname best_password
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
        if !logto = None then
          Printf.printf
            "Final file size is %i bytes, %.2f%% of original.\n"
            s
            ((float s /. float !initial_file_size) *. 100.)

let write_pdf ?(encryption = None) ?(is_decompress=false) mk_id pdf =
  if args.debugcrypt then Printf.printf "write_pdf\n";
  if args.create_objstm && not args.keepversion
    then pdf.Pdf.minor <- max pdf.Pdf.minor 5;
  let mk_id = args.makenewid || mk_id in
    match args.out with
    | NoOutputSpecified ->
        output_pdfs =| pdf
    | File outname ->
        begin match encryption with
          None ->
            let pdf = nobble pdf in
              if not is_decompress then
                begin
                  ignore (Cpdf.recompress_pdf pdf);
                  if args.squeeze then Cpdf.squeeze ?logto:!logto pdf;
                end;
              Pdf.remove_unreferenced pdf;
              really_write_pdf ~is_decompress mk_id pdf outname
        | Some _ ->
            really_write_pdf ~encryption ~is_decompress mk_id pdf outname
        end
    | Stdout ->
        let temp = Filename.temp_file "cpdflin" ".pdf" in
          begin match encryption with
            None -> 
              let pdf = nobble pdf in
                if not is_decompress then
                  begin
                    ignore (Cpdf.recompress_pdf pdf);
                    if args.squeeze then Cpdf.squeeze ?logto:!logto pdf;
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
              End_of_file -> flush stdout (*r For Windows *)

(* Returns empty string on failure. Should only be used in conjunction with
split at bookmarks code, so should never fail, by definiton. *)
let remove_unsafe_characters s =
  let chars =
    lose
      (function x ->
         match x with
         '/' | '?' | '<' | '>' | '\\' | ':' | '*' | '|' | '\"' | '^' | '+' | '=' -> true
         | x when int_of_char x < 32 || int_of_char x > 126 -> true
         | _ -> false)
      (explode s)
  in
    match chars with
    | '.'::more -> implode more
    | chars -> implode chars

let get_bookmark_name pdf marks splitlevel n _ =
  match keep (function m -> n = Pdfpage.pagenumber_of_target pdf m.Pdfmarks.target && m.Pdfmarks.level <= splitlevel) marks with
  | {Pdfmarks.text = title}::_ -> remove_unsafe_characters title
  | _ -> ""

(* @F means filename without extension *)
(* @N means sequence number with no padding *)
(* @S means start page of this section *)
(* @E means end page of this section *)
(* @B means bookmark name at start page *)
let process_others marks pdf splitlevel filename sequence startpage endpage s =
  let rec procss prev = function
    | [] -> rev prev
    | '@'::'F'::t -> procss (rev (explode filename) @ prev) t
    | '@'::'N'::t -> procss (rev (explode (string_of_int sequence)) @ prev) t
    | '@'::'S'::t -> procss (rev (explode (string_of_int startpage)) @ prev) t
    | '@'::'E'::t -> procss (rev (explode (string_of_int endpage)) @ prev) t
    | '@'::'B'::t -> procss (rev (explode (get_bookmark_name pdf marks splitlevel startpage pdf)) @ prev) t
    | h::t -> procss (h::prev) t
  in
     implode (procss [] (explode s))

let name_of_spec marks (pdf : Pdf.t) splitlevel spec n filename startpage endpage =
  let fill l n =
    let chars = explode (string_of_int n) in
      if length chars > l
        then implode (drop chars (length chars - l))
        else implode ((many '0' (l - length chars)) @ chars)
  in
    let chars = explode spec in
      let before, including = cleavewhile (neq '%') chars in
        let percents, after = cleavewhile (eq '%') including in
          if percents = []
            then
              process_others marks pdf splitlevel filename n startpage endpage spec
            else
              process_others marks pdf splitlevel filename n startpage endpage
              (implode before ^ fill (length percents) n ^ implode after)

(* Find the stem of a filename *)
let stem s =
  implode
    (rev (tail_no_fail
      (dropwhile
        (neq '.') (rev (explode (Filename.basename s))))))

let fast_write_split_pdfs
  enc splitlevel original_filename sq nobble spec main_pdf pagenums pdf_pages
=
  let marks = Pdfmarks.read_bookmarks main_pdf in
    iter2
      (fun number pagenums ->
         let pdf = nobble (Pdfpage.pdf_of_pages main_pdf pagenums) in
           let startpage, endpage = extremes pagenums in
             let name =
               name_of_spec
                 marks main_pdf splitlevel spec number
                 (stem original_filename) startpage endpage
             in
               Pdf.remove_unreferenced pdf;
               if sq then Cpdf.squeeze ?logto:!logto pdf;
               really_write_pdf ~encryption:enc (not (enc = None)) pdf name)
      (indx pagenums)
      pagenums

(* Return list, in order, a *set* of page numbers of bookmarks at a given level *)
let bookmark_pages level pdf =
  setify_preserving_order
    (option_map
      (function
         l when l.Pdfmarks.level = level ->
           Some (Pdfpage.pagenumber_of_target pdf l.Pdfmarks.target)
       | _ -> None)
      (Pdfmarks.read_bookmarks pdf))

let split_at_bookmarks
  enc original_filename ~squeeze nobble level spec pdf
=
  let pdf_pages = Pdfpage.pages_of_pagetree pdf in
    let points = bookmark_pages level pdf in
      let points =
        lose (fun x -> x <= 0 || x > Pdfpage.endpage pdf) (map pred points)
      in
        let pts = splitat points (indx pdf_pages) in
          fast_write_split_pdfs
            enc level original_filename squeeze nobble spec pdf pts pdf_pages

let split_pdf
  enc original_filename
  chunksize linearize ~cpdflin ~preserve_objstm ~create_objstm ~squeeze
  nobble spec pdf
=
  let pdf_pages = Pdfpage.pages_of_pagetree pdf in
    fast_write_split_pdfs
      enc 0 original_filename squeeze nobble spec pdf
      (splitinto chunksize (indx pdf_pages)) pdf_pages


(* Copy a font from [frompdf] with name [fontname] on page [fontpage] to [pdf] on all pages in [range] *)
let copy_font frompdf fontname fontpage range pdf =
  match Pdf.renumber_pdfs [frompdf; pdf] with
  | [] | [_] | _::_::_::_ -> assert false
  | [frompdf; pdf] ->
      (* 1. Get fontpage *)
      let frompdf_pages = Pdfpage.pages_of_pagetree frompdf in
        let frompdf_page =
           try select fontpage frompdf_pages with
             Not_found -> failwith "copy_font: Page not found in input pdf"
        in
      (* 2. Extract font *)
      let fonts =
        match Pdf.lookup_direct frompdf "/Font" frompdf_page.Pdfpage.resources with
        | Some f -> f
        | None -> failwith "copy_font: font not found"
      in
        let fromfont =
          match Pdf.lookup_direct frompdf fontname fonts with
          | Some f -> f
          | None -> failwith "copy_font: font not found"
        in
          let basefontname =
            match Pdf.lookup_direct frompdf "/BaseFont" fromfont with
            | Some (Pdf.Name n) -> n
            | _ -> "/CopyFontAddedNoName"
          in
      (* 3. Get all objects forming font (except main /Font one) *)
      let objnumbers = Pdf.objects_referenced [] [] frompdf fromfont in
      (* 4. Copy them to from frompdf to pdf. *)
      iter (function objnum -> Pdf.addobj_given_num pdf (objnum, Pdf.lookup_obj frompdf objnum)) objnumbers;
      (* 5. Get pages from pdf *)
      let pdf_pages = Pdfpage.pages_of_pagetree pdf in
      (* 6. Add the font to pages in range *)
      let pages' =
        map
        (function (page, pagenum) ->
           if mem pagenum range then
             let font =
               match Pdf.lookup_direct pdf "/Font" page.Pdfpage.resources with
               | Some f -> f
               | None -> Pdf.Dictionary []
             in
               let font' =
                 match font with
                 | (Pdf.Dictionary _) as d ->
                     Pdf.add_dict_entry d basefontname fromfont
                 | _ -> failwith "copy_font: error"
               in
                 let resources' =
                   Pdf.add_dict_entry page.Pdfpage.resources "/Font" font'
                 in
                   {page with
                      Pdfpage.resources = resources'}
             else page)
        (combine pdf_pages (indx pdf_pages));
      in
      (* 7. Put the pages back into the pdf, and return *)
      let pdf, root = Pdfpage.add_pagetree pages' pdf in
        Pdfpage.add_root root [] pdf

(* Extract Images. *)
let pnm_to_channel_24 channel w h s =
  let white () = output_char channel ' ' 
  and newline () = output_char channel '\n'
  and output_string = Pervasives.output_string channel in
    output_string "P6";
    white ();
    output_string (string_of_int w);
    white ();
    output_string (string_of_int h);
    white ();
    output_string "255";
    newline ();
    let pos = ref 0 in
      for y = 1 to h do
        for x = 1 to w * 3 do
          output_byte channel (bget s !pos);
          incr pos
        done
      done

let null_device =
  match Sys.os_type with
  | "Win32" -> "nul"
  | _ -> "/dev/null"

(* cpdf -extract-images in.pdf 2-5 -o img%%% (FIXME: Add output spec. Document png stuff.) *)
let write_stream name stream =
  let fh = open_out_bin name in
    for x = 0 to bytes_size stream - 1 do
      output_byte fh (bget stream x)
    done;
    close_out fh

let write_image pdf resources name image =
  match Pdfimage.get_image_24bpp pdf resources image with
  | Pdfimage.JPEG (stream, _) -> write_stream (name ^ ".jpg") stream
  | Pdfimage.JPEG2000 (stream, _) -> write_stream (name ^ ".jpx") stream
  | Pdfimage.JBIG2 (stream, _) -> write_stream (name ^ ".jbig2") stream
  | Pdfimage.Raw (w, h, Pdfimage.BPP24, stream) ->
      let fh = open_out_bin (name ^ ".pnm") in
        pnm_to_channel_24 fh w h stream;
        close_out fh;
      (* If pnmtopng is present, convert the pnm to a PNG. *)
      begin match
        Sys.command ("pnmtopng -gamma 0.45 -quiet " ^ "\"" ^ name ^ ".pnm\"" ^ "> \"" ^ name ^ ".png\" 2>" ^ null_device)
      with
      | 0 -> Sys.remove (name ^ ".pnm")
      | _ -> ()
      end
  | _ -> ()

(* FIXME: Doesn't cope with images within form xobjects *)
let extract_images pdf range stem =
  let pdf_pages = Pdfpage.pages_of_pagetree pdf in
    let pages =
      option_map
        (function (i, pdf_pages) -> if mem i range then Some pdf_pages else None)
        (combine (indx pdf_pages) pdf_pages)
    in
      iter
        (function page ->
           let xobjects =
             match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
             | Some (Pdf.Dictionary elts) -> map snd elts
             | _ -> []
           in
             let images =
               keep (fun o -> Pdf.lookup_direct pdf "/Subtype" o = Some (Pdf.Name "/Image")) xobjects
             in
               if images <> [] then
               (let names =
                 map
                   (function n -> let r = Cpdf.name_of_spec [] (*FIXME *) pdf 0 stem n "" 0 0 in (*i flprint r; flprint "\n"; i*) r)
                   (ilist 1 (length images))
               in
                 iter2 (write_image pdf page.Pdfpage.resources) names images))
        pages

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


(* If a cropbox exists, make it the mediabox. If not, change nothing. *)
let copy_cropbox_to_mediabox pdf range =
  Cpdf.process_pages
    (fun _ page ->
       match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
       | Some pdfobject -> {page with Pdfpage.mediabox = Pdf.direct pdf pdfobject}
       | None -> page)
    pdf
    range

(* Image resolution *)
type xobj =
  | Image of int * int (* width, height *)
  | Form of Pdftransform.transform_matrix * Pdf.pdfobject * Pdf.pdfobject (* Will add actual data later. *)

(* Given a page and a list of (pagenum, name, thing) *)
let rec image_resolution_page pdf page pagenum dpi (images : (int * string * xobj) list) =
  try
    let pageops = Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content
    and transform = ref [ref Pdftransform.i_matrix] in
      iter
        (function
         | Pdfops.Op_cm matrix ->
             begin match !transform with
             | [] -> raise (Failure "no transform")
             | _ -> (hd !transform) := Pdftransform.matrix_compose !(hd !transform) matrix
             end
         | Pdfops.Op_Do xobject ->
             let trans (x, y) =
               match !transform with
               | [] -> raise (Failure "no transform")
               | _ -> Pdftransform.transform_matrix !(hd !transform) (x, y)
             in
               let o = trans (0., 0.)
               and x = trans (1., 0.)
               and y = trans (0., 1.)
               in
                 (*i Printf.printf "o = %f, %f, x = %f, %f, y = %f, %f\n" (fst o) (snd o) (fst x) (snd x) (fst y) (snd y); i*)
                 let rec lookup_image k = function
                   | [] -> assert false
                   | (_, a, _) as h::_ when a = k -> h
                   | _::t -> lookup_image k t 
                 in
                   begin match lookup_image xobject images with
                   | (pagenum, name, Form (xobj_matrix, content, resources)) ->
                        let content =
                          (* Add in matrix etc. *)
                          let total_matrix = Pdftransform.matrix_compose xobj_matrix !(hd !transform) in
                            let ops =
                              Pdfops.Op_cm total_matrix::
                              Pdfops.parse_operators pdf resources [content]
                            in
                              Pdfops.stream_of_ops ops
                        in
                          let page =
                            {Pdfpage.content = [content];
                             Pdfpage.mediabox = Pdf.Null;
                             Pdfpage.resources = resources;
                             Pdfpage.rotate = Pdfpage.Rotate0;
                             Pdfpage.rest = Pdf.Dictionary []}
                          in
                            let newpdf = Pdfpage.change_pages true pdf [page] in
                              image_resolution newpdf [pagenum] dpi
                   | (pagenum, name, Image (w, h)) ->
                       let lx = Pdfunits.convert 0. Pdfunits.PdfPoint Pdfunits.Inch (distance_between o x)
                       and ly = Pdfunits.convert 0. Pdfunits.PdfPoint Pdfunits.Inch (distance_between o y) in
                         let wdpi = float w /. lx
                         and hdpi = float h /. ly in
                         if wdpi < dpi || hdpi < dpi then
                           Printf.printf "%i, %s, %i, %i, %f, %f\n" pagenum xobject w h wdpi hdpi
                         (*i else
                           Printf.printf "S %i, %s, %i, %i, %f, %f\n" pagenum xobject (int_of_float w) (int_of_float h) wdpi hdpi i*)
                   end
         | Pdfops.Op_q ->
             begin match !transform with
             | [] -> raise (Failure "Unbalanced q/Q ops")
             | h::t ->
                 let h' = ref Pdftransform.i_matrix in
                   h' := !h;
                   transform := h'::h::t
             end
         | Pdfops.Op_Q ->
             begin match !transform with
             | [] -> raise (Failure "Unbalanced q/Q ops")
             | _ -> transform := tl !transform
             end
         | _ -> ())
        pageops
    with
      e -> Printf.printf "Error %s\n" (Printexc.to_string e); flprint "\n"

and image_resolution pdf range dpi =
  let images = ref [] in
    Cpdf.iter_pages
      (fun pagenum page ->
         (* 1. Get all image names and their native resolutions from resources as string * int * int *)
         match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
          | Some (Pdf.Dictionary xobjects) ->
              iter
                (function (name, xobject) ->
                   match Pdf.lookup_direct pdf "/Subtype" xobject with
                   | Some (Pdf.Name "/Image") ->
                       let width =
                         match Pdf.lookup_direct pdf "/Width" xobject with
                         | Some x -> Pdf.getnum x
                         | None -> 1.
                       and height =
                         match Pdf.lookup_direct pdf "/Height" xobject with
                         | Some x -> Pdf.getnum x
                         | None -> 1.
                       in
                         images := (pagenum, name, Image (int_of_float width, int_of_float height))::!images
                   | Some (Pdf.Name "/Form") ->
                       let resources =
                         match Pdf.lookup_direct pdf "/Resources" xobject with
                         | None -> page.Pdfpage.resources (* Inherit from page or form above. *)
                         | Some r -> r
                       and contents =
                         xobject 
                       and matrix =
                         match Pdf.lookup_direct pdf "/Matrix" xobject with
                         | Some (Pdf.Array [a; b; c; d; e; f]) ->
                             {Pdftransform.a = Pdf.getnum a; Pdftransform.b = Pdf.getnum b; Pdftransform.c = Pdf.getnum c;
                              Pdftransform.d = Pdf.getnum d; Pdftransform.e = Pdf.getnum e; Pdftransform.f = Pdf.getnum f}
                         | _ -> Pdftransform.i_matrix
                       in
                         images := (pagenum, name, Form (matrix, contents, resources))::!images
                   | _ -> ()
                )
                xobjects
          | _ -> ())
      pdf
      range;
      (* Now, split into differing pages, and call [image_resolution_page] on each one *)
      let pagesplits =
        map
          (function (a, _, _)::_ as ls -> (a, ls) | _ -> assert false)
          (collate (fun (a, _, _) (b, _, _) -> compare a b) (rev !images))
      and pages =
        Pdfpage.pages_of_pagetree pdf
      in
        iter
          (function (pagenum, images) ->
             let page = select pagenum pages in
               image_resolution_page pdf page pagenum dpi images)
          pagesplits

(* Missing Fonts *)

let is_missing pdf dict =
  match Pdf.lookup_direct pdf "/FontDescriptor" dict with
  | None -> true
  | Some d ->
      match Pdf.lookup_direct pdf "/FontFile" d with
      | Some _ -> false
      | None ->
          match Pdf.lookup_direct pdf "/FontFile2" d with
          | Some _ -> false
          | None ->
              match Pdf.lookup_direct pdf "/FontFile3" d with
              | Some _ -> false
              | None -> true

let missing_font pdf page (name, dict) =
  if is_missing pdf dict then
    let subtype =
      match Pdf.lookup_direct pdf "/Subtype" dict with
      | Some (Pdf.Name n) -> n
      | _ -> ""
    and basefont =
      match Pdf.lookup_direct pdf "/BaseFont" dict with
      | Some (Pdf.Name n) -> n
      | _ -> ""
    and encoding =
     match Pdf.lookup_direct pdf "/Encoding" dict with
      | Some (Pdf.Name n) -> n
      | _ -> ""
    in 
      if Pdftext.standard_font_of_name basefont <> None then () else
      Printf.printf "%i, %s, %s, %s, %s\n" page name subtype basefont encoding

let missing_fonts pdf range =
  Cpdf.iter_pages
    (fun num page ->
       match Pdf.lookup_direct pdf "/Font" page.Pdfpage.resources with
       | Some (Pdf.Dictionary fontdict) ->
           (* Extract descendant fonts *)
           let name_dict_pairs =
             flatten
               (map
                  (fun (name, dict) ->
                     match Pdf.lookup_direct pdf "/DescendantFonts" dict with
                     | Some (Pdf.Array desc_fonts) -> map (fun d -> name, d) desc_fonts
                     | _ -> [(name, dict)])
                  fontdict)
           in
             iter (missing_font pdf num) name_dict_pairs
       | _ -> ())
    pdf
    range

let lines_of_channel c =
  let ls = ref [] in
    try
      while true do ls =| input_line c done; []
    with
      End_of_file -> rev !ls

(* parse the results of the standard error part of gs -sDEVICE=bbox *)
let parse_whiteboxes filename =
  try
    let fh = open_in filename in
      let lines = lines_of_channel fh in
        let lines =
          keep
            (function l ->
               match explode l with
               | '%'::'%'::_ -> true | _ -> false)
            lines
        in
        let hires_lines = drop_odds lines in
          let result =
            map
              (fun line ->
                 let line = implode (drop (explode line) 20) in
                   match Pdfgenlex.lex_string line with
                   | [a; b; c; d] ->
                        let getfloat = function
                          | Pdfgenlex.LexInt i -> float i
                          | Pdfgenlex.LexReal f -> f
                          | _ -> raise (Failure "parse_whiteboxes")
                        in
                          getfloat a, getfloat b, getfloat c, getfloat d
                   | x -> raise (Failure ("bad lex on line " ^ line ^ "made tokens " ^ Pdfgenlex.string_of_tokens x)))
              hires_lines
          in
            close_in fh;
            result
  with
    e ->
      Printf.eprintf "%s\n" ("parse_whiteboxes " ^ Printexc.to_string e);
      raise (Failure "")

(* Make start, end pairs from a sortedrange *)
let rec startends_of_range_inner pairs ls =
  match pairs, ls with
  | _, [] -> rev pairs
  | [], (h::t) ->
      startends_of_range_inner [(h, h)] t
  | ((s, e)::ps), (h::t) when h = e + 1 ->
      startends_of_range_inner ((s, h)::ps) t
  | ps, (h::t) ->
      startends_of_range_inner ((h, h)::ps) t

let startends_of_range x =
  startends_of_range_inner [] x

(* Calculating margins *)
let calculate_margins filename pdf (s, e) =
  (* Call ghostscript *)
  let gscall =
   args.path_to_ghostscript ^
   " -dSAFER -dNOPAUSE -dBATCH -sDEVICE=bbox -r1200"
   ^ " -dFirstPage="
   ^ string_of_int s
   ^ " -dLastPage=" ^ string_of_int e
   ^ " \"" ^ filename ^ "\"" ^ " > waste.txt 2> margins.txt"
  in
    match Sys.command gscall with
    | 0 ->
      (* Parse white boxes *)
      let whiteboxes =
        parse_whiteboxes "margins.txt"
      (* Get media boxes *)
      and mediaboxes =
        take' (e - s + 1)
          (drop' (s - 1)
            (map
              (function page ->
                 (* Prefer the crop box *)
                 match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
                 | Some pdfobject -> Pdf.parse_rectangle (Pdf.direct pdf pdfobject)
                 | None -> Pdf.parse_rectangle page.Pdfpage.mediabox)
              (Pdfpage.pages_of_pagetree pdf)))
      in
        iter2
          (fun (m_minx, m_miny, m_maxx, m_maxy) (w_minx, w_miny, w_maxx, w_maxy) ->
             if w_minx = 0. && w_miny = 0. && w_maxx = 0. && w_maxy = 0. then
               Printf.printf "100, 100, 100, 100\n"
             else
               let topmargin = ((m_maxy -. w_maxy) /. (m_maxy -. m_miny)) *. 100.
               and bottommargin = ((w_miny -. m_miny) /. (m_maxy -. m_miny)) *. 100.
               and leftmargin = ((w_minx -. m_minx) /. (m_maxx -. m_minx)) *. 100.
               and rightmargin = ((m_maxx -. w_maxx) /. (m_maxx -. m_minx)) *. 100. in
                Printf.printf "%f, %f, %f, %f\n" leftmargin rightmargin topmargin bottommargin)
          mediaboxes
          whiteboxes;
      (* Clean up temp files *)
      Sys.remove "margins.txt";
      Sys.remove "waste.txt"
   | _ -> Printf.eprintf "Call to ghostscript failed."

let calculate_margins filename pdf range =
  iter (calculate_margins filename pdf) (startends_of_range (sort compare range))

(* copy the contents of the box f to the box t. If mediabox_if_missing is set,
the contents of the mediabox will be used if the from fox is not available. If
mediabox_is_missing is false, the page is unaltered. *)
let copy_box f t mediabox_if_missing pdf range =
  Cpdf.process_pages
    (fun _ page ->
       if f = "/MediaBox" then
         {page with Pdfpage.rest =
            (Pdf.add_dict_entry page.Pdfpage.rest t (page.Pdfpage.mediabox))}
       else
         match Pdf.lookup_direct pdf f page.Pdfpage.rest with
         | Some pdfobject ->
             if t = "/MediaBox"
               then {page with
                       Pdfpage.mediabox = Pdf.direct pdf pdfobject}
               else {page with Pdfpage.rest =
                       (Pdf.add_dict_entry page.Pdfpage.rest t (Pdf.direct pdf pdfobject))}
         | None ->
             if mediabox_if_missing
               then {page with Pdfpage.rest = Pdf.add_dict_entry page.Pdfpage.rest t page.Pdfpage.mediabox}
               else page)
    pdf
    range

(* Remove Embedded fonts. This is done by removing the Font Descriptor. *)
let remove_fontdescriptor pdf = function
  | Pdf.Dictionary d as font ->
      begin match lookup "/Type" d with
      | Some (Pdf.Name "/Font") ->
         (match Pdf.lookup_direct pdf "/FontDescriptor" font with
         | Some fontdes ->
             let fontdescriptor' =
               Pdf.remove_dict_entry
                (Pdf.remove_dict_entry
                  (Pdf.remove_dict_entry fontdes "/FontFile")
                   "/FontFile2")
                 "/FontFile3"
             in
               Pdf.add_dict_entry font "/FontDescriptor" (Pdf.Indirect (Pdf.addobj pdf fontdescriptor'))
         | _ -> font)
      | _ -> font
      end
  | x -> x

let remove_fonts pdf =
  Pdf.objiter (fun k v -> ignore (Pdf.addobj_given_num pdf (k, remove_fontdescriptor pdf v))) pdf;
  pdf

let dump_attachment out pdf (_, embeddedfile) =
  match Pdf.lookup_direct pdf "/F" embeddedfile with
  | Some (Pdf.String s) ->
      let efdata =
        begin match Pdf.lookup_direct pdf "/EF" embeddedfile with
        | Some d ->
            let stream =
              match Pdf.lookup_direct pdf "/F" d with
              | Some s -> s
              | None -> error "Bad embedded file stream"
            in
              Pdfcodec.decode_pdfstream_until_unknown pdf stream;
              begin match stream with Pdf.Stream {contents = (_, Pdf.Got b)} -> b | _ -> error "Bad embedded file stream" end
        | _ -> error "Bad embedded file stream"
        end
      in
        let filename = if out = "" then s else out ^ "/" ^ s in (* FIXME dirsep! *)
        (*i Printf.printf "writing to %s\n" filename; i*)
          let fh = open_out_bin filename in
            for x = 0 to bytes_size efdata - 1 do output_byte fh (bget efdata x) done;
            close_out fh
  | _ -> ()

let dump_attached_document pdf out =
  let root = Pdf.lookup_obj pdf pdf.Pdf.root in
    let names =
      match Pdf.lookup_direct pdf "/Names" root with Some n -> n | _ -> Pdf.Dictionary []
    in
      match Pdf.lookup_direct pdf "/EmbeddedFiles" names with
      | Some x ->
          iter (dump_attachment out pdf) (Pdf.contents_of_nametree pdf x)
      | None -> () 

let dump_attached_page pdf out page =
  let annots =
    match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
    | Some (Pdf.Array l) -> l
    | _ -> []
  in
    let efannots =
      keep
        (fun annot ->
           match Pdf.lookup_direct pdf "/Subtype" annot with
           | Some (Pdf.Name "/FileAttachment") -> true
           | _ -> false)
        annots
    in
      let fsannots = option_map (Pdf.lookup_direct pdf "/FS") efannots in
        iter (dump_attachment out pdf) (map (fun x -> 0, x) fsannots)

(* Dump both document-level and page-level attached files to file, using their file names *)
let dump_attached_files pdf out =
  try
    dump_attached_document pdf out;
    iter (dump_attached_page pdf out) (Pdfpage.pages_of_pagetree pdf)
  with
    _ -> error "Couldn't dump attached files"

(* If pages in stamp < pages in main, extend stamp by repeating its last page. If pages in stamp more, chop stamp *)
let equalize_pages_extend main stamp =
  let length_stamp = Pdfpage.endpage stamp
  in let length_main = Pdfpage.endpage main
  in let extend_lastpage lastpage page len =
    Pdfpage.change_pages true page (Pdfpage.pages_of_pagetree page @ (many lastpage len))
  in let chop pdf n =
    Pdfpage.change_pages true pdf (take (Pdfpage.pages_of_pagetree pdf) n)
  in
    if length_stamp > length_main
      then chop stamp length_main
      else extend_lastpage (last (Pdfpage.pages_of_pagetree stamp)) stamp (length_main - length_stamp)

let remove_unused_resources_page pdf n page =
  let xobjects, all_names =
    match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
    | Some (Pdf.Dictionary d) -> Pdf.Dictionary d, map fst d
    | _ -> Pdf.Dictionary [], []
  in
    let names_to_keep =
      option_map
        (function Pdfops.Op_Do n -> Some n | _ -> None)
        (Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content)
    in
      let names_to_remove = lose (mem' names_to_keep) all_names in
        let xobjdict = fold_left (Pdf.remove_dict_entry) xobjects names_to_remove in
          {page with Pdfpage.resources = Pdf.add_dict_entry page.Pdfpage.resources  "/XObject" xobjdict}

let remove_unused_resources pdf =
  Cpdf.process_pages (remove_unused_resources_page pdf) pdf (ilist 1 (Pdfpage.endpage pdf))

let extract_page_text pdf _ page =
  let text_extractor = ref None in
    fold_left ( ^ ) ""
      (map
        (function
         | Pdfops.Op_Tf (fontname, _) ->
             let fontdict =
               match Pdf.lookup_direct pdf "/Font" page.Pdfpage.resources with
               | None -> raise (Pdf.PDFError "Missing /Font in text extraction")
               | Some d ->
                   match Pdf.lookup_direct pdf fontname d with
                   | None -> raise (Pdf.PDFError "Missing font in text extraction")
                   | Some d -> d
             in
               text_extractor := Some (Pdftext.text_extractor_of_font pdf fontdict);
               ""
         | Pdfops.Op_Tj text when !text_extractor <> None ->
             Pdftext.utf8_of_codepoints
               (Pdftext.codepoints_of_text (unopt !text_extractor) text)
         | Pdfops.Op_TJ (Pdf.Array objs) when !text_extractor <> None ->
             fold_left ( ^ ) ""
               (option_map
                  (function
                   | Pdf.String text ->
                       Some
                         (Pdftext.utf8_of_codepoints
                           (Pdftext.codepoints_of_text (unopt !text_extractor) text))
                   | _ -> None)
                  objs)
         | _ -> "")
        (Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content))

(* For each page, extract all the ops with text in them, and concatenate it all together *)
let extract_text pdf range =
  fold_left ( ^ ) ""
    (Cpdf.map_pages (extract_page_text pdf) pdf range)

(* Extracts font to font.dat in CWD. *)
let extract_fontfile pagenumber fontname pdf =
  let resources = (select pagenumber (Pdfpage.pages_of_pagetree pdf)).Pdfpage.resources in
    match Pdf.lookup_direct pdf "/Font" resources with
    | None -> failwith "extract_fontfile: font not found"
    | Some fonts ->
        let fontobj = Pdf.lookup_fail "no /Fonts" pdf fontname fonts in
          let font = Pdftext.read_font pdf fontobj in
            match font with
            | Pdftext.CIDKeyedFont (_, {Pdftext.cid_fontdescriptor = {Pdftext.fontfile = Some fontfile}}, _)
            | Pdftext.SimpleFont {Pdftext.fontdescriptor = Some {Pdftext.fontfile = Some fontfile}} ->
                begin let objnum =
                  match fontfile with
                  | Pdftext.FontFile i -> i
                  | Pdftext.FontFile2 i -> i
                  | Pdftext.FontFile3 i -> i
                in
                  match Pdf.lookup_obj pdf objnum with
                  | Pdf.Stream s as obj ->
                      Pdfcodec.decode_pdfstream pdf obj;
                      begin match s with
                      | {contents = (_, Pdf.Got bytes)} ->
                           let fh = open_out_bin "font.dat" in
                             for x = 0 to bytes_size bytes - 1 do output_byte fh (bget bytes x) done;
                             close_out fh;
                             (* Now try to read using Pdfcff module *)
                             (*let font = Pdftruetype.to_type3 pdf font in*)
                               (*let extractor = Pdftext.text_extractor_of_font pdf fontobj in*)
                                 (*flprint "glyph names for incodes 0,1,2,3...";
                                 iter print_string (Pdftext.glyphnames_of_text extractor "\000\001\002\003\004\005\006\007");
                                 flprint "\n";*)
                                 ()
                      | _ -> failwith "extract_fontfile"
                      end
                  | _ -> failwith "extract_fontfile"
                end
            | _ -> failwith "unsupported or unfound font"

let addrectangle (w, h) color position relative_to_cropbox underneath range pdf =
  let addrectangle_page _ page =
    page
  in
    Cpdf.process_pages addrectangle_page pdf range

let print_spot_colour n s =
  Printf.printf "%i %s\n" n s

let rec really_list_spot_colours pagenum = function
  | Pdfspace.Separation (n, t, _) ->
      print_spot_colour pagenum n;
      really_list_spot_colours pagenum t
  | Pdfspace.Indexed (t, _)
  | Pdfspace.PatternWithBaseColourspace t
  | Pdfspace.DeviceN (_, t, _, _) -> really_list_spot_colours pagenum t
  | _ -> ()

let list_spot_colours_colourspace pagenum pdf resources cs =
  let space = Pdfspace.read_colourspace pdf resources cs in
    really_list_spot_colours pagenum space

let list_spot_colours_csdict pagenum pdf resources = function
  Pdf.Dictionary items ->
    List.iter
      (list_spot_colours_colourspace pagenum pdf resources)
      (List.map snd items) (* FIXME indirect *)
| _ -> raise (Pdf.PDFError "Bad csdict in list_spot_colours_csdict")

let list_spot_colours_xobject pagenum pdf resources xobject =
  match Pdf.lookup_direct pdf "/Resources" xobject with
    Some resources ->
      begin match Pdf.lookup_direct pdf "/ColorSpace" resources with
        Some csdict -> list_spot_colours_csdict pagenum pdf resources csdict
      | None -> ()
      end
  | None -> ()

let list_spot_colours_xobjectdict pagenum pdf resources = function
  Pdf.Dictionary items ->
    List.iter
      (list_spot_colours_xobject pagenum pdf resources)
      (List.map snd items) (* FIXME indirect *)
| _ -> raise (Pdf.PDFError "Bad xobjectdict in list_spot_colours_xobjectdict")

let list_spot_colours_page pdf pagenumber page =
  begin match Pdf.lookup_direct pdf "/ColorSpace" page.Pdfpage.resources with
    None -> ()
  | Some csdict ->
      list_spot_colours_csdict pagenumber pdf page.Pdfpage.resources csdict
  end;
  begin match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
    None -> ()
  | Some xobjectdict ->
      list_spot_colours_xobjectdict pagenumber pdf page.Pdfpage.resources xobjectdict
  end

let list_spot_colours (pdf : Pdf.t) (range : int list) =
  Cpdf.iter_pages (list_spot_colours_page pdf) pdf range

(* Main function *)
let go () =
  match args.op with
  | Some Version ->
      flprint
        ("cpdf Version " ^ string_of_int major_version ^ "." ^ string_of_int minor_version ^ " " ^ version_date ^ "\n")
  | None | Some Merge ->
      begin match args.out, args.inputs with
      | _, (_::_ as inputs) ->
          let op = match inputs with [_] -> None | _ -> Some Merge in
          let write_pdf x pdf =
            match args.keep_this_id with
            | None -> write_pdf x pdf
            | Some s ->
                (* get the ID from the file with name 's', and copy to pdf *)
                let namewiths =
                  keep (function (InFile s', _, _, _, _, _) when s' = s -> true | _ -> false) inputs
                in
                  match namewiths with
                  | (namewiths, _, _, _, _, _) as input::t ->
                      let spdf = get_pdf_from_input_kind input op namewiths in
                        write_pdf x (Cpdf.copy_id true spdf pdf)
                  | _ -> write_pdf x pdf
          in
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
                    (* FIXME Here, if the output file is different from the
                    input file, and we're just extracting pages, might we use a
                    lazy read? *)
                    if hd ranges <> "all" || !Pdfpage.flat_pagetrees then
                      let range = parse_pagespec pdf (hd ranges) in
                        let newpdf = Pdfpage.pdf_of_pages ~retain_numbering:args.retain_numbering pdf range in
                          write_pdf false newpdf
                    else
                        write_pdf false pdf
                | _ ->
                    (* We check permissions. A merge is allowed if each file
                     included was (a) not encrypted (detected by the absence of
                     saved encryption information in the PDF, or (b) decrypted using
                     the owner password (stored in the input) *)
                    if
                      not
                        (fold_left ( && ) true
                          (map2
                            (fun (_, _, _, _, was_dec_with_owner, _) pdf ->
                               !was_dec_with_owner || pdf.Pdf.saved_encryption = None)
                            inputs
                            pdfs))
                    then
                      soft_error "Merge requires the owner password for all encrypted files."
                    else
                      (* If args.keep_this_id is set, change the ID to the one from the kept one *)
                      let rangenums = map2 parse_pagespec pdfs ranges in
                        let outpdf =
                          Pdfmerge.merge_pdfs args.retain_numbering args.remove_duplicate_fonts
                          (map string_of_input_kind names) pdfs rangenums
                        in
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
  | Some RemoveUnusedResources ->
      begin match args.inputs, args.out with
      | _::_, _ ->
         let pdf = get_single_pdf (Some RemoveUnusedResources) false in
           let outpdf = remove_unused_resources pdf in
             write_pdf true outpdf
      | _ -> error "RemoveUnusedResources: bad command line"
      end
  | Some (CopyFont fromfile) ->
      begin match args.inputs, args.out with
      | (_, pagespec, u, o, _, _)::_, _ ->
          let pdf = get_single_pdf (Some (CopyFont fromfile)) false
          and frompdf = pdfread_pdf_of_file (optstring u) (optstring o) fromfile in
            let range = parse_pagespec pdf pagespec in
              let copyfontname =
                match args.copyfontname with
                | Some x -> x
                | None -> failwith "copy_font: no font name given"
              in
                let outpdf = copy_font frompdf copyfontname args.copyfontpage range pdf in
                  write_pdf true outpdf
      | _ -> error "copyfont: bad command line"
      end
  | Some RemoveFonts ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf (Some RemoveFonts) false in
            write_pdf true (remove_fonts pdf)
      | _ -> error "remove fonts: bad command line"
      end
  | Some ExtractFontFile ->
      (*Graphics.open_graph " 1600x1050";*)
      begin match args.inputs, args.out with
      | (_, pagespec, u, o, _, _)::_, _ ->
          let pdf = get_single_pdf (Some ExtractFontFile) false in
            let page = args.copyfontpage
            and name =
              match args.copyfontname with
              | Some x -> x
              | None -> failwith "extract fontfile: no font name given"
            in
              extract_fontfile page name pdf
      | _ -> error "extract fontfile: bad command line"
      end 
  | Some CountPages ->
      let pdf, inname, input =
        match args.inputs with
        | (InFile inname, _, u, o, _, revision) as input::_ ->
             pdfread_pdf_of_channel_lazy ?revision (optstring u) (optstring o) (open_in_bin inname), inname, input
        | (StdIn, _, u, o, _, revision) as input::_ -> pdf_of_stdin ?revision u o, "", input
        | (AlreadyInMemory pdf, _, _, _, _, _) as input::_ -> pdf, "", input
        | _ -> raise (Arg.Bad "cpdf: No input specified.\n")
      in
        (*let pdf = decrypt_if_necessary input (Some CountPages) pdf in*)
        (* 3/11/2016. We removed decryption here, because it doesn't seem necessary. Put
         * back in on counterexample *)
          output_page_count pdf
  | Some Revisions ->
      let input =
        match args.inputs with
          (InFile inname, _, _, _, _, _)::_ -> Pdfio.input_of_channel (open_in_bin inname)
        | (StdIn, _, _, _, _, _)::_ -> Pdfio.input_of_channel stdin
        | _ -> raise (Arg.Bad "Revisions: must be a filename or stdin")
      in
        begin try
          Printf.printf "%i\n%!" (Pdfread.revisions input) 
        with
          _ -> soft_error "Malformed XRef table. Cannot determine number of revisions."
        end
  | Some Clean ->
      begin match args.out with
      | (File _ | Stdout) ->
          let pdf' = get_single_pdf (Some Clean) false in
            write_pdf false pdf'
      | _ -> error "Clean: No output specified"
      end
  | Some Info ->
      let pdf, inname, input =
        match args.inputs with
        | (InFile inname, _, u, o, _, revision) as input::_ ->
             pdfread_pdf_of_channel_lazy ?revision (optstring u) (optstring o) (open_in_bin inname), inname, input
        | (StdIn, _, u, o, _, revision) as input::_ -> pdf_of_stdin ?revision u o, "", input
        | (AlreadyInMemory pdf, _, _, _, _, _) as input::_ -> pdf, "", input
        | _ -> raise (Arg.Bad "cpdf: No input specified.\n")
      in
        Printf.printf "Encryption: %s\n" (getencryption pdf);
        Printf.printf "Permissions: %s\n" (getpermissions pdf);
        if inname <> "" then
          Printf.printf "Linearized: %b\n" (Pdfread.is_linearized (Pdfio.input_of_channel (open_in_bin inname)));
        let pdf = decrypt_if_necessary input (Some Info) pdf in
          Cpdf.output_info args.encoding pdf;
          Cpdf.output_xmp_info args.encoding pdf
  | Some PageInfo ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf args.op true in
            let range = parse_pagespec pdf pagespec in
              Cpdf.output_page_info (get_single_pdf (Some PageInfo) true) range
      | _ -> error "list-bookmarks: bad command line"
      end
  | Some Metadata ->
      Cpdf.print_metadata (get_single_pdf (Some Metadata) true)
  | Some Fonts ->
      Cpdf.print_fonts (get_single_pdf (Some Fonts) true)
  | Some ListBookmarks ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
        let pdf = get_single_pdf args.op true in
          let range = parse_pagespec pdf pagespec in
            Cpdf.list_bookmarks args.encoding range pdf (Pdfio.output_of_channel stdout);
            flush stdout
      | _ -> error "list-bookmarks: bad command line"
      end
  | Some Crop ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf (Some Crop) false in
            let xywhlist = parse_rectangles pdf args.rectangle in
              let range = parse_pagespec pdf pagespec in
                let pdf = Cpdf.crop_pdf xywhlist pdf range in
                  write_pdf false pdf
      | _ -> error "crop: bad command line"
      end
  | Some MediaBox ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf (Some MediaBox) false in
            let xywhlist = parse_rectangles pdf args.rectangle in
              let range = parse_pagespec pdf pagespec in
                let pdf = Cpdf.set_mediabox xywhlist pdf range in
                  write_pdf false pdf
      | _ -> error "set media box: bad command line"
      end
  | Some CopyBox ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf (Some CopyBox) false in
            let range = parse_pagespec pdf pagespec in
              let f, t =
                begin match args.frombox, args.tobox with
                | Some f, Some t -> f, t
                | _ -> error "Copy box: no tobox or no frombox specified"
                end
              in
                let pdf = copy_box f t args.mediabox_if_missing pdf range in
                  write_pdf false pdf
      | _ -> error "Copy Box: bad command line"
      end
  | Some Decompress ->
      let pdf = get_single_pdf (Some Decompress) true in
        Pdf.iter_stream
          (function stream ->
             try Pdfcodec.decode_pdfstream_until_unknown pdf stream with
               e -> Printf.eprintf "Decode failure: %s. Carrying on...\n" (Printexc.to_string e); ())
          pdf;
        write_pdf ~is_decompress:true args.makenewid pdf
  | Some Compress ->
      let pdf = get_single_pdf (Some Compress) false in
        if args.remove_duplicate_streams then
          Pdfmerge.remove_duplicate_fonts pdf;
        write_pdf false (Cpdf.recompress_pdf pdf)
  | Some RemoveCrop ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf (Some RemoveCrop) false in
            let range = parse_pagespec pdf pagespec in
              let pdf = Cpdf.remove_cropping_pdf pdf range in
                write_pdf false pdf
      | _ -> error "remove-crop: bad command line"
      end
  | Some CopyCropBoxToMediaBox ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf (Some CopyCropBoxToMediaBox) false in
            let range = parse_pagespec pdf pagespec in
              let pdf = copy_cropbox_to_mediabox pdf range in
                write_pdf false pdf
      | _ -> error "remove-crop: bad command line"
      end
  | Some (Rotate _)  | Some (Rotateby _) ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf args.op false in
            let range = parse_pagespec pdf pagespec in
              let rotate =
                match args.op with
                | Some (Rotate i) -> Cpdf.rotate_pdf i
                | Some (Rotateby i) -> Cpdf.rotate_pdf_by i
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
            let range = parse_pagespec pdf pagespec in
              let pdf = Cpdf.rotate_contents ~fast:args.fast a pdf range in
                write_pdf false pdf
      | _ -> error "rotate-contents: bad command line"
      end
  | Some Upright ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf args.op false in
            let range = parse_pagespec pdf pagespec in
              let pdf = Cpdf.upright ~fast:args.fast range pdf in
                write_pdf false pdf
      | _ -> error "rotate-contents: bad command line"
      end
  | Some ((VFlip | HFlip) as flip) ->
      begin match args.inputs, args.out with
      | (_, pagespec, _, _, _, _)::_, _ ->
          let pdf = get_single_pdf args.op false in
            let range = parse_pagespec pdf pagespec in
              let pdf = 
                if flip = VFlip
                  then Cpdf.vflip_pdf ~fast:args.fast pdf range
                  else Cpdf.hflip_pdf ~fast:args.fast pdf range
              in
                write_pdf false pdf
      | _ -> error "flip: bad command line"
      end
  | Some ((SetAuthor _ | SetTitle _ | SetSubject _ | SetKeywords _
          | SetCreate _ | SetModify _ | SetCreator _ | SetProducer _
          | SetTrapped | SetUntrapped) as op) ->
      let key, value, version  =
        let f s = if args.encoding <> Cpdf.Raw then Pdftext.pdfdocstring_of_utf8 s else unescape_octals s in
          match op with
          | SetAuthor s -> "/Author", Pdf.String (f s), 0
          | SetTitle s -> "/Title", Pdf.String (f s), 1
          | SetSubject s -> "/Subject", Pdf.String (f s), 1
          | SetKeywords s -> "/Keywords", Pdf.String (f s), 1
          | SetCreate s -> "/CreationDate", Pdf.String (Cpdf.expand_date s), 0
          | SetModify s -> "/ModDate", Pdf.String (Cpdf.expand_date s), 0
          | SetCreator s -> "/Creator", Pdf.String (f s), 0
          | SetProducer s -> "/Producer", Pdf.String (f s), 0
          | SetTrapped -> "/Trapped", Pdf.Boolean true, 3
          | SetUntrapped -> "/Trapped", Pdf.Boolean false, 3
          | _ -> assert false
      in
        let pdf = get_single_pdf args.op false in
          let version = if args.keepversion then pdf.Pdf.minor else version in
            write_pdf false (Cpdf.set_pdf_info (key, value, version) pdf)
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
     let version = if args.keepversion then pdf.Pdf.minor else version in
          write_pdf false (Cpdf.set_viewer_preference (key, value, version) pdf)
      end
  | Some (OpenAtPage n) ->
      write_pdf false (Cpdf.set_open_action (get_single_pdf args.op false) false n)
  | Some (OpenAtPageFit n) ->
      write_pdf false (Cpdf.set_open_action (get_single_pdf args.op false) true n)
  | Some (SetMetadata metadata_file) ->
      write_pdf false (Cpdf.set_metadata args.keepversion metadata_file (get_single_pdf args.op false))
  | Some (SetVersion v) ->
      let pdf = get_single_pdf args.op false in
         write_pdf false {pdf with Pdf.minor = v}
  | Some (SetPageLayout s) ->
           write_pdf false (Cpdf.set_page_layout (get_single_pdf args.op false) s)
  | Some (SetPageMode s) ->
           write_pdf false (Cpdf.set_page_mode (get_single_pdf args.op false) s)
  | Some Split ->
      begin match args.inputs, args.out with
        | [(f, ranges, _, _, _, _)], File output_spec ->
            let pdf = get_single_pdf args.op true in
              let enc =
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
              in
                args.create_objstm <- args.preserve_objstm;
                split_pdf
                  enc args.original_filename args.chunksize args.linearize args.cpdflin
                  args.preserve_objstm args.preserve_objstm (*yes--always create if preserving *)
                  args.squeeze nobble output_spec pdf
        | _, Stdout -> error "Can't split to standard output"
        | _, NoOutputSpecified -> error "Split: No output format specified"
        | _ -> error "Split: bad parameters"
      end
  | Some (SplitOnBookmarks level) ->
      begin match args.out with
        | File output_spec ->
            let pdf = get_single_pdf args.op false in
              let enc =
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
              in
                args.create_objstm <- args.preserve_objstm;
                split_at_bookmarks
                  enc args.original_filename args.squeeze nobble level output_spec pdf
        | Stdout -> error "Can't split to standard output"
        | NoOutputSpecified -> error "Split: No output format specified"
      end
  | Some Presentation ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec pdf (get_pagespec ()) in
          let pdf' =
            Cpdf.presentation
              range
              args.transition args.duration args.horizontal
              args.inward args.direction args.effect_duration pdf
          in
            pdf.Pdf.minor <- if args.keepversion then pdf.Pdf.minor else max pdf.Pdf.minor 1;
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
            Cpdf.copy_id
              args.keepversion
              (pdfread_pdf_of_file (optstring u) (optstring o) getfrom)
              (get_pdf_from_input_kind input args.op k)
          in
            write_pdf false pdf
      | _ -> error "copy-id: No input file specified"
      end
  | Some (ThinLines w) ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec pdf (get_pagespec ()) in
          write_pdf false (Cpdf.thinlines range w pdf)
  | Some BlackText ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec pdf (get_pagespec ()) in
          write_pdf false (Cpdf.blacktext range pdf)
  | Some BlackLines ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec pdf (get_pagespec ()) in
          write_pdf false (Cpdf.blacklines range pdf)
  | Some BlackFills ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec pdf (get_pagespec ()) in
          write_pdf false (Cpdf.blackfills range pdf)
  | Some RemoveAnnotations ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec pdf (get_pagespec ()) in
          write_pdf false (Cpdf.remove_annotations range pdf)
  | Some (CopyAnnotations getfrom) ->
      begin match args.inputs with
      | [(k, _, u, o, _, _) as input] ->
        let input_pdf = get_pdf_from_input_kind input args.op k in
          let range = parse_pagespec input_pdf (get_pagespec ()) in
            let pdf =
              Cpdf.copy_annotations
                range
                (pdfread_pdf_of_file (optstring u) (optstring o) getfrom)
                input_pdf
          in
            write_pdf false pdf
      | _ -> error "copy-annotations: No input file specified"
      end
  | Some ListAnnotations ->
      Cpdf.list_annotations args.encoding (get_single_pdf args.op true)
  | Some ListAnnotationsMore ->
      Cpdf.list_annotations_more (get_single_pdf args.op true)
  | Some Shift ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec pdf (get_pagespec ()) in
          let dxdylist = parse_coordinates pdf args.coord in
            write_pdf false (Cpdf.shift_pdf ~fast:args.fast dxdylist pdf range)
  | Some Scale ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec pdf (get_pagespec ()) in
          let sxsylist = parse_coordinates pdf args.coord in
            write_pdf false (Cpdf.scale_pdf ~fast:args.fast sxsylist pdf range)
  | Some ScaleToFit ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec pdf (get_pagespec ()) in
          let xylist = parse_coordinates pdf args.coord
          and scale = args.scale in
            write_pdf false (Cpdf.scale_to_fit_pdf ~fast:args.fast scale xylist args.op pdf range)
  | Some (ScaleContents scale) ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec pdf (get_pagespec ()) in
          write_pdf false (Cpdf.scale_contents ~fast:args.fast args.position scale pdf range)
  | Some ListAttachedFiles ->
      let pdf = get_single_pdf args.op false in
        let attachments = Cpdf.list_attached_files pdf in
        iter
          (fun a -> Printf.printf "%i %s\n" a.Cpdf.pagenumber a.Cpdf.name)
          attachments;
        flprint ""
  | Some DumpAttachedFiles ->
      let pdf = get_single_pdf args.op false in
        begin match args.out with
        | NoOutputSpecified -> dump_attached_files pdf ""
        | File n -> dump_attached_files pdf n
        | Stdout -> error "Can't dump attachments to stdout"
        end
  | Some RemoveAttachedFiles ->
      write_pdf false (Cpdf.remove_attached_files (get_single_pdf args.op false))
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
            let pdf = fold_left (Cpdf.attach_file args.keepversion topage) pdf (rev files) in
              write_pdf false pdf
      | _ -> error "attach file: No input file specified"
      end
  | Some PadBefore ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec pdf (get_pagespec ()) in
          write_pdf false (Cpdf.padbefore range pdf)
  | Some PadAfter ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec pdf (get_pagespec ()) in
          write_pdf false (Cpdf.padafter range pdf)
  | Some (PadEvery n) ->
      let pdf = get_single_pdf args.op false in
        let range =
          match keep (function m -> m mod n = 0) (ilist 1 (Pdfpage.endpage pdf)) with
          | [] -> []
          | l -> if last l = Pdfpage.endpage pdf then all_but_last l else l
        in
          write_pdf false (Cpdf.padafter range pdf)
  | Some (PadMultiple n) ->
      let pdf = get_single_pdf args.op false in
        write_pdf false (Cpdf.padmultiple n pdf)
  | Some Draft ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec pdf (get_pagespec ()) in
          write_pdf false (Cpdf.draft args.boxes range pdf)
  | Some (AddText text) ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec pdf (get_pagespec ()) in
          let font =
            match args.font with
            | StandardFont f -> Some f
            | OtherFont f -> None (* it's in fontname *)
          in
            let pdf =
              if args.prerotate then Cpdf.upright ~fast:args.fast range pdf else pdf
            and filename =
              match args.inputs with
              | (InFile inname, _, _, _, _, _)::_ -> inname
              | _ -> ""
            in
              write_pdf false
                (Cpdf.addtexts
                   false args.linewidth args.outline args.fast args.fontname
                   font args.embedfonts args.bates args.batespad args.color args.position
                   args.linespacing args.fontsize args.underneath text range
                   args.orientation args.relative_to_cropbox args.opacity
                   args.justification args.midline args.topline filename pdf)
  | Some RemoveText ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec pdf (get_pagespec ()) in
          write_pdf false (Cpdf.removetext range pdf)
  | Some AddRectangle ->
      let pdf = get_single_pdf args.op false in
        let range = parse_pagespec pdf (get_pagespec ()) in
          write_pdf false
            (addrectangle
               (parse_coordinate pdf args.coord)
               args.color args.position args.relative_to_cropbox args.underneath range pdf)
  | Some (AddBookmarks file) ->
      write_pdf false
        (Cpdf.add_bookmarks true (Pdfio.input_of_channel (open_in_bin file))
          (get_single_pdf args.op false))
  | Some RemoveBookmarks ->
      write_pdf false (Pdfmarks.remove_bookmarks (get_single_pdf args.op false))
  | Some TwoUp ->
      write_pdf false (Cpdf.twoup (get_single_pdf args.op false))
  | Some TwoUpStack ->
      write_pdf false (Cpdf.twoup_stack (get_single_pdf args.op false))
  | Some (StampOn over) ->
      let overpdf =
        match over with
        | "stamp_use_stdin" -> pdf_of_stdin "" ""
        | x -> pdfread_pdf_of_file None None x
      in
        let pdf = get_single_pdf args.op false in
          let range = parse_pagespec pdf (get_pagespec ()) in
            let pdf =
              Cpdf.stamp
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
          let range = parse_pagespec pdf (get_pagespec ()) in
            let pdf =
              Cpdf.stamp
                args.relative_to_cropbox args.position args.topline args.midline args.fast
                args.scale_stamp_to_fit false range underpdf pdf
            in
              write_pdf false pdf
  | Some (CombinePages over) ->
      write_pdf false
        (Cpdf.combine_pages args.fast (get_single_pdf args.op false) (pdfread_pdf_of_file None None over) false false true)
  | Some Encrypt ->
      let pdf = get_single_pdf args.op false in
        let pdf = Cpdf.recompress_pdf <| nobble pdf
        and encryption =
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
        in
          Pdf.remove_unreferenced pdf;
          if not args.keepversion then
            begin
              let newversion =
                match args.crypt_method with
                "40bit" -> 1 | "128bit" -> 4 | "AES" -> 6 | "AES256" | "AES256ISO" -> 7 | _ -> 0
              in
                let newversion = if args.create_objstm then 5 else newversion in
                  pdf.Pdf.minor <- max pdf.Pdf.minor newversion
            end;
            write_pdf ~encryption:(Some encryption) args.makenewid pdf
  | Some Decrypt ->
      args.recrypt <- false;
      write_pdf false (get_single_pdf args.op false)
  | Some RemoveMetadata ->
      write_pdf false (Cpdf.remove_metadata (get_single_pdf args.op false))
  | Some ExtractImages ->
      let output_spec =
        begin match args.out with
        | File output_spec -> output_spec
        | _ -> ""
        end
      in
        let pdf = get_single_pdf args.op true in
          let range = parse_pagespec pdf (get_pagespec ()) in
            extract_images pdf range output_spec
  | Some (ImageResolution f) ->
      let pdf = get_single_pdf args.op true in
        let range = parse_pagespec pdf (get_pagespec ()) in
          image_resolution pdf range f
  | Some MissingFonts ->
      let pdf = get_single_pdf args.op true in
        let range = parse_pagespec pdf (get_pagespec ()) in
          missing_fonts pdf range
  | Some CSP1 ->
      write_pdf false (Cpdf.custom_csp1 (get_single_pdf (Some CSP1) false)) 
  | Some (CSP2 f) ->
      write_pdf false (Cpdf.custom_csp2 f (get_single_pdf (Some (CSP2 f)) false)) 
  | Some CSP3 ->
      begin match args.inputs with
      | [(InFile s, _, _, _, _, _)] ->
           let pdf = get_single_pdf args.op true in
             let range = parse_pagespec pdf (get_pagespec ()) in
               calculate_margins s pdf range
      | _ ->
         Printf.eprintf "CSP3: Too many input files or input not a file"
      end
  | Some ExtractText ->
      let pdf = get_single_pdf args.op true in
        let range = parse_pagespec pdf (get_pagespec ()) in
          let text = extract_text pdf range in
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
          Cpdf.add_page_labels
            pdf args.labelstyle args.labelprefix args.labelstartval range;
          write_pdf false pdf
  | Some RemovePageLabels ->
      let pdf = get_single_pdf args.op false in
        Pdfpagelabels.remove pdf;
        write_pdf false pdf
  | Some PrintPageLabels ->
      let pdf = get_single_pdf args.op true in
        iter
          print_string
          (map Pdfpagelabels.string_of_pagelabel (Pdfpagelabels.read pdf))
  | Some (RemoveDictEntry key) ->
      let pdf = get_single_pdf args.op true in
        (* 1. Process all objects *)
        Pdf.objselfmap
          (function
             (Pdf.Dictionary _ as d) | (Pdf.Stream _ as d) ->
               Pdf.remove_dict_entry d key
           | x -> x)
          pdf;
        (* FIXME: We might like to do the trailer dictionary too *)
        write_pdf false pdf
  | Some ListSpotColours ->
      let pdf = get_single_pdf args.op true in
        let range = parse_pagespec pdf (get_pagespec ()) in
          list_spot_colours pdf range

let parse_argv () =
  if args.debug then
    Array.iter (Printf.eprintf "arg: %s\n") Sys.argv;
  Arg.parse_argv ~current:(ref 0)

let align_specs s =
  Arg.align s

(* The old -control mechanism clashed with AND, but must be retained for
backwards compatibility. There is a new mechanism -args file which performs
direct textual substitution of the file, before any expansion of ANDs *)
let rec expand_args_inner prev = function
    [] -> rev prev
  | "-args"::filename::r ->
      expand_args_inner (rev (parse_control_file filename) @ prev) r
  | h::t -> expand_args_inner (h::prev) t

let expand_args argv =
  let l = Array.to_list argv in
    Array.of_list (expand_args_inner [] l)

(* FIXME: Now we call this repeatedly from interactive programs, careful to
ensure that all memory is cleaned. See clearance of filenames hashtable, for
example. *)

(* Main function. *)
let go_withargv argv =
  Hashtbl.clear filenames;
  if demo then
    flprint "This demo is for evaluation only. http://www.coherentpdf.com/\n";
  if noncomp then
    begin
      prerr_string "For non-commercial use only\n";
      prerr_string "To purchase a license visit http://www.coherentpdf.com/\n\n";
      flush stderr
    end;
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
           parse_argv () s (align_specs specs) anon_fun usage_msg;
           parse_argv () (Array.of_list ("cpdf"::!control_args)) (align_specs specs) anon_fun usage_msg;
           let addrange pdf = AlreadyInMemory pdf, args.dashrange, "", "", ref false, None in
             args.inputs <- rev (map addrange !output_pdfs) @ rev args.inputs;
             output_pdfs := [];
             go ())
         sets;
      flush stdout (*r for Windows *)
  with
  | Arg.Bad s ->
      prerr_string
        (implode (takewhile (neq '\n') (explode s)) ^ " Use -help for help.\n\n");
      flush stderr;
      if not !stay_on_error then exit 2 else raise StayOnError
  | Arg.Help _ ->
      Arg.usage (align_specs specs) usage_msg;
      flush stderr (*r for Windows *)
  | Sys_error s as e ->
      prerr_string (s ^ "\n\n");
      flush stderr;
      if not !stay_on_error then
        (if args.debug then raise e else exit 2)
      else raise StayOnError
  | Pdf.PDFError s as e ->
      prerr_string
        ("cpdf encountered an error. Technical details follow:\n\n" ^ s ^ "\n\n");
      flush stderr;
      if not !stay_on_error then
        if args.debug then raise e else exit 2
      else
        raise StayOnError
  | Cpdf.SoftError s -> soft_error s
  | Cpdf.HardError s -> error s
  | e ->
      prerr_string
        ("cpdf encountered an unexpected error. Technical Details follow:\n" ^
         Printexc.to_string e ^ "\n\n");
      flush stderr;
      if not !stay_on_error then
        (if args.debug then raise e else exit 2) else raise StayOnError

let go () =
  go_withargv Sys.argv

