open Pdfutil
open Cpdferror

exception MatterhornError of Cpdfyojson.Safe.t

exception MatterhornUnimplemented

let merror () = raise (MatterhornError `Null)
let merror_str s = raise (MatterhornError (`String s))
let unimpl () = raise MatterhornUnimplemented
let todo () = ()
let not_fully_implemented () = ()
let covered_elsewhere () = ()

(* A simple type for structure trees, for doing structure checks. For now just
   the element name, and its children. *)
type st = E of string * st list

let rec read_st_inner pdf stnode =
  let s =
    match Pdf.lookup_direct pdf "/S" stnode with
    | Some (Pdf.Name s) -> s
    | _ -> ""
  in
    match Pdf.lookup_direct pdf "/K" stnode with
    | None -> E (s, [])
    | Some (Pdf.Dictionary d) -> E (s, [read_st_inner pdf (Pdf.Dictionary d)])
    | Some (Pdf.Integer mcd) -> E (s, []) (* marked content identifier, we drop. *)
    | Some (Pdf.Array a) -> E (s, read_st_inner_array pdf a)
    | _ -> error "malformed st node"

and read_st_inner_array pdf nodes =
  map (read_st_inner pdf) nodes

let read_st pdf =
  match Pdf.lookup_obj pdf pdf.Pdf.root with
  | Pdf.Dictionary d ->
      begin match lookup "/StructTreeRoot" d with
      | None -> E ("/StructTreeRoot", [])
      | Some st -> E ("/StructTreeRoot", [read_st_inner pdf st])
      end
  | _ -> error "read_st no root"

let string_of_st st =
  let rec convert (E (s, ks)) = `Tuple [`String s; `List (map convert ks)] in
    Cpdfyojson.Safe.pretty_to_string (convert st)

(* Content marked as Artifact is present inside tagged content. *)
let matterhorn_01_003 pdf = todo ()

(* Tagged content is present inside content marked as Artifact. *)
let matterhorn_01_004 pdf = todo ()

(* Content is neither marked as Artifact nor tagged as real content. *)
let matterhorn_01_005 pdf = todo ()

(* Suspects entry has a value of true. *)
let matterhorn_01_007 pdf = todo ()

(* Here, for now, we allow the ISO 32000 and ISO 32000-2 *)
(* FIXME which verison of PDF/UA are we doing? Can we do both? or pick? *)
let standard_structure_types =
  ["/Document"; "/DocumentFragment"; "/Part"; "/Sect"; "/Div"; "/Aside";
  "/NonStruct"; "/P"; "/H1"; "/H2"; "/H3"; "/H4"; "/H5"; "/H6"; "/H"; "/Title";
  "/FENote"; "/Sub"; "/Lbl"; "/Span"; "/Em"; "/Strong"; "/Link"; "/Annot";
  "/Form"; "/Ruby"; "/RB"; "/RT"; "/RP"; "/Warichu"; "/WT"; "/WP"; "/L"; "/LI";
  "/LBody"; "/Table"; "/TR"; "/TH"; "/TD"; "/THead"; "/TBody"; "/TFoot";
  "/Caption"; "/Figure"; "/Formula"; "/Artifact";
  (* 2008 ISO 3200 only *)
  "/Art"; "/BlockQuote"; "/TOC"; "/TOCI"; "/Index"; "/Private"; "/Quote";
  "/Note"; "/Reference"; "/Code"]

let read_rolemap pdf = function
  | Pdf.Dictionary d ->
      option_map (function (k, Pdf.Name v) -> Some (k, v) | _ -> None) d
  | _ -> error "read_rolemap: not a rolemap"

let rec follow_standard rm n =
  match List.assoc_opt n rm with
  | None -> raise Exit
  | Some x when mem x standard_structure_types -> ()
  | Some x -> follow_standard rm x

let circular rm =
  let rec circular n k rm =
    n < 0 || match List.assoc_opt k rm with None -> false | Some k' -> circular (n - 1) k' rm
  in
    List.exists (fun k -> circular (length rm) k rm) (map fst rm)

(* One or more non-standard tag’s mapping does not terminate with a standard
type. *)
let matterhorn_02_001 pdf =
  match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/StructTreeRoot"; "/RoleMap"] with
  | Some rm ->
      let rolemap = read_rolemap pdf rm in
        if circular rolemap then () else (* Will be reported below *)
          iter (fun x -> try follow_standard rolemap x with Exit -> merror ()) (map fst rolemap)
  | None -> ()

(* A circular mapping exists. *)
let matterhorn_02_003 pdf =
  match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/StructTreeRoot"; "/RoleMap"] with
  | Some rm ->
      let rolemap = read_rolemap pdf rm in
        if circular rolemap then merror ()
  | None -> ()

(* One or more standard types are remapped. *)
let matterhorn_02_004 pdf =
  match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/StructTreeRoot"; "/RoleMap"] with
  | Some rm ->
      let rolemap = read_rolemap pdf rm in
      if List.exists (function k -> mem k standard_structure_types) (map fst rolemap) then merror ()
  | None -> ()

(* Document does not contain an XMP metadata stream *)
let matterhorn_06_001 pdf =
  match Cpdfmetadata.get_metadata pdf with
  | Some _ -> ()
  | None -> merror ()

(* The XMP metadata stream in the Catalog dictionary does not include the
   PDF/UA identifier. *)
let matterhorn_06_002 pdf =
  match Cpdfmetadata.get_metadata pdf with
  | Some metadata ->
      let _, tree = Cpdfmetadata.xmltree_of_bytes metadata in
        begin match Cpdfmetadata.get_data_for Cpdfmetadata.pdfuaid "part" tree with
        | Some _ -> ()
        | None -> merror ()
        end
  | None -> () (* case covered by test 06_001 above, no need for two failures *)

(* XMP metadata stream does not contain dc:title *)
let matterhorn_06_003 pdf =
  match Cpdfmetadata.get_metadata pdf with
  | Some metadata ->
      let _, tree = Cpdfmetadata.xmltree_of_bytes metadata in
        begin match Cpdfmetadata.get_data_for Cpdfmetadata.dc "title" tree with
        | Some _ -> ()
        | None -> merror ()
        end
  | None -> () (* case covered by test 06_001 above, no need for two failures *)

(* ViewerPreferences dictionary of the Catalog dictionary does not contain a
   DisplayDocTitle entry. *)
let matterhorn_07_001 pdf =
  match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
  | Some catalog ->
      begin match Pdf.lookup_direct pdf "/ViewerPreferences" catalog with
      | Some d ->
          begin match Pdf.lookup_direct pdf "/DisplayDocTitle" d with
          | Some _ -> ()
          | None -> merror ()
          end
      | None -> merror ()
      end
  | _ -> merror ()

(* ViewerPreferences dictionary of the Catalog dictionary contains a
   DisplayDocTitle entry with a value of false. *)
let matterhorn_07_002 pdf = 
  match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
  | Some catalog ->
      begin match Pdf.lookup_direct pdf "/ViewerPreferences" catalog with
      | Some d ->
          begin match Pdf.lookup_direct pdf "/DisplayDocTitle" d with
          | Some (Pdf.Boolean false) -> merror ()
          | _ -> ()
          end
      | None -> ()
      end
  | _ -> ()

(* A table-related structure element is used in a way that does not conform to
   the syntax defined in ISO 32000-1, Table 337. *)
let matterhorn_09_004 pdf = todo ()

(* A list-related structure element is used in a way that does not conform to
   Table 336 in ISO 32000-1. *)
let matterhorn_09_005 pdf =
  flprint "CHECKING LISTS...\n";
  let rec check_l = function
    | E ("/L", cs) ->
        (* 0 or 1 captions *)
        let cs = match cs with E ("/Caption", _)::cs | cs -> cs in
          (* 1 or n /LI *)
          begin match cs with
          | [] -> merror_str "No /LI in /L"
          | cs -> iter check_li cs
          end
    | E (_, cs) ->
        iter check_l cs
  and check_li = function
    (* for each /LI, 1 or n /Lbl or /LBody or both *)
    | E ("/LI", []) -> merror_str "Empty /LI"
    | E ("/LI", cs) -> iter check_li_child cs
    | E (_, _) -> merror_str "Unknown child of /LI"
    (* need to check all children of /LBody too, to see if any is /L *)
  and check_li_child = function
    | E (("/LBody"| "/Lbl"), cs) -> iter check_l cs
    | E (_, _) -> merror_str "Child of /LI must be /Lbl or /LBody"
  in
    check_l (read_st pdf)

(* A TOC-related structure element is used in a way that does not conform to
   Table 333 in ISO 32000-1. *)
let matterhorn_09_006 pdf = todo ()

(* A Ruby-related structure element is used in a way that does not conform to
   Table 338 in ISO 32000-1. *)
let matterhorn_09_007 pdf = todo ()

(* A Warichu-related structure element is used in a way that does not conform
   to Table 338 in ISO 32000-1. *)
let matterhorn_09_008 pdf = todo ()

(* Character code cannot be mapped to Unicode. *)
let matterhorn_10_001 pdf =
  unimpl ()

(* Natural language for text in page content cannot be determined. *)
let matterhorn_11_001 pdf =
  unimpl ()

(* Natural language for text in Alt, ActualText and E attributes cannot be
   determined. *)
let matterhorn_11_002 pdf = todo ()

(* Natural language in the Outline entries cannot be determined. *)
let matterhorn_11_003 pdf = todo ()

(* Natural language in the Contents entry for annotations cannot be determined.
 *)
let matterhorn_11_004 pdf = todo ()

(* Natural language in the TU entry for form fields cannot be determined. *)
let matterhorn_11_005 pdf = todo ()

(* Natural language for document metadata cannot be determined. *)
let matterhorn_11_006 pdf = todo ()

(* <Figure> tag alternative or replacement text missing. *)
let matterhorn_13_004 pdf = todo ()

(* Does use numbered headings, but the first heading tag is not <H1>. *)
let matterhorn_14_002 pdf = todo ()

(* Numbered heading levels in descending sequence are skipped (Example: <H3>
   follows directly after <H1>). *)
let matterhorn_14_003 pdf = todo ()

(* A node contains more than one <H> tag. *)
let matterhorn_14_006 pdf = todo ()

(* Document uses both <H> and <H#> tags. *)
let matterhorn_14_007 pdf = todo ()

(* In a table not organized with Headers attributes and IDs, a <TH> cell does
   not contain a Scope attribute. *)
let matterhorn_15_003 pdf = todo ()

(* <Formula> tag is missing an Alt attribute. *)
let matterhorn_17_002 pdf = todo ()

(* Unicode mapping requirements are not met. *)
let matterhorn_17_003 pdf = todo ()

(* ID entry of the <Note> tag is not present. *)
let matterhorn_19_003 pdf = todo ()

(* ID entry of the <Note> tag is non-unique. *)
let matterhorn_19_004 pdf = todo ()

(* Name entry is missing or has an empty string as its value in an Optional
   Content Configuration Dictionary in the Configs entry in the OCProperties
   entry in the Catalog dictionary. *)
let matterhorn_20_001 pdf = todo ()

(* Name entry is missing or has an empty string as its value in an Optional
   Content Configuration Dictionary that is the value of the D entry in the
   OCProperties entry in the Catalog dictionary. *)
let matterhorn_20_002 pdf = todo ()

(* An AS entry appears in an Optional Content Configuration Dictionary. *)
let matterhorn_20_003 pdf = todo ()

(* The file specification dictionary for an embedded file does not contain F
   and UF entries. *)
let matterhorn_21_001 pdf =
  let from_nametree =
    match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
    | Some catalog ->
        begin match Pdf.lookup_direct pdf "/Names" catalog with
        | Some names ->
            begin match Pdf.lookup_direct pdf "/EmbeddedFiles" names with
            | Some embeddedfiles -> 
                map snd (Pdf.contents_of_nametree pdf embeddedfiles)
            | None -> []
            end
        | None -> []
        end
    | None -> []
  in
  let from_annots =
    option_map
      (fun x ->
         if x.Pdfannot.subtype = Pdfannot.FileAttachment
           then Pdf.lookup_direct pdf "/FS" x.Pdfannot.annotrest
           else None)
      (flatten (map (Pdfannot.annotations_of_page pdf) (Pdfpage.pages_of_pagetree pdf)))
  in
  if
    List.exists
      (fun x ->
         match Pdf.lookup_direct pdf "/F" x, Pdf.lookup_direct pdf "/UF" x with
         | Some _, Some _ -> false
         | _ -> true)
      (from_nametree @ from_annots)
  then
    merror ()

(* File contains the dynamicRender element with value “required”. *)
let matterhorn_25_001 pdf =
  let rec contains_required_dynamicRender = function
    | Cpdfmetadata.E (((_, "dynamicRender"), _), [Cpdfmetadata.D "required"]) -> true
    | Cpdfmetadata.E (_, children) -> List.exists contains_required_dynamicRender children
    | Cpdfmetadata.D _ -> false
  in
    match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
    | Some catalog ->
        begin match Pdf.lookup_direct pdf "/AcroForm" catalog with
        | Some d ->
            begin match Pdf.lookup_direct pdf "/XFA" d with
            | Some (Pdf.Array xfa) ->
                begin match option_map (function (Pdf.String "config", x) -> Some x | _ -> None) (pairs xfa) with
                | [config] ->
                    begin match Pdf.direct pdf config with
                    | Pdf.Stream _ as s ->
                      Pdfcodec.decode_pdfstream pdf s;
                      begin match s with
                      | Pdf.Stream {contents = _, Pdf.Got xmlstream} ->
                          let _, tree = Cpdfmetadata.xmltree_of_bytes xmlstream in
                            if contains_required_dynamicRender tree then merror ()
                      | _ -> assert false
                      end
                    | _ -> ()
                    end
                | _ -> ()
                end
            | _ -> ()
            end
        | _ -> ()
        end
    | None -> ()

(* The file is encrypted but does not contain a P entry in its encryption
   dictionary. *)
let matterhorn_26_001 pdf = ()
  (* Would already have failed at this point, because CamlPDF does not allow
  the decryption of a file with no /P *)

(* The file is encrypted and does contain a P entry but the 10th bit position
   of the P entry is false. *)
let matterhorn_26_002 pdf =
  match pdf.Pdf.saved_encryption with
  | None -> ()
  | Some {Pdf.from_get_encryption_values = (_, _, _, p, _, _, _)} ->
      if mem Pdfcrypt.NoExtract (Pdfcrypt.banlist_of_p p) then merror ()

(* An annotation, other than of subtype Widget, Link and PrinterMark, is not a
   direct child of an <Annot> structure element. *)
let matterhorn_28_002 pdf = todo ()

(* An annotation, other than of subtype Widget, does not have a Contents entry
   and does not have an alternative description (in the form of an Alt entry in
   the enclosing structure element). *)
let matterhorn_28_004 pdf = todo ()

(* A form field does not have a TU entry and does not have an alternative
   description (in the form of an Alt entry in the enclosing structure
   element). *)
let matterhorn_28_005 pdf = todo ()

(* An annotation with subtype undefined in ISO 32000 does not meet 7.18.1. *)
let matterhorn_28_006 pdf =
  if
    List.exists
      (fun x -> match x.Pdfannot.subtype with Pdfannot.Unknown _ -> true | _ -> false)
      (flatten (map (Pdfannot.annotations_of_page pdf) (Pdfpage.pages_of_pagetree pdf)))
  then
    merror ()

(* An annotation of subtype TrapNet exists. *)
let matterhorn_28_007 pdf =
  if
    List.exists
      (fun x -> x.Pdfannot.subtype = Pdfannot.TrapNet)
      (flatten (map (Pdfannot.annotations_of_page pdf) (Pdfpage.pages_of_pagetree pdf)))
  then
    merror ()

(* A page containing an annotation does not contain a Tabs entry *)
let matterhorn_28_008 pdf =
  if
    List.exists
      (fun p ->
         Pdfannot.annotations_of_page pdf p <> [] && Pdf.lookup_direct pdf "/Tabs" p.Pdfpage.rest = None)
      (Pdfpage.pages_of_pagetree pdf)
  then
    merror ()

(* A page containing an annotation has a Tabs entry with a value other than S.
 *)
let matterhorn_28_009 pdf =
  if
    List.exists
      (fun p ->
         Pdfannot.annotations_of_page pdf p <> [] &&
         Pdf.lookup_direct pdf "/Tabs" p.Pdfpage.rest <> None && (* already covered by 28_008 above. *)
         Pdf.lookup_direct pdf "/Tabs" p.Pdfpage.rest <> Some (Pdf.Name "/S"))
      (Pdfpage.pages_of_pagetree pdf)
  then
    merror ()

(* A widget annotation is not nested within a <Form> tag. *)
let matterhorn_28_010 pdf = todo ()

(* A link annotation is not nested within a <Link> tag. *)
let matterhorn_28_011 pdf = todo ()

(* A link annotation does not include an alternate description in its Contents
   entry. *)
let matterhorn_28_012 pdf =
  if
    List.exists
      (fun x -> x.Pdfannot.subtype = Pdfannot.Link && x.Pdfannot.annot_contents = None )
      (flatten (map (Pdfannot.annotations_of_page pdf) (Pdfpage.pages_of_pagetree pdf)))
  then
    merror ()

(* CT entry is missing from the media clip data dictionary. *)
let matterhorn_28_014 pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Type" o, Pdf.lookup_direct pdf "/S" o, Pdf.lookup_direct pdf "/CT" o with
       | Some (Pdf.Name "/MediaClip"), Some (Pdf.Name "/MCD"), None -> merror ()
       | _ -> ())
    pdf

(* Alt entry is missing from the media clip data dictionary. *)
let matterhorn_28_015 pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Type" o, Pdf.lookup_direct pdf "/S" o, Pdf.lookup_direct pdf "/CT" o with
       | Some (Pdf.Name "/MediaClip"), Some (Pdf.Name "/MCD"), None -> merror ()
       | _ -> ())
    pdf

(* File attachment annotations do not conform to 7.11. *)
let matterhorn_28_016 pdf =
  covered_elsewhere ()

(* A PrinterMark annotation is included in the logical structure. *)
let matterhorn_28_017 pdf = todo ()

(* The appearance stream of a PrinterMark annotation is not marked as Artifact.
 *)
let matterhorn_28_018 pdf = todo ()

(* A reference XObject is present. *)
let matterhorn_30_001 pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Subtype" o, Pdf.lookup_direct pdf "/Ref" o with
       | Some (Pdf.Name "/Form"), Some _ -> merror ()
       | _ -> ())
    pdf

(* Form XObject contains MCIDs and is referenced more than once. *)
let matterhorn_30_002 pdf =
  (* We need to consider inheritence here. What solutions do we already have
     for that, and do we need anything new? *)
  unimpl ()

(* A Type 0 font dictionary with encoding other than Identity-H and Identity-V
   has values for Registry in both CIDSystemInfo dictionaries that are not
   identical. *)
let matterhorn_31_001 pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Subtype" o, Pdf.lookup_direct pdf "/Encoding" o with
       | Some (Pdf.Name "/Type0"), Some (Pdf.Name ("/Identity-H" | "/Identity-V")) -> ()
       | Some (Pdf.Name "/Type0"), _ ->
           merror_str
             "Advisory: contains composite font with non-identity encoding. Cpdf\
              cannot check the CIDSystemInfo entries are identical automatically."
       | _ -> ())
    pdf

(* A Type 0 font dictionary with encoding other than Identity-H and Identity-V
   has values for Ordering in both CIDSystemInfo dictionaries that are not
   identical. *)
let matterhorn_31_002 pdf =
  matterhorn_31_001 pdf

(* A Type 0 font dictionary with encoding other than Identity-H and Identity-V
   has a value for Supplement in the CIDSystemInfo dictionary of the CID font
   that is less than the value for Supplement in the CIDSystemInfo dictionary
   of the CMap. *)
let matterhorn_31_003 pdf =
  matterhorn_31_001 pdf

(* A Type 2 CID font contains neither a stream nor the name Identity as the
   value of the CIDToGIDMap entry. *)
let matterhorn_31_004 pdf =
  Pdf.objiter
    (fun _ n ->
       match Pdf.lookup_direct pdf "/Subtype" n with
       | Some (Pdf.Name "/CIDFontType2") ->
           begin match Pdf.lookup_direct pdf "/CIDtoGIDMap" n with
           | Some (Pdf.Name "/Identity" | Pdf.Stream _) -> ()
           | _ -> merror ()
           end
       | _ -> ())
    pdf

(* A Type 2 CID font does not contain a CIDToGIDMap entry. *)
let matterhorn_31_005 pdf =
  Pdf.objiter
    (fun _ n ->
       match Pdf.lookup_direct pdf "/Subtype" n with
       | Some (Pdf.Name "/CIDFontType2") ->
           begin match Pdf.lookup_direct pdf "/CIDtoGIDMap" n with
           | Some _ -> ()
           | _ -> merror ()
           end
       | _ -> ())
    pdf

(* A CMap is neither listed as described in ISO 32000-1:2008, 9.7.5.2, Table
   118 nor is it embedded. *)
let cmap_names =
  ["/GB-EUC-H";
   "/GB-EUC-V";
   "/GBpc-EUC-H";
   "/GBpc-EUC-V";
   "/GBK-EUC-H";
   "/GBK-EUC-V";
   "/GBKp-EUC-H";
   "/GBKp-EUC-V";
   "/GBK2K-H";
   "/GBK2K-V";
   "/UniGB-UCS32-H";
   "/UniGB-UCS32-V";
   "/UniGB-UTF16-H";
   "/UniGB-UTF16-V";
   "/B5pc-H";
   "/B5pc-V";
   "/HKscs-B5-H";
   "/HKscs-B5-V";
   "/ETen-B5-H";
   "/ETen-B5-V";
   "/ETenms-B5-H";
   "/ETenms-B5-V";
   "/CNS-EUC-H";
   "/CNS-EUC-V";
   "/UniCNS-UCS2-H";
   "/UniCNS-UCS2-V";
   "/UniCNS-UTF16-H";
   "/UniCNS-UTF16-V";
   "/83pv-RKSJ-H";
   "/90ms-RKSJ-H";
   "/90ms-RKSJ-V";
   "/90msp-RKSJ-H";
   "/90msp-RKSJ-V";
   "/90pv-RKSJ-H";
   "/Add-RKSJ-H";
   "/Add-RKSJ-V";
   "/EUC-H";
   "/EUC-V";
   "/Ext-RKSJ-H";
   "/Ext-RKSJ-V";
   "/H";
   "/V";
   "/UniJIS-UCS2-H";
   "/UniJIS-UCS2-V";
   "/UniJIS-UCS2-HW-H";
   "/UniJIS-UCS2-HW-V";
   "/UniJIS-UTF16-H";
   "/UniJIS-UTF16-V";
   "/KSC-EUC-H";
   "/KSC-EUC-V";
   "/KSCms-UHC-H";
   "/KSCms-UHC-V";
   "/KSCms-UHC-HW-H";
   "/KSCms-UHS-HW-V";
   "/KSCpc-EUC-H";
   "/UniKS-UCS2-H";
   "/UniKS-UCS2-V";
   "/Identity-H";
   "/Identity-V"]

let matterhorn_31_006 pdf =
  Pdf.objiter
    (fun _ o ->
     match Pdf.lookup_direct pdf "/Subtype" o with
     | Some (Pdf.Name "/Type0") ->
         begin match Pdf.lookup_direct pdf "/Encoding" o with
         | Some (Pdf.Name n) when not (List.mem n cmap_names) -> merror_str n
         | _ -> ()
         end
     | _ -> ())
    pdf

(* The WMode entry in a CMap dictionary is not identical to the WMode value in
the CMap stream. *)
let matterhorn_31_007 pdf =
  unimpl ()

(* A CMap references another CMap which is not listed in ISO 32000-1:2008,
   9.7.5.2, Table 118. *)
let matterhorn_31_008 pdf =
  unimpl ()

(* For a font used by text intended to be rendered the font program is not
   embedded. *)
(* NB This, for now, reports all unembedded fonts. *)
let matterhorn_31_009 pdf =
  let l = Cpdffont.missing_fonts_return pdf (ilist 1 (Pdfpage.endpage pdf)) in
    if l <> [] then
      raise (MatterhornError (`List (map (fun x -> `String x) l)))

(* For a font used by text the font program is embedded but it does not contain
   glyphs for all of the glyphs referenced by the text used for rendering. *)
let matterhorn_31_011 pdf =
  unimpl ()

(* The FontDescriptor dictionary of an embedded Type 1 font contains a CharSet
   string, but at least one of the glyphs present in the font program is not
   listed in the CharSet string. *)
let matterhorn_31_012 pdf =
  unimpl ()

(* The FontDescriptor dictionary of an embedded Type 1 font contains a CharSet
   string, but at least one of the glyphs listed in the CharSet string is not
   present in the font program. *)
let matterhorn_31_013 pdf =
  unimpl ()

(* The FontDescriptor dictionary of an embedded CID font contains a CIDSet
   string, but at least one of the glyphs present in the font program is not
   listed in the CIDSet string. *)
let matterhorn_31_014 pdf =
  unimpl ()

(* The FontDescriptor dictionary of an embedded CID font contains a CIDSet
   string, but at least one of the glyphs listed in the CIDSet string is not
   present in the font program. *)
let matterhorn_31_015 pdf =
  unimpl ()

(* For one or more glyphs, the glyph width information in the font dictionary
   and in the embedded font program differ by more than 1/1000 unit. *)
let matterhorn_31_016 pdf =
  unimpl ()

(* A non-symbolic TrueType font is used for rendering, but none of the cmap
   entries in the embedded font program is a non-symbolic cmap. *)
let matterhorn_31_017 pdf =
  unimpl ()

(* A non-symbolic TrueType font is used for rendering, but for at least one
   glyph to be rendered the glyph cannot be looked up by any of the
   non-symbolic cmap entries in the embedded font program. *)
let matterhorn_31_018 pdf =
  unimpl ()

(* The font dictionary for a non-symbolic TrueType font does not contain an
   Encoding entry. *)
let is_non_symbolic pdf o =
  match Pdf.lookup_direct pdf "/FontDescriptor" o with
  | Some fd ->
      begin match Pdf.lookup_direct pdf "/Flags" fd with
      | Some (Pdf.Integer i) -> not (i land 0b100 > 0)
      | _ -> true
      end
  | None -> true 

let matterhorn_31_019 pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Subtype" o with
       | Some (Pdf.Name "/TrueType") ->
           begin match is_non_symbolic pdf o, Pdf.lookup_direct pdf "/Encoding" o with
           | true, None -> merror ()
           | _ -> ()
           end
       | _ -> ())
    pdf

(* The font dictionary for a non-symbolic TrueType font contains an Encoding
   dictionary which does not contain a BaseEncoding entry. *)
let matterhorn_31_020 pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Subtype" o with
       | Some (Pdf.Name "/TrueType") ->
           begin match is_non_symbolic pdf o, Pdf.lookup_direct pdf "/Encoding" o with
           | true, Some ((Pdf.Dictionary _) as d) ->
               begin match Pdf.lookup_direct pdf "/BaseEncoding" d with
               | None -> merror ()
               | Some _ -> ()
               end
           | _ -> ()
           end
       | _ -> ())
    pdf

(* The value for either the Encoding entry or the BaseEncoding entry in the
   Encoding dictionary in a non-symbolic TrueType font dictionary is neither
   MacRomanEncoding nor WinAnsiEncoding. *)
let matterhorn_31_021 pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Subtype" o with
       | Some (Pdf.Name "/TrueType") ->
           begin match is_non_symbolic pdf o, Pdf.lookup_direct pdf "/Encoding" o with
           | true, Some (Pdf.Name ("/MacRomanEncoding" | "/WinAnsiEncoding")) -> ()
           | true, Some (Pdf.Name _) -> merror ()
           | true, Some ((Pdf.Dictionary _) as d) ->
               begin match Pdf.lookup_direct pdf "/BaseEncoding" d with
               | Some (Pdf.Name ("/MacRomanEncoding" | "/WinAnsiEncoding")) -> ()
               | Some _ -> merror ()
               | _ -> ()
               end
           | _ -> ()
           end
       | _ -> ())
    pdf

(* The Differences array in the Encoding entry in a non-symbolic TrueType font
   dictionary contains one or more glyph names which are not listed in the
   Adobe Glyph List. *)
let matterhorn_31_022 pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Subtype" o with
       | Some (Pdf.Name "/TrueType") ->
           begin match is_non_symbolic pdf o, Pdf.lookup_direct pdf "/Encoding" o with
           | true, Some d ->
               begin match Pdf.lookup_direct pdf "/Differences" d with
               | Some (Pdf.Array a) ->
                   let glyphs = Pdfglyphlist.glyph_hashes () in
                   let names = option_map (function Pdf.Name n -> Some n | _ -> None) a in
                     if not (List.for_all (Hashtbl.mem glyphs) names) then merror ()
               | _ -> ()
               end
           | _ -> ()
           end
       | _ -> ())
    pdf

(* The Differences array is present in the Encoding entry in a non-symbolic
   TrueType font dictionary but the embedded font program does not contain a
   (3,1) Microsoft Unicode cmap. *)
let matterhorn_31_023 pdf =
  unimpl ()

(* The Encoding entry is present in the font dictionary for a symbolic TrueType
   font. *)
let matterhorn_31_024 pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Subtype" o with
       | Some (Pdf.Name "/TrueType") ->
           begin match is_non_symbolic pdf o, Pdf.lookup_direct pdf "/Encoding" o with
           | false, Some _ -> merror ()
           | _ -> ()
           end
       | _ -> ())
    pdf

(* The embedded font program for a symbolic TrueType font contains no cmap. *)
let matterhorn_31_025 pdf =
  unimpl ()

(* The embedded font program for a symbolic TrueType font contains more than
   one cmap, but none of the cmap entries is a (3,0) Microsoft Symbol cmap. *)
let matterhorn_31_026 pdf =
  unimpl ()

(* A font dictionary does not contain the ToUnicode entry and none of the
   following is true: the font uses MacRomanEncoding, MacExpertEncoding or
   WinAnsiEncoding; the font is a Type 1 or Type 3 font and the glyph names of
   the glyphs referenced are all contained in the Adobe Glyph List or the set
   of named characters in the Symbol font, as defined in ISO 32000-1:2008,
   Annex D; the font is a Type 0 font, and its descendant CIDFont uses
   Adobe-GB1, Adobe-CNS1, Adobe-Japan1 or Adobe-Korea1 character collections;
   the font is a non-symbolic TrueType font. *)
let matterhorn_31_027 pdf =
  not_fully_implemented ();
  (* Here, we implement most of this one, but can't check the set of referenced
     glyphs for Type1 / Type3. *)
  let c1 o =
    match Pdf.lookup_direct pdf "/Encoding" o with
    | Some (Pdf.Name ("/MacRomanEncoding" | "/MacExpertEncoding" | "/WinAnsiEncoding")) -> true
    | _ -> false
  in
  let c3 o =
    match Pdf.lookup_direct pdf "/Subtype" o with
    | Some (Pdf.Name "/Type0") ->
        begin match Pdf.lookup_direct pdf "/DescendantFonts" o with
        | Some (Pdf.Array [df]) ->
            begin match Pdf.lookup_direct pdf "/CIDSystemInfo" df with
            | Some cidinfo ->
                begin match Pdf.lookup_direct pdf "/Registry" cidinfo, Pdf.lookup_direct pdf "/Ordering" cidinfo with
                | Some (Pdf.Name "/Adobe"), Some (Pdf.Name ("/GB1" | "/CNS1" | "/Japan1" | "/Korea1")) -> true
                | _ -> false
                end
            | _ -> false
            end
        | _ -> false
        end
    | _ -> false
  in
  let c4 o = is_non_symbolic pdf o in
    Pdf.objiter
      (fun _ o ->
         match Pdf.lookup_direct pdf "/ToUnicode" o with
         | Some _ -> ()
         | None ->
             begin match Pdf.lookup_direct pdf "/Type" o with
             | Some (Pdf.Name "/Font") ->
                 begin match c1 o, c3 o, c4 o with
                 | false, false, false ->
                     begin match Pdf.lookup_direct pdf "/Subtype" o with
                     | Some (Pdf.Name ("/Type1" | "/Type3")) -> unimpl ()
                     | _ -> merror ()
                     end
                 | _ -> ()
                 end
             | _ -> ()
             end)
      pdf

let all_tounicodes pdf = 
  let tus = ref [] in
    Pdf.objiter
      (fun _ o ->
        match Pdf.indirect_number pdf "/ToUnicode" o with
        | Some i -> tus := i::!tus
        | None -> ())
      pdf;
      (setify !tus)

let check_unicode tu n =
  mem n (flatten (map (fun x -> Pdftext.codepoints_of_utf16be (snd x)) tu))

(* One or more Unicode values specified in the ToUnicode CMap are zero (0). *)
let matterhorn_31_028 pdf =
  iter
    (fun i ->
      let tu = Pdftext.parse_tounicode pdf (Pdf.lookup_obj pdf i) in
        if check_unicode tu 0 then merror ())
    (all_tounicodes pdf)

(* One or more Unicode values specified in the ToUnicode CMap are equal to
   either U+FEFF or U+FFFE. *)
let matterhorn_31_029 pdf =
  iter
    (fun i ->
      let tu = Pdftext.parse_tounicode pdf (Pdf.lookup_obj pdf i) in
        if check_unicode tu 0xFEFF || check_unicode tu 0xFFFE then merror ())
    (all_tounicodes pdf)

(* One or more characters used in text showing operators reference the .notdef
   glyph. *)
let matterhorn_31_030 pdf =
  unimpl ()

let matterhorn =
  [("01-003", "Content marked as Artifact is present inside tagged content.", "UA1:7.1-1", matterhorn_01_003);
   ("01-004", "Tagged content is present inside content marked as Artifact.", "UA1:7.1-1", matterhorn_01_004);
   ("01-005", "Content is neither marked as Artifact nor tagged as real content.", "UA1:7-1-2", matterhorn_01_005);
   ("01-007", "Suspects entry has a value of true.", "UA1:7-1-11", matterhorn_01_007);
   ("02-001", "One or more non-standard tag’s mapping does not terminate with a standard type.", "UA1:7.1-3", matterhorn_02_001);
   ("02-002", "A circular mapping exists.", "UA1:7.1-3", matterhorn_02_003);
   ("02-003", "One or more standard types are remapped.", "UA1:7.1-4", matterhorn_02_004);
   ("06-001", "Document does not contain an XMP metadata stream", "UA1:7.1-8", matterhorn_06_001);
   ("06-002", "The XMP metadata stream in the Catalog dictionary does not include the PDF/UA identifier.", "UA1:5", matterhorn_06_002);
   ("06-003", "XMP metadata stream does not contain dc:title", "UA1:7.1-8", matterhorn_06_003);
   ("07-001", "ViewerPreferences dictionary of the Catalog dictionary does not contain a DisplayDocTitle entry", "UA1:7.1-9", matterhorn_07_001);
   ("07-002", "ViewerPreferences dictionary of the Catalog dictionary contains a DisplayDocTitle entry with a value of false", "UA1:7.1-9", matterhorn_07_002);
   ("09-004", "A table-related structure element is used in a way that does not conform to the syntax defined in ISO 32000-1, Table 337.", "UA1-7.2-1", matterhorn_09_004);
   ("09-005", "A list-related structure element is used in a way that does not conform to Table 336 in ISO 32000-1.", "UA1-7.2-1", matterhorn_09_005);
   ("09-006", "A TOC-related structure element is used in a way that does not conform to Table 333 in ISO 32000-1.", "UA1-7.2-1", matterhorn_09_006);
   ("09-007", "A Ruby-related structure element is used in a way that does not conform to Table 338 in ISO 32000-1.", "UA1-7.2-1", matterhorn_09_007);
   ("09-008", "A Warichu-related structure element is used in a way that does not conform to Table 338 in ISO 32000-1.", "UA1-7.2-1", matterhorn_09_008);
   ("10-001", "Character code cannot be mapped to Unicode.", "UA1:7.2-2", matterhorn_10_001);
   ("11-001", "Natural language for text in page content cannot be determined.", "UA1:7.2-3", matterhorn_11_001);
   ("11-002", "Natural language for text in Alt, ActualText and E attributes cannot be determined.", "UA1:7.2-3", matterhorn_11_002);
   ("11-003", "Natural language in the Outline entries cannot be determined.", "UA1:7.2-3", matterhorn_11_003);
   ("11-004", "Natural language in the Contents entry for annotations cannot be determined.", "UA1:7.2-3", matterhorn_11_004);
   ("11-005", "Natural language in the TU entry for form fields cannot be determined.", "UA1:7.2-3", matterhorn_11_005);
   ("11-006", "Natural language for document metadata cannot be determined.", "UA1:7.2-3", matterhorn_11_006);
   ("13-004", "<Figure> tag alternative or replacement text missing.", "UA1:7.3-3", matterhorn_13_004);
   ("14-002", "Does use numbered headings, but the first heading tag is not <H1>.", "UA1:7.4.2-1", matterhorn_14_002);
   ("14-003", "Numbered heading levels in descending sequence are skipped (Example: <H3> follows directly after <H1>).", "UA1:7.4-1", matterhorn_14_003);
   ("14-006", "A node contains more than one <H> tag.", "UA1:7.4.4-1", matterhorn_14_006);
   ("14-007", "Document uses both <H> and <H#> tags.", "UA1:7.4.4-3", matterhorn_14_007);
   ("15-003", "In a table not organized with Headers attributes and IDs, a <TH> cell does not contain a Scope attribute.", "UA1:7.5-2", matterhorn_15_003);
   ("17-002", "<Formula> tag is missing an Alt attribute.", "UA1:7.7-1", matterhorn_17_002);
   ("17-003", "Unicode mapping requirements are not met.", "UA1:7.7-2", matterhorn_17_003);
   ("19-003", "ID entry of the <Note> tag is not present.", "UA1:7.9-2", matterhorn_19_003);
   ("19-004", "ID entry of the <Note> tag is non-unique.", "UA1:7.9-2", matterhorn_19_004);
   ("20-001", "Name entry is missing or has an empty string as its value in an Optional Content Configuration Dictionary in the Configs entry in the OCProperties entry in the Catalog dictionary.", "UA1:7.10-1", matterhorn_20_001);
   ("20-002", "Name entry is missing or has an empty string as its value in an Optional Content Configuration Dictionary that is the value of the D entry in the OCProperties entry in the Catalog dictionary.", "UA1:7.10-1", matterhorn_20_002);
   ("20-003", "An AS entry appears in an Optional Content Configuration Dictionary.", "UA1:7.10-2", matterhorn_20_003);
   ("21-001", "The file specification dictionary for an embedded file does not contain F and UF entries.", "UA1:7.11-1", matterhorn_21_001);
   ("25-001", "File contains the dynamicRender element with value “required”.", "UA1:7.15-1", matterhorn_25_001);
   ("26-001", "The file is encrypted but does not contain a P entry in its encryption dictionary.", "UA1:7.16-1", matterhorn_26_001);
   ("26-002", "The file is encrypted and does contain a P entry but the 10th bit position of the P entry is false.", "UA1:7.16-1", matterhorn_26_002);
   ("28-002", "An annotation, other than of subtype Widget, Link and PrinterMark, is not a direct child of an <Annot> structure element.", "UA1:7.18.1-2", matterhorn_28_002);
   ("28-004", "An annotation, other than of subtype Widget, does not have a Contents entry and does not have an alternative description (in the form of an Alt entry in the enclosing structure element).", "UA1:7.18.1-4", matterhorn_28_004);
   ("28-005", "A form field does not have a TU entry and does not have an alternative description (in the form of an Alt entry in the enclosing structure element).", "UA1:7.18.1-4", matterhorn_28_005);
   ("28-006", "An annotation with subtype undefined in ISO 32000 does not meet 7.18.1.", "UA1:7.18.2-1", matterhorn_28_006);
   ("28-007", "An annotation of subtype TrapNet exists.", "UA1:7.18.2-2", matterhorn_28_007);
   ("28-008", "A page containing an annotation does not contain a Tabs entry", "UA1:7.18.3-1", matterhorn_28_008);
   ("28-009", "A page containing an annotation has a Tabs entry with a value other than S.", "UA1:7.18.3-1", matterhorn_28_009);
   ("28-010", "A widget annotation is not nested within a <Form> tag.", "UA1:7.18.4-1", matterhorn_28_010);
   ("28-011", "A link annotation is not nested within a <Link> tag.", "UA1:7.18.5-1", matterhorn_28_011);
   ("28-012", "A link annotation does not include an alternate description in its Contents entry.", "UA1:7.18.5-2", matterhorn_28_012);
   ("28-014", "CT entry is missing from the media clip data dictionary.", "", matterhorn_28_014);
   ("28-015", "Alt entry is missing from the media clip data dictionary.", "UA1:7.18.6.2-1", matterhorn_28_015);
   ("28-016", "File attachment annotations do not conform to 7.11.", "UA1:7.18.7-1", matterhorn_28_016);
   ("28-017", "A PrinterMark annotation is included in the logical structure.", "UA1:7.18.8-1", matterhorn_28_017);
   ("28-018", "The appearance stream of a PrinterMark annotation is not marked as Artifact.", "UA1:7.18.8-2", matterhorn_28_018);
   ("30-001", "A reference XObject is present.", "UA1:7.2", matterhorn_30_001);
   ("30-002", "Form XObject contains MCIDs and is referenced more than once.", "UA1:7.21.3.1-1",  matterhorn_30_002);
   ("31-001", "A Type 0 font dictionary with encoding other than Identity-H and Identity-V has values for Registry in both CIDSystemInfo dictionaries that are not identical.", "UA1:7.21.3-1", matterhorn_31_001);
   ("31-002", "A Type 0 font dictionary with encoding other than Identity-H and Identity-V has values for Ordering in both CIDSystemInfo dictionaries that are not identical.", "UA1:7.21.3.1-1", matterhorn_31_002);
   ("31-003", "A Type 0 font dictionary with encoding other than Identity-H and Identity-V has a value for Supplement in the CIDSystemInfo dictionary of the CID font that is less than the value for Supplement in the CIDSystemInfo dictionary of the CMap.", "UA1:7.21.3.1-1", matterhorn_31_003);
   ("31-004", "A Type 2 CID font contains neither a stream nor the name Identity as the value of the CIDToGIDMap entry.", "UA1:7.21.3.2-1", matterhorn_31_004);
   ("31-005", "A Type 2 CID font does not contain a CIDToGIDMap entry.", "UA1:7.21.3.2-1", matterhorn_31_005);
   ("31-006", "A CMap is neither listed as described in ISO 32000- 1:2008, 9.7.5.2, Table 118 nor is it embedded.", "UA1:7.21.3.3-1", matterhorn_31_006);
   ("31-007", "The WMode entry in a CMap dictionary is not identical to the WMode value in the CMap stream.", "UA1:7.21.3.3-1", matterhorn_31_007);
   ("31-008", "A CMap references another CMap which is not listed in ISO 32000-1:2008, 9.7.5.2, Table 118.", "UA1:7.21.3.3-2", matterhorn_31_008);
   ("31-009", "For a font used by text intended to be rendered the font program is not embedded.", "UA1:7.21.4.1-1", matterhorn_31_009);
   ("31-011", "For a font used by text the font program is embedded but it does not contain glyphs for all of the glyphs referenced by the text used for rendering.", "UA1:7.21.4.1-3", matterhorn_31_011);
   ("31-012", "The FontDescriptor dictionary of an embedded Type 1 font contains a CharSet string, but at least one of the glyphs present in the font program is not listed in the CharSet string.", "UA1:7.21.4.2-1", matterhorn_31_012);
   ("31-013", "The FontDescriptor dictionary of an embedded Type 1 font contains a CharSet string, but at least one of the glyphs listed in the CharSet string is not present in the font program.", "UA1:7.21.4.2-2", matterhorn_31_013);
   ("31-014", "The FontDescriptor dictionary of an embedded CID font contains a CIDSet string, but at least one of the glyphs present in the font program is not listed in the CIDSet string.", "UA1:7.21.4.2-3", matterhorn_31_014);
   ("31-015", "The FontDescriptor dictionary of an embedded CID font contains a CIDSet string, but at least one of the glyphs listed in the CIDSet string is not present in the font program.", "UA1:7.21.4.2-4", matterhorn_31_015);
   ("31-016", "For one or more glyphs, the glyph width information in the font dictionary and in the embedded font program differ by more than 1/1000 unit.", "UA1:7.21.5-1", matterhorn_31_016);
   ("31-017", "A non-symbolic TrueType font is used for rendering, but none of the cmap entries in the embedded font program is a non-symbolic cmap.", "UA1:7.21.6-1", matterhorn_31_017);
   ("31-018", "A non-symbolic TrueType font is used for rendering, but for at least one glyph to be rendered the glyph cannot be looked up by any of the non-symbolic cmap entries in the embedded font program.", "UA1:7.21.6-2", matterhorn_31_018);
   ("31-019", "The font dictionary for a non-symbolic TrueType font does not contain an Encoding entry.", "UA1:7.21.6-3", matterhorn_31_019);
   ("31-020", "The font dictionary for a non-symbolic TrueType font contains an Encoding dictionary which does not contain a BaseEncoding entry.", "UA1:7.21.6-4", matterhorn_31_020);
   ("31-021", "The value for either the Encoding entry or the BaseEncoding entry in the Encoding dictionary in a non-symbolic TrueType font dictionary is neither MacRomanEncoding nor WinAnsiEncoding.", "UA1:7.21.6-5", matterhorn_31_021);
   ("31-022", "The Differences array in the Encoding entry in a non-symbolic TrueType font dictionary contains one or more glyph names which are not listed in the Adobe Glyph List.", "UA1:7.21.6-6", matterhorn_31_022);
   ("31-023", "The Differences array is present in the Encoding entry in a non-symbolic TrueType font dictionary but the embedded font program does not contain a (3,1) Microsoft Unicode cmap.", "UA1:7.21.6-7", matterhorn_31_023);
   ("31-024", "The Encoding entry is present in the font dictionary for a symbolic TrueType font.", "UA1:7.21.6-8", matterhorn_31_024);
   ("31-025", "The embedded font program for a symbolic TrueType font contains no cmap.", "UA1:7.21.6-9", matterhorn_31_025);
   ("31-026", "The embedded font program for a symbolic TrueType font contains more than one cmap, but none of the cmap entries is a (3,0) Microsoft Symbol cmap.", "UA1:7.21.6-10", matterhorn_31_026);
   ("31-027", "A font dictionary does not contain the ToUnicode entry and none of the following is true: the font uses MacRomanEncoding, MacExpertEncoding or WinAnsiEncoding; the font is a Type 1 or Type 3 font and the glyph names of the glyphs referenced are all contained in the Adobe Glyph List or the set of named characters in the Symbol font, as defined in ISO 32000-1:2008, Annex D; the font is a Type 0 font, and its descendant CIDFont uses Adobe-GB1, Adobe-CNS1, Adobe-Japan1 or Adobe-Korea1 character collections; the font is a non-symbolic TrueType font.", "UA1:7.21.7-1", matterhorn_31_027);
   ("31-028", "One or more Unicode values specified in the ToUnicode CMap are zero (0).", "UA1:7.21.7-2", matterhorn_31_028);
   ("31-029", "One or more Unicode values specified in the ToUnicode CMap are equal to either U+FEFF or U+FFFE.", "UA1:7.21.7-3", matterhorn_31_029);
   ("31-030", "One or more characters used in text showing operators reference the .notdef glyph.", "UA1:7.21.8-1", matterhorn_31_030);
  ]

let test_matterhorn pdf =
  option_map
    (fun (name, error, section, test) ->
      try test pdf; None with
       | MatterhornError extra -> Some (name, error, section, extra)
       | MatterhornUnimplemented -> None
       | e -> Some (name, "Incomplete", section, `String ("ERROR: " ^ Printexc.to_string e)))
    matterhorn

let test_matterhorn_print pdf =
  iter
    (fun (name, error, section, extra) ->
       Printf.eprintf "%s %s %s %s\n" name section error
       (if extra = `Null then "" else "(" ^ Cpdfyojson.Safe.to_string extra ^ ")"))
    (test_matterhorn pdf)

let test_matterhorn_json pdf =
  `List
    (map
      (fun (name, error, section, extra) ->
        `Assoc [("name", `String name); ("section", `String section); ("error", `String error); ("extra", extra)])
      (test_matterhorn pdf))

let pdfua_marker =
  Cpdfmetadata.(E (((rdf, "Description"), [((rdf, "about"), ""); ((Cpdfxmlm.ns_xmlns, "pdfuaid"), pdfuaid)]), [E (((pdfuaid, "part"), []), [D "1"])])) 

let rec insert_as_rdf_description fragment = function
  | Cpdfmetadata.E (((_, "RDF"), _) as rdftag, rdfs) ->
      Cpdfmetadata.E (rdftag, fragment::rdfs)
  | Cpdfmetadata.E (((_, "xmpmeta"), _) as xmptag, cs) ->
      Cpdfmetadata.E (xmptag, map (insert_as_rdf_description fragment) cs)
  | _ -> error "insert_as_rdf_description: could not find insertion point."

let rec delete_pdfua_marker tree =
  let is_pdfuaid = function
  | Cpdfmetadata.E (((pdfuaid, "part"), _), _) when pdfuaid = Cpdfmetadata.pdfuaid -> true
  | _ -> false
  in
    match tree with
    | Cpdfmetadata.E (((rdf, "Description"), _), c) when rdf = Cpdfmetadata.rdf && List.exists is_pdfuaid c ->
        Cpdfmetadata.D ""
    | Cpdfmetadata.E (x, children) ->
        Cpdfmetadata.E (x, map delete_pdfua_marker children)
    | x -> x

let mark pdf =
  let pdf2 = if Cpdfmetadata.get_metadata pdf = None then Cpdfmetadata.create_metadata pdf else pdf in
   pdf.Pdf.objects <- pdf2.Pdf.objects;
   pdf.Pdf.trailerdict <- pdf2.Pdf.trailerdict;
   pdf.Pdf.root <- pdf2.Pdf.root;
   match Cpdfmetadata.get_metadata pdf with
   | Some metadata ->
       let dtd, tree = Cpdfmetadata.xmltree_of_bytes metadata in
       let newtree =
         begin match Cpdfmetadata.get_data_for Cpdfmetadata.pdfuaid "part" tree with
         | Some _ -> insert_as_rdf_description pdfua_marker (delete_pdfua_marker tree)
         | None -> insert_as_rdf_description pdfua_marker tree
         end
       in
         let newbytes = Cpdfmetadata.bytes_of_xmltree (dtd, newtree) in
         let pdf3 = Cpdfmetadata.set_metadata_from_bytes true newbytes pdf in
           pdf.Pdf.objects <- pdf3.Pdf.objects;
           pdf.Pdf.trailerdict <- pdf3.Pdf.trailerdict;
           pdf.Pdf.root <- pdf3.Pdf.root
   | None -> assert false

let extract_struct_tree pdf =
  match Pdf.lookup_obj pdf pdf.Pdf.root with
  | Pdf.Dictionary d ->
      let zero =
        `List [`Int 0;
                `Assoc [("/CPDFJSONformatversion", `Int 1);
                        ("/CPDFJSONpageobjnumbers", `List (map (fun x -> `Int (unopt (Pdfpage.page_object_number pdf x))) (ilist 1 (Pdfpage.endpage pdf))))]]
      in
        begin match lookup "/StructTreeRoot" d with
        | None -> `List [zero]
        | Some x ->
            let objs = Pdf.objects_referenced ["/Pg"; "/Obj"; "/Stm"; "/StmOwn"] [] pdf x in
              `List
                 (zero::map
                    (fun objnum ->
                       let jsonobj =
                         Cpdfjson.json_of_object ~utf8:true ~no_stream_data:false ~parse_content:false pdf (function _ -> ()) (Pdf.lookup_obj pdf objnum)
                       in
                         `List [`Int objnum; jsonobj])
                    objs)
        end
  | _ -> error "extract_struct_tree: no root"


(* Use JSON data to replace objects in a file. Negative objects are new ones,
   we make them positive and renumber them not to clash. Everything else must
   remain unrenumbered. *)
let replace_struct_tree pdf json =
  try
    let rec rewrite_indirects negobjnummap = function
      | Pdf.Indirect i ->
          begin match lookup i negobjnummap with
          | Some x -> Pdf.Indirect x
          | None -> Pdf.Indirect i
          end
      | Pdf.Dictionary d -> Pdf.recurse_dict (rewrite_indirects negobjnummap) d
      | Pdf.Array a -> Pdf.recurse_array (rewrite_indirects negobjnummap) a
      | x -> x
    in
      match json with
      | `List (`List [`Int 0; _]::xs) ->
          let pos, neg = List.partition (function (`List [`Int x; _]) -> x > 0 | _ -> error "structure 1") xs in
          let pos = map (function `List [`Int x; j] -> (x, Cpdfjson.object_of_json j) | _ -> error "structure 2") pos in
          let neg = map (function `List [`Int x; j] -> (x, Cpdfjson.object_of_json j) | _ -> error "structure 3") neg in
          let nextnum = Pdf.objcard pdf + 1 in
          let negobjnummap = if length neg = 0 then [] else map2 (fun n n' -> (n, n')) (map fst neg) (ilist nextnum (nextnum + length neg - 1)) in
          let pos = map (fun (objnum, obj) -> (objnum, rewrite_indirects negobjnummap obj)) pos in
          let neg = map (fun (objnum, obj) -> (objnum, rewrite_indirects negobjnummap obj)) neg in
            iter (Pdf.addobj_given_num pdf) (pos @ neg)
      | _ -> error "Top level JSON wrong. Must be list with 0 first."
  with
    e -> error (Printf.sprintf "replace_struct_tree: %s" (Printexc.to_string e))
