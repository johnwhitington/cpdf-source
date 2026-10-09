(** Annotations. *)
open Pdfutil
open Cpdferror

let rec subtype_of_string = function
  | "/Text" -> Pdfannot.Text
  | "/Link" -> Pdfannot.Link
  | "/FreeText" -> Pdfannot.FreeText
  | "/Line" -> Pdfannot.Line
  | "/Square" -> Pdfannot.Square
  | "/Circle" -> Pdfannot.Circle
  | "/Polygon" -> Pdfannot.Polygon
  | "/PolyLine" -> Pdfannot.PolyLine
  | "/Highlight" -> Pdfannot.Highlight
  | "/Underline" -> Pdfannot.Underline
  | "/Squiggly" -> Pdfannot.Squiggly
  | "/StrikeOut" -> Pdfannot.StrikeOut
  | "/Stamp" -> Pdfannot.Stamp
  | "/Caret" -> Pdfannot.Caret
  | "/Ink" -> Pdfannot.Ink
  | "/FileAttachment" -> Pdfannot.FileAttachment
  | "/Sound" -> Pdfannot.Sound
  | "/Movie" -> Pdfannot.Movie
  | "/Widget" -> Pdfannot.Widget
  | "/Screen" -> Pdfannot.Screen
  | "/PrinterMark" -> Pdfannot.PrinterMark
  | "/TrapNet" -> Pdfannot.TrapNet
  | "/Watermark" -> Pdfannot.Watermark
  | "/3D" -> Pdfannot.ThreeDee
  | "/Redact" -> Pdfannot.Redact
  | "/Projection" -> Pdfannot.Projection
  | s ->
      match explode s with
      | '/'::'P'::'o'::'p'::'u'::'p'::':'::r ->
          Pdfannot.Popup
            {subtype = subtype_of_string (implode r);
             annot_contents = None;
             subject = None;
             rectangle = (0., 0., 0., 0.);
             border = None;
             colour = None;
             annotrest = Pdf.Dictionary []}
      | _ -> Pdfannot.Unknown s

(* List annotations, simple old style. *)
let get_annotation_string encoding pdf annot =
  match Pdf.lookup_direct pdf "/Contents" annot with
  | Some (Pdf.String s) -> Cpdfmetadata.encode_output encoding s
  | _ -> ""

let print_annotation encoding pdf num s =
  let s = get_annotation_string encoding pdf s in
  match s with
  | "" -> ()
  | s ->
    flprint (Printf.sprintf "Page %d: " num);
    flprint s;
    flprint "\n"

let list_page_annotations encoding pdf num page =
  match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
  | Some (Pdf.Array annots) ->
      iter (print_annotation encoding pdf num) (map (Pdf.direct pdf) annots)
  | _ -> ()

let list_annotations range encoding pdf =
  Cpdfpage.iter_pages (list_page_annotations encoding pdf) pdf range

(* New, JSON style *)
let rewrite_destination f d =
  match d with
  | Pdf.Array (Pdf.Indirect i::r) -> (* out *)
      Pdf.Array (Pdf.Integer (f i)::r)
  | Pdf.Array (Pdf.Integer i::r) -> (* in *)
      Pdf.Array (Pdf.Indirect (f i)::r)
  | x -> x

let rewrite_destinations f pdf annot =
  (* Deal with /P in annotation *)
  let annot =
    match Pdf.indirect_number pdf "/P" annot with
    | Some i ->
        Pdf.add_dict_entry annot "/P" (Pdf.Integer (f i))
    | None ->
        match Pdf.lookup_direct pdf "/P" annot with
        | Some (Pdf.Integer i) ->
            Pdf.add_dict_entry annot "/P" (Pdf.Indirect (f i))
        | _ -> annot
  in
  (* Deal with /Dest in annotation *)
  match Pdf.lookup_direct pdf "/Dest" annot with
  | Some d -> Pdf.add_dict_entry annot "/Dest" (rewrite_destination f d)
  | None ->
      (* Deal with /A --> /D dest when /A --> /S = /GoTo *)
      match Pdf.lookup_direct pdf "/A" annot with
      | Some action ->
          begin match Pdf.lookup_direct pdf "/D" action with
          | Some d ->
              Pdf.add_dict_entry
                annot "/A" (Pdf.add_dict_entry action "/D" (rewrite_destination f d))
          | None -> annot
          end
     | None -> annot

let extra = ref []

let annotations_json_page ?subtypes ?subtypes_no calculate_pagenumber pdf page pagenum =
  match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
  | Some (Pdf.Array annots) ->
      option_map
        (fun annot ->
           begin match annot with
           | Pdf.Indirect objnum ->
               let annot = Pdf.direct pdf annot in
               let keep =
                 match subtypes with
                 | None -> true
                 | Some [] ->
                     begin match subtypes_no with
                     | None -> true
                     | Some l ->
                         match Pdf.lookup_direct pdf "/Subtype" annot with
                         | Some (Pdf.Name x') -> not (mem (subtype_of_string x') l)
                         | _ -> false
                         end
                 | Some l ->
                     match Pdf.lookup_direct pdf "/Subtype" annot with
                     | Some (Pdf.Name x') -> mem (subtype_of_string x') l
                     | _ -> true
               in
               if not keep then None else
               let annot =
                 rewrite_destinations
                   (fun i -> calculate_pagenumber (Pdfdest.Fit (Pdfdest.PageObject i)))
                   pdf annot
               in
                 extra := annot::!extra;
                 Some (`List
                   [`Int pagenum;
                    `Int objnum;
                     Cpdfjson.json_of_object ~utf8:true ~clean_strings:true pdf (fun _ -> ())
                       ~no_stream_data:false ~parse_content:false annot])
           | _ -> Pdfe.log "Warning: annotations must be indirect\n"; None
           end)
        annots
  | _ -> []

let get_annotations_json ?subtypes ?subtypes_no pdf range =
  let refnums = Pdf.page_reference_numbers pdf in
  let fastrefnums = hashtable_of_dictionary (combine refnums (indx refnums)) in
  let calculate_pagenumber =  Pdfpage.pagenumber_of_target ~fastrefnums pdf in
  extra := [];
  let pages = Pdfpage.pages_of_pagetree pdf in
  let pagenums = indx pages in
  let pairs = combine pages pagenums in
  let pairs = option_map (fun (p, n) -> if mem n range then Some (p, n) else None) pairs in
  let pages, pagenums = split pairs in
  let json = flatten (map2 (annotations_json_page ?subtypes ?subtypes_no calculate_pagenumber pdf) pages pagenums) in
  let jsonobjnums : int list = map (function `List [_; `Int n; _] -> n | _ -> assert false) json in
  let extra =
    map
      (fun n ->
         `List
           [`Int n;
            Cpdfjson.json_of_object ~utf8:true ~clean_strings:true pdf (fun _ -> ())
              ~no_stream_data:false ~parse_content:false (Pdf.lookup_obj pdf n)])
      (setify
        (flatten
          (map 
            (fun x ->
               let x = Pdf.remove_dict_entry x "/Popup" in
               let x = Pdf.remove_dict_entry x "/Parent" in
                 Pdf.objects_referenced [] [] pdf x)
          !extra)))
  in
  let extra =
    option_map
      (function `List [`Int n; _] as json -> if mem n jsonobjnums then None else Some json | _ -> assert false)
      extra
  in
  let header =
    `List
     [`Int ~-1;
      Cpdfjson.json_of_object ~utf8:true ~clean_strings:true pdf (fun _ -> ())
        ~no_stream_data:false ~parse_content:false
        (Pdf.Dictionary ["/CPDFJSONannotformatversion", Pdf.Integer 1])]
  in
  let json = `List ([header] @ json @ extra) in
    Pdfio.bytes_of_string (Cpdfyojson.Safe.pretty_to_string json)

(* Return annotations *)
let get_annotations encoding pdf =
  let pages = Pdfpage.pages_of_pagetree pdf in
    flatten
      (map2
       (fun page pagenumber ->
         match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
         | Some (Pdf.Array annots) ->
             let strings =
               map (get_annotation_string encoding pdf) (map (Pdf.direct pdf) annots)
             in
               combine (many pagenumber (length strings)) strings
         | _ -> [])
        pages
        (ilist 1 (length pages))) 

(** Set annotations from JSON, keeping any existing ones. *)
let set_annotations_json pdf i =
  match Cpdfyojson.Safe.from_string (Pdfio.string_of_input i) with
  | `List entries ->
      (* Renumber the PDF so everything has bigger object numbers than that. *)
      let maxobjnum =
        fold_left max min_int
          (map
            (function
             | `List [_; `Int i; _] | `List [`Int i; _] -> i
             | _ -> error "Bad annots entry")
           entries)
      in
      let pdf_objnums = map fst (list_of_hashtbl pdf.Pdf.objects.Pdf.pdfobjects) in
      let change_table =
        hashtable_of_dictionary (map2 (fun f t -> (f, t)) pdf_objnums (ilist (maxobjnum + 1) (maxobjnum + length pdf_objnums)))
      in
      let pdf' = Pdf.renumber change_table pdf in
        pdf.root <- pdf'.root;
        pdf.objects <- pdf'.objects;
        pdf.trailerdict <- pdf'.trailerdict;
        (* Add the extra objects back in and build the annotations. *) 
        let extras = option_map (function `List [`Int i; o] -> Some (i, o) | _ -> None) entries in
        let annots = option_map (function `List [`Int pagenum; `Int i; o] -> Some (pagenum, i, o) | _ -> None) entries in
          iter (fun (i, o) -> Pdf.addobj_given_num pdf (i, Cpdfjson.object_of_json o)) extras;
          let pageobjnummap =
            let refnums = Pdf.page_reference_numbers pdf in
              combine (indx refnums) refnums
          in
          let pages = Pdfpage.pages_of_pagetree pdf in
          let annotsforeachpage = collate compare (sort compare annots) in
          let newpages =
            map2
              (fun pagenum page ->
                 let forthispage = flatten (keep (function (p, _, _)::t when p = pagenum -> true | _ -> false) annotsforeachpage) in
                   iter
                     (fun (_, i, o) ->
                        let f = fun pnum -> match lookup pnum pageobjnummap with Some x -> x | None -> pnum in
                          Pdf.addobj_given_num pdf (i, rewrite_destinations f pdf (Cpdfjson.object_of_json o)))
                     forthispage;
                   if forthispage = [] then page else
                     let annots =
                       match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with | Some (Pdf.Array annots) -> annots | _ -> []
                     in
                     let newannots = map (fun (_, i, _) -> Pdf.Indirect i) forthispage in
                       {page with Pdfpage.rest = Pdf.add_dict_entry page.Pdfpage.rest "/Annots" (Pdf.Array (annots @ newannots))})
              (indx pages)
              pages
          in
            let pdf' = Pdfpage.change_pages true pdf newpages in
              pdf.root <- pdf'.root;
              pdf.objects <- pdf'.objects;
              pdf.trailerdict <- pdf'.trailerdict
  | _ -> error "Bad Annotations JSON file"

let copy_annotations range frompdf topdf =
  set_annotations_json topdf (Pdfio.input_of_bytes (get_annotations_json frompdf range))

(* Remove annotations *)
let remove_annotations range pdf =
  let remove_annotations_page pagenum page =
    if mem pagenum range then
      let rest' =
        Pdf.remove_dict_entry page.Pdfpage.rest "/Annots"
      in
        {page with Pdfpage.rest = rest'}
    else
      page
  in
    Cpdfpage.process_pages (Pdfpage.ppstub remove_annotations_page) pdf range

(* We add the text by generating a page/pdf with the content and pulling its
   contents out, adding them to the content we have already. We extract any
   fonts from the resources too. *)
let generate_appearance
  ~overlay ~overlay_text_colour ~overlay_justification ~overlay_repeat ~overlay_auto_size
  ~font ~fontsize ~opacity ~linespacing ~linewidth ~outline pdf ops (minx, miny, maxx, maxy)
=
  let pages =
    Cpdftexttopdf.typeset_fake_pages
      pdf
      ~font
      ~papersize:(Pdfpaper.make Pdfunits.PdfPoint (maxx -. minx) (maxy -. miny))
      ~fontsize
      ~colour:overlay_text_colour
      ~opacity
      ~linespacing
      ~linewidth
      ~outline
      (Pdfio.bytes_of_string (if overlay_repeat then fold_left (fun x y -> x ^ " " ^ y) "" (many overlay 100) else overlay))
  in
    (* Get the ops and resources, and concatenate and return. *)
    let page =
      match pages with
      | [] -> assert false
      | p::_ -> p
    in
      let page_ops = Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content in
        ops @ [Pdfops.Op_cm (Pdftransform.mktranslate minx miny)] @ page_ops, page.Pdfpage.resources

(* Add a (presently, redaction) annotation at the given position on the given pages. *)
let add_annotation (minx, miny, maxx, maxy)
  ~main_color ~outline_color ~overlay ~overlay_text_colour ~overlay_justification ~overlay_repeat ~overlay_auto_size
  ~font ~fontsize ~opacity ~linespacing ~linewidth ~outline
  pdf range
=
  let add_dict resources = function
  | Pdf.Stream ({contents = (dict, stream)} as s) ->
      let dict = Pdf.add_dict_entry dict "/BBox"
        (Pdf.Array [Pdf.Real (minx -. 0.5); Pdf.Real (miny -. 0.5); Pdf.Real (maxx +. 0.5); Pdf.Real (maxy +. 0.5)])
      in
      let dict = Pdf.add_dict_entry dict "/Matrix"
        (Pdf.Array [Pdf.Real 1.; Pdf.Real 0.; Pdf.Real 0.; Pdf.Real 1.; Pdf.Real (~-.minx +. 0.5); (Pdf.Real (~-.miny +. 0.5))])
      in
      let dict = Pdf.add_dict_entry dict "/Resources" resources in
      let dict = Pdf.add_dict_entry dict "/Subtype" (Pdf.Name "/Form") in
      let dict = Pdf.add_dict_entry dict "/Type" (Pdf.Name "/XObject") in
      s := (dict, stream);
      (Pdf.Stream s)
  | _ -> assert false
  in
  let d_ro_r =
    let ops, resources =
      let ops =
        [Cpdfutil.colour_op main_color;
         Pdfops.Op_cm {Pdftransform.a = 1.; b = 0.; c = 0.; d = 1.; e = 0.; f = 0.};
         Pdfops.Op_m (minx, miny); Pdfops.Op_l (maxx, miny); Pdfops.Op_l (maxx, maxy); Pdfops.Op_l (minx, maxy); Pdfops.Op_l (minx, miny);
         Pdfops.Op_f]
      in
        match overlay with
        | None -> ops, Pdf.Dictionary []
        | Some overlay ->
            generate_appearance ~overlay ~overlay_text_colour ~overlay_justification ~overlay_repeat ~overlay_auto_size
            ~font ~fontsize ~opacity ~linespacing ~linewidth ~outline pdf ops (minx, miny, maxx, maxy)
    in
      Pdf.addobj pdf (add_dict resources (Pdfops.stream_of_ops ops))
  in
  let n =
    let ops, resources =
      [Cpdfutil.colour_op_stroke outline_color;
         Pdfops.Op_cm {Pdftransform.a = 1.; b = 0.; c = 0.; d = 1.; e = 0.; f = 0.};
         Pdfops.Op_w 1.5;
         Pdfops.Op_J 2;
         Pdfops.Op_m (minx, miny); Pdfops.Op_l (maxx, miny); Pdfops.Op_l (maxx, maxy); Pdfops.Op_l (minx, maxy); Pdfops.Op_l (minx, miny);
         Pdfops.Op_S],
      (Pdf.Dictionary [])
    in
      Pdf.addobj pdf (add_dict resources (Pdfops.stream_of_ops ops))
  in
  let nm =
    let t = Unix.gettimeofday () in
    let now_ms = (fun () -> Int64.of_float t) in
      Cpdfuuidm.to_binary_string (Cpdfuuidm.v7_non_monotonic_gen ~now_ms (Random.State.make_self_init ()) ())
  in
  let m =
    match Sys.getenv_opt "CPDF_REPRODUCIBLE_DATES" with
    | Some "true" -> Cpdfstrftime.strftime ~time:Cpdfstrftime.dummy "D:%Y%m%d%H%M%S" 
    | _ -> Cpdfstrftime.strftime "D:%Y%m%d%H%M%S"
  in
  let quadpoints =
    [Pdf.Real minx; Pdf.Real miny; Pdf.Real maxx; Pdf.Real miny; Pdf.Real minx; Pdf.Real maxy; Pdf.Real maxx; Pdf.Real maxy]
  in
  let annot =
    {Pdfannot.subtype = Pdfannot.Redact;
     annot_contents = None;
     subject = None;
     rectangle = (minx, miny, maxx, maxy);
     border = None;
     colour = Some [0.858826; 0.203918; 0.145096];
     annotrest =
       Pdf.Dictionary
         [("/F", Pdf.Integer 4);
          ("/NM", Pdf.String nm);
          ("/M", Pdf.String m);
          ("/QuadPoints", Pdf.Array quadpoints);
          ("/AP", Pdf.Dictionary [("/D", Pdf.Indirect d_ro_r;); ("/N", Pdf.Indirect n); ("/R", Pdf.Indirect d_ro_r)]);
          ("/RO", Pdf.Indirect d_ro_r)]}
  in
    Cpdfpage.process_pages
      (Pdfpage.ppstub (fun pnum page -> if mem pnum range then Pdfannot.add_annotation pdf page annot else page)) pdf range

(* Stamp onto page from appearance stream in annotation. *)
let stamp_annotation_appearance pdf page annot appearance_i =
  let rec fresh_name ns n =
    let newname = "/X" ^ string_of_int n in
    if mem newname ns then fresh_name ns (n + 1) else newname
  in
    let xobjects, name =
      match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
      | Some (Pdf.Dictionary d) -> (Pdf.Dictionary d, fresh_name (map fst d) 0)
      | _ -> (Pdf.Dictionary [], "/X0")
    in
      let resources = Pdf.add_dict_entry page.Pdfpage.resources "/XObject" (Pdf.add_dict_entry xobjects name (Pdf.Indirect appearance_i)) in
      let matrix = Pdf.parse_matrix pdf "/Matrix" (Pdf.direct pdf (Pdf.Indirect appearance_i)) in
      let bminx, bminy, bmaxx, bmaxy =
        match Pdf.lookup_direct pdf "/BBox" (Pdf.Indirect appearance_i) with
        | None -> (0., 0., 0., 0.)
        | Some x -> begin try Pdf.parse_rectangle pdf x with _ -> (0., 0., 0., 0.) end
      in
      let rminx, rminy, rmaxx, rmaxy =
        match Pdf.lookup_direct pdf "/Rect" annot with
        | None -> (0., 0., 0., 0.)
        | Some x -> begin try Pdf.parse_rectangle pdf x with _ -> (0., 0., 0., 0.) end
      in
      let tbx0, tby0 = Pdftransform.transform_matrix matrix (bminx, bminy) in
      let tbx1, tby1 = Pdftransform.transform_matrix matrix (bmaxx, bmaxy) in
      let tap_minx, tap_miny, tap_maxx, tap_maxy = fmin tbx0 tbx1, fmin tby0 tby1, fmax tbx0 tbx1, fmax tby0 tby1 in
      let a_matrix =
        let dx, dy = rminx -. tap_minx, rminy -. tap_miny in
        let sx, sy = (rmaxx -. rminx) /. (tap_maxx -. tap_minx), (rmaxy -. rminy) /. (tap_maxy -. tap_miny) in
          Pdftransform.matrix_compose (Pdftransform.mkscale (rminx, rminy) sx sy) (Pdftransform.mktranslate dx dy)
      in
      let a_matrix_normalised =
        {Pdftransform.a = safe_float a_matrix.a;
         Pdftransform.b = safe_float a_matrix.b;
         Pdftransform.c = safe_float a_matrix.c;
         Pdftransform.d = safe_float a_matrix.d;
         Pdftransform.e = safe_float a_matrix.e;
         Pdftransform.f = safe_float a_matrix.f}
      in
      let ops = Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content in
      let ops = Pdfops.Op_q::Pdfops.Op_cm a_matrix_normalised::Pdfops.Op_Do name::Pdfops.Op_Q::ops in
        {page with resources; content = [Pdfops.stream_of_ops ops]}

(* Remove orphaned /Popup annotations from a collection of annotations on a page, in-situ.  *)
let remove_orphaned_popups pdf annots =
  let popup_objnums =
    option_map
      (function Pdf.Indirect i -> begin match Pdf.lookup_direct pdf "/Subtype" (Pdf.Indirect i) with Some (Pdf.Name "/Popup") -> Some i | _ -> None end | _ -> None)
      annots
  in
  let orphaned =
    option_map
      (function i -> match Pdf.lookup_immediate "/Parent" (Pdf.direct pdf (Pdf.Indirect i)) with Some (Pdf.Indirect x) when not (mem (Pdf.Indirect x) annots) -> Some i | _ -> None)
      popup_objnums
  in
    option_map
      (function Pdf.Indirect i -> if mem i orphaned then None else Some (Pdf.Indirect i) | x -> Some x)
      annots

(* Flatten annotations to page. If no apperance, annotation remains. Orphaned /Popups cleaned. *)
let flatten ?subtypes ?subtypes_no pdf range =
  Cpdfpage.process_pages  
    (Pdfpage.ppstub
      (fun pnum page ->
         if mem pnum range then
           let page = ref page in
           let annots =
             match Pdf.lookup_direct pdf "/Annots" !page.Pdfpage.rest with
             | Some (Pdf.Array annots) -> annots
             | _ -> []
           in
           let apn_objnums =
             option_map
               (function a ->
                  let subtype =
                    match Pdf.lookup_direct pdf "/Subtype" a with
                    | Some (Pdf.Name x) -> subtype_of_string x
                    | _ -> Pdfannot.Unknown ""
                  in

                    begin match Pdf.lookup_direct pdf "/AP" a with
                    | Some ap -> 
                        begin match Pdf.lookup_immediate "/N" ap with
                        | Some (Pdf.Indirect i) ->
                            begin match subtypes with
                            | Some [] | None ->
                                begin match subtypes_no with
                                | Some [] | None -> Some (a, i)
                                | Some l ->
                                    if mem subtype l then None else Some (a, i)
                                end
                            | Some l ->
                                if mem subtype l then Some (a, i) else None
                            end
                        | _ -> None
                        end
                    | None -> None
                    end)
               annots
           in
             iter (fun (a, i) -> page := stamp_annotation_appearance pdf !page a i) apn_objnums;
             {!page with Pdfpage.rest = Pdf.add_dict_entry !page.Pdfpage.rest "/Annots" (Pdf.Array (remove_orphaned_popups pdf annots))}
         else page))
      pdf
      range
