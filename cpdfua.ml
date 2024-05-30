open Pdfutil

exception MatterhornError of Cpdfyojson.Safe.t

let merror () = raise (MatterhornError `Null)

let matterhorn_01_003 pdf = ()
let matterhorn_01_004 pdf = ()
let matterhorn_01_005 pdf = ()
let matterhorn_01_007 pdf = ()
let matterhorn_02_001 pdf = ()
let matterhorn_02_003 pdf = ()
let matterhorn_06_001 pdf = ()
let matterhorn_06_002 pdf = ()
let matterhorn_06_003 pdf = ()

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

let matterhorn_09_004 pdf = ()
let matterhorn_09_005 pdf = ()
let matterhorn_09_006 pdf = ()
let matterhorn_09_007 pdf = ()
let matterhorn_09_008 pdf = ()
let matterhorn_10_001 pdf = ()
let matterhorn_11_001 pdf = ()
let matterhorn_11_002 pdf = ()
let matterhorn_11_003 pdf = ()
let matterhorn_11_004 pdf = ()
let matterhorn_11_005 pdf = ()
let matterhorn_11_006 pdf = ()
let matterhorn_13_004 pdf = ()
let matterhorn_14_002 pdf = ()
let matterhorn_14_003 pdf = ()
let matterhorn_14_006 pdf = ()
let matterhorn_14_007 pdf = ()
let matterhorn_15_003 pdf = ()
let matterhorn_17_002 pdf = ()
let matterhorn_17_003 pdf = ()
let matterhorn_19_003 pdf = ()
let matterhorn_19_004 pdf = ()
let matterhorn_20_001 pdf = ()
let matterhorn_20_002 pdf = ()
let matterhorn_20_003 pdf = ()
let matterhorn_21_001 pdf = ()
let matterhorn_25_001 pdf = ()
let matterhorn_26_001 pdf = ()
let matterhorn_26_002 pdf = ()
let matterhorn_28_002 pdf = ()
let matterhorn_28_004 pdf = ()
let matterhorn_28_005 pdf = ()
let matterhorn_28_006 pdf = ()
let matterhorn_28_007 pdf = ()
let matterhorn_28_008 pdf = ()
let matterhorn_28_009 pdf = ()
let matterhorn_28_010 pdf = ()
let matterhorn_28_011 pdf = ()
let matterhorn_28_012 pdf = ()
let matterhorn_28_014 pdf = ()
let matterhorn_28_015 pdf = ()
let matterhorn_28_016 pdf = ()
let matterhorn_28_017 pdf = ()
let matterhorn_28_018 pdf = ()
let matterhorn_28_012 pdf = ()
let matterhorn_30_001 pdf = ()
let matterhorn_30_002 pdf = ()
let matterhorn_31_001 pdf = ()
let matterhorn_31_002 pdf = ()
let matterhorn_31_003 pdf = ()
let matterhorn_31_004 pdf = ()
let matterhorn_31_005 pdf = ()
let matterhorn_31_006 pdf = ()
let matterhorn_31_007 pdf = ()
let matterhorn_31_008 pdf = ()
let matterhorn_31_009 pdf = ()
let matterhorn_31_011 pdf = ()
let matterhorn_31_012 pdf = ()
let matterhorn_31_013 pdf = ()
let matterhorn_31_014 pdf = ()
let matterhorn_31_015 pdf = ()
let matterhorn_31_016 pdf = ()
let matterhorn_31_017 pdf = ()
let matterhorn_31_018 pdf = ()
let matterhorn_31_019 pdf = ()
let matterhorn_31_020 pdf = ()
let matterhorn_31_021 pdf = ()
let matterhorn_31_022 pdf = ()
let matterhorn_31_023 pdf = ()
let matterhorn_31_024 pdf = ()
let matterhorn_31_025 pdf = ()
let matterhorn_31_026 pdf = ()
let matterhorn_31_027 pdf = ()
let matterhorn_31_028 pdf = ()
let matterhorn_31_029 pdf = ()
let matterhorn_31_030 pdf = ()

let matterhorn =
  [("01-003", "Content marked as Artifact is present inside tagged content.", "UA1:7.1-1", matterhorn_01_003);
   ("01-004", "Tagged content is present inside content marked as Artifact.", "UA1:7.1-1", matterhorn_01_004);
   ("01-005", "Content is neither marked as Artifact nor tagged as real content.", "UA1:7-1-2", matterhorn_01_005);
   ("01-007", "Suspects entry has a value of true.", "UA1:7-1-11", matterhorn_01_007);
   ("02-001", "One or more non-standard tag’s mapping does not terminate with a standard type.", "UA1:7.1-3", matterhorn_02_001);
   ("02-002", "A circular mapping exists.", "UA1:7.1-4", matterhorn_02_003);
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
       | e -> Some (name, "Incomplete", section, `Null)
    )
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

let mark pdf = ()
