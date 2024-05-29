open Pdfutil

exception MatterhornError

let matterhorn_01_003 pdf = ()
let matterhorn_01_004 pdf = ()
let matterhorn_01_005 pdf = ()
let matterhorn_01_007 pdf = ()
let matterhorn_02_001 pdf = ()
let matterhorn_02_003 pdf = ()
let matterhorn_06_001 pdf = ()
let matterhorn_06_002 pdf = ()
let matterhorn_06_003 pdf = ()
let matterhorn_07_001 pdf = ()
let matterhorn_07_002 pdf = ()
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
  ]

let test_matterhorn pdf =
  option_map
    (fun (name, error, section, test) ->
      try test pdf; None with
       | MatterhornError -> Some (name, error, section)
       | e -> Some (name, "Incomplete", section)
    )
    matterhorn

let test_matterhorn_print pdf =
  iter (fun (name, error, section) -> Printf.eprintf "%s %s %s\n" name section error) (test_matterhorn pdf)

let test_matterhorn_json pdf =
  `List (map (fun (name, error, section) -> `Assoc [("name", `String name); ("section", `String section); ("error", `String error)]) (test_matterhorn pdf))
