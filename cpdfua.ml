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
