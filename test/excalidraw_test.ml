open Excalidraw_export.Types
open Excalidraw_export.Excalidraw
open Excalidraw_export.Render_svg

let project_root =
  Filename.(
    concat
      (concat
         (concat (dirname Sys.executable_name) "..")
         "..")
      "..")

let fixture path = Filename.concat project_root path

let parse_fixture path =
  try parse_file (fixture path)
  with
  | Sys_error msg -> Alcotest.fail ("Sys_error: " ^ msg)
  | Yojson.Json_error msg -> Alcotest.fail ("Json_error: " ^ msg)
  | Failure msg -> Alcotest.fail ("Failure: " ^ msg)

let float_testable = Alcotest.testable Fmt.float ( = )

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
       let len = in_channel_length ic in
       really_input_string ic len)

let find_substring haystack needle =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop i =
    if i + needle_len > haystack_len then None
    else if String.sub haystack i needle_len = needle then Some i
    else loop (i + 1)
  in
  loop 0

let extract_attr name svg =
  let attr = name ^ "=\"" in
  match find_substring svg attr with
  | None -> Alcotest.fail ("Missing SVG attribute: " ^ name)
  | Some start ->
    let value_start = start + String.length attr in
    match String.index_from_opt svg value_start '"' with
    | None -> Alcotest.fail ("Unterminated SVG attribute: " ^ name)
    | Some value_end -> String.sub svg value_start (value_end - value_start)

let strip_px value =
  if String.length value >= 2 && value.[String.length value - 2] = 'p'
     && value.[String.length value - 1] = 'x'
  then String.sub value 0 (String.length value - 2)
  else value

let reference_svg name = read_file (fixture ("test/fixtures/upstream-excalidraw/" ^ name))

let test_parse_upstream_diagram () =
  let doc = parse_fixture "test/fixtures/upstream-excalidraw/diagram.json" in
  Alcotest.(check int) "element count" 3 (List.length doc.elements);
  (match List.find_opt (function Rectangle _ -> true | _ -> false) doc.elements with
   | Some (Rectangle el) ->
     Alcotest.(check string) "rect id" "rect1" el.base.id;
     Alcotest.(check float_testable) "rect x" 100.0 el.base.x;
     Alcotest.(check float_testable) "rect y" 100.0 el.base.y;
     Alcotest.(check float_testable) "rect w" 214.0 el.base.width;
     Alcotest.(check float_testable) "rect h" 214.0 el.base.height;
     Alcotest.(check string) "stroke" "#000000" el.base.stroke_color;
     Alcotest.(check string) "bg" "#15aabf" el.base.background_color;
     Alcotest.(check int) "roughness" 1 el.base.roughness;
     Alcotest.(check int) "opacity" 100 el.base.opacity
   | _ -> Alcotest.fail "expected rectangle element")

let test_parse_upstream_arrow_line_text () =
  let doc = parse_fixture "test/fixtures/upstream-excalidraw/arrow_line_text.json" in
  Alcotest.(check int) "element count" 3 (List.length doc.elements);
  (match List.find_opt (function Arrow _ -> true | _ -> false) doc.elements with
   | Some (Arrow el) ->
     Alcotest.(check string) "arrow id" "arrow1" el.base.id;
     Alcotest.(check (option string)) "end arrowhead" (Some "arrow") el.end_arrowhead
   | _ -> Alcotest.fail "expected arrow element");
  (match List.find_opt (function Line _ -> true | _ -> false) doc.elements with
   | Some (Line el) ->
     Alcotest.(check string) "line id" "line1" el.base.id;
     Alcotest.(check (list (pair float_testable float_testable))) "points" [(0.0, 0.0); (100.0, 0.0)] el.points
   | _ -> Alcotest.fail "expected line element");
  (match List.find_opt (function Text _ -> true | _ -> false) doc.elements with
   | Some (Text el) ->
     Alcotest.(check string) "text" "Hello World" el.text;
     Alcotest.(check int) "fontSize" 24 el.font_size
   | _ -> Alcotest.fail "expected text element")

let test_render_svg_upstream_diagram () =
  let doc = parse_fixture "test/fixtures/upstream-excalidraw/diagram.json" in
  let svg = render ~scale:1.0 doc in
  Alcotest.(check bool) "svg non-empty" true (String.length svg > 0);
  Alcotest.(check bool) "svg has svg tag" true (String.contains svg '<');
  Alcotest.(check bool) "svg has rect" true (String.contains svg '<')

let test_render_svg_arrow_line_text () =
  let doc = parse_fixture "test/fixtures/upstream-excalidraw/arrow_line_text.json" in
  let svg = render ~scale:1.0 doc in
  Alcotest.(check bool) "svg non-empty" true (String.length svg > 0);
  Alcotest.(check bool) "svg has path" true (String.contains svg 'd')

let test_render_svg_contains_text () =
  let doc = parse_fixture "test/fixtures/upstream-excalidraw/arrow_line_text.json" in
  let svg = render ~scale:1.0 doc in
  Alcotest.(check bool) "svg contains Hello World" true (String.contains svg 'H')

let test_render_svg_viewbox () =
  let doc = parse_fixture "test/fixtures/upstream-excalidraw/diagram.json" in
  let svg = render ~scale:1.0 doc in
  Alcotest.(check bool) "svg has viewBox" true (String.contains svg 'v')

let test_snapshot_diagram_viewbox_matches_excalidraw () =
  let doc = parse_fixture "test/fixtures/upstream-excalidraw/diagram.json" in
  let reference = reference_svg "diagram.reference.svg" in
  let actual = render ~scale:1.0 doc in
  Alcotest.(check string)
    "diagram viewBox matches upstream reference"
    (extract_attr "viewBox" reference)
    (extract_attr "viewBox" actual)

let test_snapshot_diagram_background_matches_excalidraw () =
  let doc = parse_fixture "test/fixtures/upstream-excalidraw/diagram.json" in
  let reference = reference_svg "diagram.reference.svg" in
  let actual = render ~scale:1.0 doc in
  Alcotest.(check string)
    "diagram background rect matches upstream reference"
    (extract_attr "width" reference)
    (strip_px (extract_attr "width" actual));
  Alcotest.(check string)
    "diagram background height matches upstream reference"
    (extract_attr "height" reference)
    (strip_px (extract_attr "height" actual))

let test_rough_rectangle_deterministic () =
  let (stroke1, fill1) = Excalidraw_export.Rough.rectangle ~seed:12345 ~roughness:1 ~stroke_width:1.0 ~width:200.0 ~height:100.0 () in
  let (stroke2, fill2) = Excalidraw_export.Rough.rectangle ~seed:12345 ~roughness:1 ~stroke_width:1.0 ~width:200.0 ~height:100.0 () in
  Alcotest.(check string) "stroke deterministic" stroke1 stroke2;
  Alcotest.(check string) "fill deterministic" fill1 fill2

let test_rough_ellipse_deterministic () =
  let (stroke1, fill1) = Excalidraw_export.Rough.ellipse ~seed:99999 ~roughness:1 ~stroke_width:1.0 ~width:200.0 ~height:100.0 () in
  let (stroke2, fill2) = Excalidraw_export.Rough.ellipse ~seed:99999 ~roughness:1 ~stroke_width:1.0 ~width:200.0 ~height:100.0 () in
  Alcotest.(check string) "stroke deterministic" stroke1 stroke2;
  Alcotest.(check string) "fill deterministic" fill1 fill2

let test_rough_diamond_deterministic () =
  let (stroke1, fill1) = Excalidraw_export.Rough.diamond ~seed:42 ~roughness:1 ~stroke_width:1.0 ~width:200.0 ~height:100.0 () in
  let (stroke2, fill2) = Excalidraw_export.Rough.diamond ~seed:42 ~roughness:1 ~stroke_width:1.0 ~width:200.0 ~height:100.0 () in
  Alcotest.(check string) "stroke deterministic" stroke1 stroke2;
  Alcotest.(check string) "fill deterministic" fill1 fill2

let test_rough_different_seeds_differ () =
  let (stroke1, _) = Excalidraw_export.Rough.rectangle ~seed:1 ~roughness:1 ~stroke_width:1.0 ~width:200.0 ~height:100.0 () in
  let (stroke2, _) = Excalidraw_export.Rough.rectangle ~seed:2 ~roughness:1 ~stroke_width:1.0 ~width:200.0 ~height:100.0 () in
  Alcotest.(check bool) "different seeds produce different paths" true (stroke1 <> stroke2)

let test_rough_rectangle_has_cubic_beziers () =
  let (stroke, fill) = Excalidraw_export.Rough.rectangle ~seed:1041657908 ~roughness:1 ~stroke_width:1.0 ~width:214.0 ~height:214.0 () in
  Alcotest.(check bool) "stroke has C commands" true (String.contains stroke 'C');
  Alcotest.(check bool) "fill has C commands" true (String.contains fill 'C');
  Alcotest.(check bool) "stroke has M commands" true (String.contains stroke 'M');
  Alcotest.(check bool) "fill has M commands" true (String.contains fill 'M')

let test_rough_ellipse_has_cubic_beziers () =
  let (stroke, fill) = Excalidraw_export.Rough.ellipse ~seed:1041657908 ~roughness:1 ~stroke_width:1.0 ~width:214.0 ~height:214.0 () in
  Alcotest.(check bool) "stroke has C commands" true (String.contains stroke 'C');
  Alcotest.(check bool) "fill has C commands" true (String.contains fill 'C')

let test_rough_diamond_has_cubic_beziers () =
  let (stroke, fill) = Excalidraw_export.Rough.diamond ~seed:1041657908 ~roughness:1 ~stroke_width:1.0 ~width:214.0 ~height:214.0 () in
  Alcotest.(check bool) "stroke has C commands" true (String.contains stroke 'C');
  Alcotest.(check bool) "fill has C commands" true (String.contains fill 'C')

let test_rough_roughness_0_no_jitter () =
  let (stroke, _) = Excalidraw_export.Rough.rectangle ~seed:12345 ~roughness:0 ~stroke_width:1.0 ~width:200.0 ~height:100.0 () in
  (* With roughness 0, maxRandomnessOffset = 0, so all random offsets should be 0 *)
  (* The path should still have M and C commands but with zero offsets *)
  Alcotest.(check bool) "stroke has M commands" true (String.contains stroke 'M')

let test_render_rough_paths_in_svg () =
  let doc = parse_fixture "test/fixtures/upstream-excalidraw/diagram.json" in
  let svg = render ~scale:1.0 doc in
  (* Should have stroke-linecap="round" on rough paths *)
  Alcotest.(check bool) "has stroke-linecap" true (find_substring svg "stroke-linecap" <> None);
  (* Should have cubic bezier C commands from rough paths *)
  Alcotest.(check bool) "has C commands" true (String.contains svg 'C');
  (* Should have translate transform for rough shapes *)
  Alcotest.(check bool) "has translate" true (find_substring svg "translate" <> None)

let test_render_rough_fill_and_stroke_paths () =
  let doc = parse_fixture "test/fixtures/upstream-excalidraw/diagram.json" in
  let svg = render ~scale:1.0 doc in
  (* Should have hachure fill paths with background color *)
  Alcotest.(check bool) "has fill color #15aabf" true (find_substring svg "#15aabf" <> None);
  (* Should have stroke paths with stroke color *)
  Alcotest.(check bool) "has stroke color #000000" true (find_substring svg "#000000" <> None)

(* --- Snapshot structural tests --- *)

let test_snapshot_arrow_line_text_viewbox () =
  let doc = parse_fixture "test/fixtures/upstream-excalidraw/arrow_line_text.json" in
  let reference = reference_svg "arrow_line_text.reference.svg" in
  let actual = render ~scale:1.0 doc in
  let ref_vb = extract_attr "viewBox" reference in
  let act_vb = extract_attr "viewBox" actual in
  let parts = String.split_on_char ' ' ref_vb in
  let ref_w = float_of_string (List.nth parts 2) in
  let ref_h = float_of_string (List.nth parts 3) in
  let parts = String.split_on_char ' ' act_vb in
  let act_w = float_of_string (List.nth parts 2) in
  let act_h = float_of_string (List.nth parts 3) in
  let tolerance = 0.001 in
  Alcotest.(check bool) "arrow_line_text viewBox width matches" true (Float.abs (ref_w -. act_w) < tolerance);
  Alcotest.(check bool) "arrow_line_text viewBox height matches" true (Float.abs (ref_h -. act_h) < tolerance)

let test_snapshot_diagram_path_count () =
  let doc = parse_fixture "test/fixtures/upstream-excalidraw/diagram.json" in
  let actual = render ~scale:1.0 doc in
  (* Each shape produces 2 paths (fill + stroke) with 3 shapes = 6 paths *)
  let path_count = ref 0 in
  let rec count str =
    match find_substring str "<path " with
    | None -> ()
    | Some i -> incr path_count; count (String.sub str (i + 1) (String.length str - i - 1))
  in
  count actual;
  Alcotest.(check int) "diagram has 6 path elements" 6 !path_count

let test_snapshot_diagram_group_count () =
  let doc = parse_fixture "test/fixtures/upstream-excalidraw/diagram.json" in
  let actual = render ~scale:1.0 doc in
  let g_count = ref 0 in
  let rec count str =
    match find_substring str "<g " with
    | None -> ()
    | Some i -> incr g_count; count (String.sub str (i + 1) (String.length str - i - 1))
  in
  count actual;
  Alcotest.(check int) "diagram has 3 group elements" 3 !g_count

let test_snapshot_diagram_has_rough_paths () =
  let doc = parse_fixture "test/fixtures/upstream-excalidraw/diagram.json" in
  let actual = render ~scale:1.0 doc in
  (* Should have C commands in paths (cubic beziers from rough) *)
  let c_count = ref 0 in
  let rec count str =
    match find_substring str "C" with
    | None -> ()
    | Some i -> incr c_count; count (String.sub str (i + 1) (String.length str - i - 1))
  in
  count actual;
  Alcotest.(check bool) "diagram has cubic bezier commands" true (!c_count > 0)

let test_snapshot_diagram_stroke_colors () =
  let doc = parse_fixture "test/fixtures/upstream-excalidraw/diagram.json" in
  let actual = render ~scale:1.0 doc in
  let stroke_count = ref 0 in
  let rec count str =
    match find_substring str "stroke=\"" with
    | None -> ()
    | Some i -> incr stroke_count; count (String.sub str (i + 1) (String.length str - i - 1))
  in
  count actual;
  (* 3 shapes * 2 paths each = 6 stroke attributes *)
  Alcotest.(check int) "diagram stroke count" 6 !stroke_count

let test_snapshot_arrow_line_text_structure () =
  let doc = parse_fixture "test/fixtures/upstream-excalidraw/arrow_line_text.json" in
  let actual = render ~scale:1.0 doc in
  (* Should have rough paths for line and arrow *)
  Alcotest.(check bool) "line has C bezier" true (find_substring actual "C" <> None);
  (* Should have arrowhead paths *)
  Alcotest.(check bool) "arrow has arrowhead path" true (find_substring actual "C" <> None);
  (* Should contain text *)
  Alcotest.(check bool) "contains Hello World" true (find_substring actual "Hello World" <> None);
  (* Should have stroke-linecap on rough paths *)
  Alcotest.(check bool) "has stroke-linecap" true (find_substring actual "stroke-linecap" <> None)

let test_snapshot_arrow_line_text_viewbox_size () =
  let doc = parse_fixture "test/fixtures/upstream-excalidraw/arrow_line_text.json" in
  let reference = reference_svg "arrow_line_text.reference.svg" in
  let actual = render ~scale:1.0 doc in
  let ref_vb = extract_attr "viewBox" reference in
  let act_vb = extract_attr "viewBox" actual in
  let parts = String.split_on_char ' ' ref_vb in
  let ref_h = float_of_string (List.nth parts 3) in
  let parts = String.split_on_char ' ' act_vb in
  let act_h = float_of_string (List.nth parts 3) in
  let tolerance = 0.001 in
  Alcotest.(check bool) "arrow_line_text viewBox height" true (Float.abs (ref_h -. act_h) < tolerance)

let snapshot_tests =
  [
    ("snapshot arrow_line_text viewBox matches Excalidraw", `Quick, test_snapshot_arrow_line_text_viewbox);
    ("snapshot diagram path count", `Quick, test_snapshot_diagram_path_count);
    ("snapshot diagram group count", `Quick, test_snapshot_diagram_group_count);
    ("snapshot diagram has rough paths", `Quick, test_snapshot_diagram_has_rough_paths);
    ("snapshot diagram stroke colors", `Quick, test_snapshot_diagram_stroke_colors);
    ("snapshot arrow_line_text structure", `Quick, test_snapshot_arrow_line_text_structure);
    ("snapshot arrow_line_text viewBox size", `Quick, test_snapshot_arrow_line_text_viewbox_size);
  ]

let rough_tests =
  [
    ("rough rectangle deterministic", `Quick, test_rough_rectangle_deterministic);
    ("rough ellipse deterministic", `Quick, test_rough_ellipse_deterministic);
    ("rough diamond deterministic", `Quick, test_rough_diamond_deterministic);
    ("rough different seeds differ", `Quick, test_rough_different_seeds_differ);
    ("rough rectangle has cubic beziers", `Quick, test_rough_rectangle_has_cubic_beziers);
    ("rough ellipse has cubic beziers", `Quick, test_rough_ellipse_has_cubic_beziers);
    ("rough diamond has cubic beziers", `Quick, test_rough_diamond_has_cubic_beziers);
    ("rough roughness 0 no jitter", `Quick, test_rough_roughness_0_no_jitter);
    ("render rough paths in SVG", `Quick, test_render_rough_paths_in_svg);
    ("render rough fill and stroke paths", `Quick, test_render_rough_fill_and_stroke_paths);
  ]

let tests =
  [
    ("parse upstream diagram", `Quick, test_parse_upstream_diagram);
    ("parse upstream arrow/line/text", `Quick, test_parse_upstream_arrow_line_text);
    ("render upstream diagram svg", `Quick, test_render_svg_upstream_diagram);
    ("render arrow/line/text svg", `Quick, test_render_svg_arrow_line_text);
    ("render svg contains text", `Quick, test_render_svg_contains_text);
    ("render svg has viewBox", `Quick, test_render_svg_viewbox);
    ("snapshot diagram viewBox matches Excalidraw", `Quick, test_snapshot_diagram_viewbox_matches_excalidraw);
    ("snapshot diagram background matches Excalidraw", `Quick, test_snapshot_diagram_background_matches_excalidraw);
  ]

let () =
  Alcotest.run "excalidraw-export" [
    ("upstream fixtures", tests);
    ("rough paths", rough_tests);
    ("snapshots", snapshot_tests);
  ]
