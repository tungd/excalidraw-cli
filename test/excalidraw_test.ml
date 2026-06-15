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

let tests =
  [
    ("parse upstream diagram", `Quick, test_parse_upstream_diagram);
    ("parse upstream arrow/line/text", `Quick, test_parse_upstream_arrow_line_text);
    ("render upstream diagram svg", `Quick, test_render_svg_upstream_diagram);
    ("render arrow/line/text svg", `Quick, test_render_svg_arrow_line_text);
    ("render svg contains text", `Quick, test_render_svg_contains_text);
    ("render svg has viewBox", `Quick, test_render_svg_viewbox);
  ]

let () =
  Alcotest.run "excalidraw-export" [
    ("upstream fixtures", tests);
  ]
