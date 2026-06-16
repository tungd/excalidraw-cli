open Types

module S = Tyxml.Svg

let px n = (n, Some `Px)

let fill_color_of b =
  if b.background_color = "" || b.background_color = "transparent"
  then `None else `Color (b.background_color, None)

let opacity_style b =
  Printf.sprintf "opacity:%.2f" (float_of_int b.opacity /. 100.0)

let dash_style b = match b.stroke_style with
  | Solid -> ""
  | Dashed -> "stroke-dasharray:8,4"
  | Dotted -> "stroke-dasharray:2,3"

let style_attr b =
  let parts = [ opacity_style b; dash_style b ] in
  let s = String.concat ";" (List.filter (fun x -> x <> "") parts) in
  if s = "" then [] else [ S.a_style s ]

let transform_attr (b : base_element) =
  if b.angle = 0. then []
  else
    let cx = b.x +. (b.width /. 2.0) in
    let cy = b.y +. (b.height /. 2.0) in
    [ S.a_transform [ `Rotate ((b.angle, Some `Deg), Some (cx, cy)) ] ]

let stroke_paint b = `Color (b.stroke_color, None)

let rough_transform (b : base_element) =
  [ S.a_transform [ `Translate (b.x, Some b.y); `Rotate ((b.angle, Some `Deg), Some (b.width /. 2.0, b.height /. 2.0)) ] ]

let path_attr b d = [
  S.a_d d;
  S.a_stroke (stroke_paint b);
  S.a_stroke_width (px b.raw_stroke_width);
  S.a_fill `None;
  S.a_stroke_linecap `Round;
]

let fill_path_attr b d = [
  S.a_d d;
  S.a_stroke (fill_color_of b);
  S.a_stroke_width (px (b.raw_stroke_width /. 2.0));
  S.a_fill `None;
  S.a_stroke_linecap `Round;
]

(* --- Element renderers --- *)
let render_rectangle (el : rectangle_element) =
  let b = el.base in
  let stroke_d, fill_d = Rough.rectangle ~seed:b.seed ~roughness:b.roughness ~stroke_width:b.raw_stroke_width ~width:b.width ~height:b.height in
  let fill_path = S.path ~a:(fill_path_attr b fill_d) [] in
  let stroke_path = S.path ~a:(path_attr b stroke_d) [] in
  S.g ~a:(rough_transform b @ style_attr b) [ fill_path; stroke_path ]

let render_ellipse (el : ellipse_element) =
  let b = el.base in
  let stroke_d, fill_d = Rough.ellipse ~seed:b.seed ~roughness:b.roughness ~stroke_width:b.raw_stroke_width ~width:b.width ~height:b.height in
  let fill_path = S.path ~a:(fill_path_attr b fill_d) [] in
  let stroke_path = S.path ~a:(path_attr b stroke_d) [] in
  S.g ~a:(rough_transform b @ style_attr b) [ fill_path; stroke_path ]

let render_diamond (el : diamond_element) =
  let b = el.base in
  let stroke_d, fill_d = Rough.diamond ~seed:b.seed ~roughness:b.roughness ~stroke_width:b.raw_stroke_width ~width:b.width ~height:b.height in
  let fill_path = S.path ~a:(fill_path_attr b fill_d) [] in
  let stroke_path = S.path ~a:(path_attr b stroke_d) [] in
  S.g ~a:(rough_transform b @ style_attr b) [ fill_path; stroke_path ]

let render_text (el : text_element) =
  let b = el.base in
  let font_family = match el.font_family with
    | 1 -> "Virgil, Hand-drawn, cursive"
    | 2 -> "Cascadia, Helvetica, Arial, sans-serif"
    | 3 -> "Fira Code, monospace"
    | _ -> "Hand-drawn, cursive" in
  let anchor_attr = match el.text_align with
    | Left -> [ S.a_text_anchor `Start ]
    | Center -> [ S.a_text_anchor `Middle ]
    | Right -> [ S.a_text_anchor `End ] in
  let text_x = match el.text_align with
    | Left -> b.x
    | Center -> b.x +. (b.width /. 2.0)
    | Right -> b.x +. b.width in
  let font_size_f = float_of_int el.font_size in
  let line_height_px = font_size_f *. el.line_height in
  let lines = String.split_on_char '\n' el.text in
  let tspans = List.mapi (fun i line ->
      let ly = b.y +. font_size_f +. (float_of_int i *. line_height_px) in
      S.tspan ~a:[ S.a_x_list [ px text_x ]; S.a_y_list [ px ly ] ] [ S.txt line ]
    ) lines in
  let inner = S.text ~a:([
    S.a_x_list [ px text_x ]; S.a_y_list [ px (b.y +. font_size_f) ];
    S.a_font_family font_family; S.a_font_size (string_of_int el.font_size);
    S.a_stroke `None; S.a_fill (stroke_paint b);
  ] @ anchor_attr @ transform_attr b) tspans in
  S.g ~a:(style_attr b) [ inner ]

let render_linear ~is_arrow (el : linear_element) =
  let b = el.base in
  let d_str = match el.points with
    | [] -> Printf.sprintf "M%.2f,%.2f L%.2f,%.2f" b.x b.y (b.x +. b.width) (b.y +. b.height)
    | pts ->
      let (fx, fy) = List.hd pts in
      let segs = List.tl pts |> List.map (fun (px2, py2) ->
          Printf.sprintf "L%.2f,%.2f" (b.x +. px2) (b.y +. py2)
        ) in
      Printf.sprintf "M%.2f,%.2f%s" (b.x +. fx) (b.y +. fy) (String.concat "" segs) in
  let extra = if is_arrow then "marker-end:url(#arrowhead)" else "" in
  let style_parts = [ opacity_style b; dash_style b; extra ] in
  let s = String.concat ";" (List.filter (fun x -> x <> "") style_parts) in
  S.path ~a:([
    S.a_d (d_str); S.a_stroke (stroke_paint b);
    S.a_stroke_width (px b.raw_stroke_width); S.a_fill `None;
  ] @ [ S.a_style s ] @ transform_attr b) []

let render_frame (el : frame_element) =
  let b = el.base in
  S.rect ~a:[
    S.a_x (px b.x); S.a_y (px b.y);
    S.a_width (px b.width); S.a_height (px b.height);
    S.a_stroke (`Color ("#aaaaaa", None)); S.a_stroke_width (px 1.0);
    S.a_fill `None; S.a_rx (px 4.); S.a_ry (px 4.);
    S.a_style ("stroke-dasharray:4,2;opacity:0.8");
  ] []

let base_of_element = function
  | Rectangle e -> e.base | Ellipse e -> e.base | Diamond e -> e.base
  | Text e -> e.base | Line e -> e.base | Arrow e -> e.base
  | Image e -> e.base | Frame e -> e.base
  | Unknown _ ->
    { id=""; x=0.; y=0.; width=0.; height=0.; angle=0.;
      stroke_color=""; background_color=""; fill_style=Solid;
      stroke_width=Normal; raw_stroke_width=1.0; stroke_style=Solid; roughness=0; opacity=100;
      group_ids=[]; frame_id=None; roundness=None; seed=0; version=0;
      version_nonce=0; is_deleted=false; updated=0 }

let translate_base (b : base_element) dx dy =
  { b with x = b.x +. dx; y = b.y +. dy }

let translate_element dx dy = function
  | Rectangle el -> Rectangle { el with base = translate_base el.base dx dy }
  | Ellipse el -> Ellipse { el with base = translate_base el.base dx dy }
  | Diamond el -> Diamond { el with base = translate_base el.base dx dy }
  | Text el -> Text { el with base = translate_base el.base dx dy }
  | Line el -> Line { el with base = translate_base el.base dx dy }
  | Arrow el -> Arrow { el with base = translate_base el.base dx dy }
  | Image el -> Image { el with base = translate_base el.base dx dy }
  | Frame el -> Frame { el with base = translate_base el.base dx dy }
  | Unknown _ as el -> el

let compute_bbox elements =
  let init = (max_float, max_float, min_float, min_float) in
  List.fold_left (fun (minx, miny, maxx, maxy) el ->
      let b = base_of_element el in
      (min minx b.x, min miny b.y, max maxx (b.x +. b.width), max maxy (b.y +. b.height))
    ) init elements

let has_arrows elements =
  List.exists (function Arrow _ -> true | _ -> false) elements

let render_element = function
  | Rectangle el -> render_rectangle el
  | Ellipse el -> render_ellipse el
  | Diamond el -> render_diamond el
  | Text el -> render_text el
  | Line el -> render_linear ~is_arrow:false el
  | Arrow el -> render_linear ~is_arrow:true el
  | Frame el -> render_frame el
  | Image _ | Unknown _ -> S.g []

let render ?(scale=1.0) doc =
  let padding = 10.0 in
  let (minx, miny, maxx, maxy) =
    if doc.elements = [] then (0.0, 0.0, 400.0, 300.0)
    else compute_bbox doc.elements in
  let vb_w = (maxx -. minx) +. (2.0 *. padding) in
  let vb_h = (maxy -. miny) +. (2.0 *. padding) in
  let offset_x = -.minx +. padding in
  let offset_y = -.miny +. padding in
  let elements = List.map (translate_element offset_x offset_y) doc.elements in
  let bg_color = doc.app_state.view_background_color in
  let defs =
    if has_arrows elements then [
      S.defs ~a:[] [
        S.marker ~a:[
          S.a_id ("arrowhead");
          S.a_markerWidth (px 10.); S.a_markerHeight (px 7.);
          S.a_refX (px 10.); S.a_refY (px 3.5);
          S.a_orient (Some (0., Some `Deg));
        ] [
          S.polygon ~a:[
            S.a_points [ (0., 0.); (10., 3.5); (0., 7.) ];
            S.a_fill `None;
            S.a_stroke (`Color ("context-stroke", None));
          ] [];
        ];
      ]
    ] else [] in
  let bg_rect =
    S.rect ~a:[
      S.a_x (px 0.); S.a_y (px 0.);
      S.a_width (px vb_w); S.a_height (px vb_h);
      S.a_fill (`Color (bg_color, None));
    ] [] in
  let children = defs @ [ bg_rect ] @ List.map render_element elements in
  let svg_doc =
    S.svg ~a:[
      S.a_viewBox (0., 0., vb_w, vb_h);
      S.a_width (px (vb_w *. scale));
      S.a_height (px (vb_h *. scale));
    ] children in
  let xml_decl = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" in
  let buf = Buffer.create 4096 in
  let fmt = Format.formatter_of_buffer buf in
  Tyxml.Svg.pp ~indent:true () fmt svg_doc;
  Format.pp_print_flush fmt ();
  xml_decl ^ Buffer.contents buf
