open Types

(* --- Colors, style, transforms --- *)

let fill_color b =
  if b.background_color = "" || b.background_color = "transparent"
  then "none"
  else b.background_color

(* --- Raw SVG element helpers --- *)

let attr key value = Printf.sprintf "%s=\"%s\"" key value

let float_attr key v = attr key (Printf.sprintf "%.4g" v)

let attrs_to_string attrs =
  String.concat " " (List.filter (fun x -> x <> "") attrs)

let mk name ?(children=[]) attrs =
  let a_str = attrs_to_string attrs in
  let children_str = String.concat "" children in
  if a_str = "" then Printf.sprintf "<%s>%s</%s>" name children_str name
  else Printf.sprintf "<%s %s>%s</%s>" name a_str children_str name

let void_el name attrs =
  let a_str = attrs_to_string attrs in
  if a_str = "" then Printf.sprintf "<%s/>" name
  else Printf.sprintf "<%s %s/>" name a_str

(* --- Rough shape helpers --- *)

let rough_transform (b : base_element) =
  Printf.sprintf "translate(%.0f %.0f) rotate(0 %.0f %.0f)"
    b.x b.y (b.width /. 2.0) (b.height /. 2.0)

let rough_path_attrs stroke_color w d =
  let parts = [
    attr "d" d;
    attr "stroke" stroke_color;
    float_attr "stroke-width" w;
    attr "fill" "none";
    attr "stroke-linecap" "round";
  ] in
  String.concat " " parts

let rough_svg b fill_d stroke_d =
  let fill_path = void_el "path" [rough_path_attrs (fill_color b) (b.raw_stroke_width /. 2.0) fill_d] in
  let stroke_path = void_el "path" [rough_path_attrs b.stroke_color b.raw_stroke_width stroke_d] in
  let style = if b.opacity <> 100 then
    [Printf.sprintf "opacity:%.2f" (float_of_int b.opacity /. 100.0)]
  else [] in
  let all_attrs = [
    attr "stroke-linecap" "round";
    Printf.sprintf "transform=\"%s\"" (rough_transform b);
  ] @ (match style with [] -> [] | _ -> [Printf.sprintf " style=\"%s\"" (String.concat ";" style)]) in
  mk "g" ~children:[fill_path; stroke_path] all_attrs

(* --- Text element --- *)

let render_text_raw (el : text_element) =
  let b = el.base in
  let font_family = match el.font_family with
    | 1 -> "Virgil, Hand-drawn, cursive"
    | 2 -> "Cascadia, Helvetica, Arial, sans-serif"
    | 3 -> "Fira Code, monospace"
    | _ -> "Hand-drawn, cursive" in
  let anchor = match el.text_align with
    | Left -> "start" | Center -> "middle" | Right -> "end" in
  let text_x = match el.text_align with
    | Left -> b.x | Center -> b.x +. (b.width /. 2.0) | Right -> b.x +. b.width in
  let font_size = el.font_size in
  let font_size_f = float_of_int font_size in
  let line_height_px = font_size_f *. el.line_height in
  let lines = String.split_on_char '\n' el.text in
  let tspans = List.mapi (fun i line ->
      let ly = b.y +. font_size_f +. (float_of_int i *. line_height_px) in
      Printf.sprintf "<tspan x=\"%.4g\" y=\"%.4g\">%s</tspan>" text_x ly line
    ) lines in
  let transform = if b.angle <> 0. then
    let cx = b.x +. (b.width /. 2.0) in
    let cy = b.y +. (b.height /. 2.0) in
    Printf.sprintf " transform=\"rotate(%.4g %.4g %.4g)\"" b.angle cx cy
  else "" in
  let attrs = [
    float_attr "x" text_x;
    float_attr "y" (b.y +. font_size_f);
    attr "font-family" font_family;
    float_attr "font-size" (float_of_int font_size);
    attr "stroke" "none";
    attr "fill" b.stroke_color;
    attr "text-anchor" anchor;
    attr "style" (Printf.sprintf "opacity:%.2f" (float_of_int b.opacity /. 100.0));
  ] @ (if transform <> "" then [transform] else []) in
  mk "text" ~children:(List.map (fun t -> t) tspans) attrs

(* --- Linear (line/arrow) element --- *)

let rough_linear_svg (b : base_element) stroke_paths =
  let style = if b.opacity <> 100 then
    [Printf.sprintf "opacity:%.2f" (float_of_int b.opacity /. 100.0)]
  else [] in
  let transform = Printf.sprintf "translate(%.0f %.0f) rotate(0 %.0f %.0f)"
    b.x b.y (b.width /. 2.0) (b.height /. 2.0) in
  let path_els = List.map (fun d ->
    void_el "path" [rough_path_attrs b.stroke_color b.raw_stroke_width d]
  ) stroke_paths in
  let all_attrs = [
    attr "stroke-linecap" "round";
    Printf.sprintf "transform=\"%s\"" transform;
  ] @ (match style with [] -> [] | _ -> [Printf.sprintf " style=\"%s\"" (String.concat ";" style)]) in
  mk "g" ~children:path_els all_attrs

let render_linear_raw ~is_arrow (el : linear_element) =
  let b = el.base in
  let points = match el.points with
    | [] -> [(0.0, 0.0); (b.width, b.height)]
    | pts -> pts
  in
  let stroke_d = Rough.line
    ~seed:b.seed ~roughness:b.roughness
    ~stroke_width:b.raw_stroke_width
    ~points
  in
  let arrowhead_paths =
    if is_arrow then begin
      let n = List.length points in
      let (px1, py1) = if n >= 2 then List.nth points (n - 2) else (0.0, 0.0) in
      let (px2, py2) = List.nth points (n - 1) in
      let end_paths = match el.end_arrowhead with
        | Some "arrow" ->
          Rough.arrowhead ~seed:b.seed ~roughness:b.roughness
            ~stroke_width:b.raw_stroke_width
            ~x1:px1 ~y1:py1 ~x2:px2 ~y2:py2
        | _ -> []
      in
      let start_paths = match el.start_arrowhead with
        | Some "arrow" ->
          let (sx1, sy1) = List.hd points in
          let (sx2, sy2) = if n >= 2 then List.nth points 1 else (0.0, 0.0) in
          Rough.arrowhead ~seed:b.seed ~roughness:b.roughness
            ~stroke_width:b.raw_stroke_width
            ~x1:sx2 ~y1:sy2 ~x2:sx1 ~y2:sy1
        | _ -> []
      in
      end_paths @ start_paths
    end else []
  in
  let all_paths = stroke_d :: arrowhead_paths in
  rough_linear_svg b all_paths

(* --- Frame element --- *)

let render_frame_raw (el : frame_element) =
  let b = el.base in
  let attrs = [
    float_attr "x" b.x; float_attr "y" b.y;
    float_attr "width" b.width; float_attr "height" b.height;
    attr "stroke" "#aaaaaa"; float_attr "stroke-width" 1.0;
    attr "fill" "none";
    float_attr "rx" 4.0; float_attr "ry" 4.0;
    attr "style" "stroke-dasharray:4,2;opacity:0.8";
  ] in
  void_el "rect" attrs

(* --- Element dispatch --- *)

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

let render_element_raw = function
  | Rectangle el ->
    let (stroke_d, fill_d) = Rough.rectangle ~seed:el.base.seed ~roughness:el.base.roughness ~stroke_width:el.base.raw_stroke_width ~width:el.base.width ~height:el.base.height in
    rough_svg el.base fill_d stroke_d
  | Ellipse el ->
    let (stroke_d, fill_d) = Rough.ellipse ~seed:el.base.seed ~roughness:el.base.roughness ~stroke_width:el.base.raw_stroke_width ~width:el.base.width ~height:el.base.height in
    rough_svg el.base fill_d stroke_d
  | Diamond el ->
    let (stroke_d, fill_d) = Rough.diamond ~seed:el.base.seed ~roughness:el.base.roughness ~stroke_width:el.base.raw_stroke_width ~width:el.base.width ~height:el.base.height in
    rough_svg el.base fill_d stroke_d
  | Text el -> render_text_raw el
  | Line el -> render_linear_raw ~is_arrow:false el
  | Arrow el -> render_linear_raw ~is_arrow:true el
  | Frame el -> render_frame_raw el
  | Image _ | Unknown _ -> ""

(* --- Main render function --- *)

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
  let buf = Buffer.create 4096 in
  let pf fmt = Printf.ksprintf (Buffer.add_string buf) fmt in
  pf "<svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\"";
  pf " viewBox=\"0 0 %.4g %.4g\"" vb_w vb_h;
  pf " width=\"%.4g\" height=\"%.4g\">" (vb_w *. scale) (vb_h *. scale);
  (* Background *)
  pf "<rect x=\"0\" y=\"0\" width=\"%.4g\" height=\"%.4g\" fill=\"%s\"></rect>" (vb_w *. scale) (vb_h *. scale) bg_color;
  (* Elements *)
  List.iter (fun el ->
    Buffer.add_string buf (render_element_raw el)
  ) elements;
  pf "</svg>";
  Buffer.contents buf
