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

(* --- Dash array helpers --- *)

let dash_array_of b =
  match b.stroke_style with
  | Solid -> ""
  | Dashed ->
    let w = b.raw_stroke_width in
    Printf.sprintf "stroke-dasharray:%.4g,%.4g" 8.0 (8.0 +. w)
  | Dotted ->
    let w = b.raw_stroke_width in
    Printf.sprintf "stroke-dasharray:%.4g,%.4g" 1.5 (6.0 +. w)

let dash_attr b =
  let da = dash_array_of b in
  if da = "" then [] else [attr "style" da]

(* --- Rough shape helpers --- *)

let rough_transform (b : base_element) =
  Printf.sprintf "translate(%.0f %.0f) rotate(0 %.0f %.0f)"
    b.x b.y (b.width /. 2.0) (b.height /. 2.0)

let rough_path_attrs~stroke_color ~w ~d ~b =
  let parts = [
    attr "d" d;
    attr "stroke" stroke_color;
    float_attr "stroke-width" w;
    attr "fill" "none";
  ] @ (dash_attr b) in
  String.concat " " parts


(* --- Text element --- *)

(* Font metrics used by Excalidraw for each font family.
   The ratios are derived from actual canvas measurements.
   baseline_ratio: y-position of the alphabetic baseline from the top of the line box,
     expressed as a fraction of fontSize.
     For most fonts this is ~0.88-0.9. For Virgil (handwriting) it's ~0.881.
   height_ratio: the actual visual line height divided by the theoretical
     lineHeight * fontSize. For standard fonts this is 1.0.
     For Virgil it's ~1.0145 because the font extends slightly beyond the metrics. *)

type font_metrics = { baseline_ratio : float; height_ratio : float }

let font_metrics_of = function
  | 1 -> { baseline_ratio = 0.881; height_ratio = 1.014525 }
  | _ -> { baseline_ratio = 0.9;  height_ratio = 1.0 }

let render_text_raw (el : text_element) =
  let b = el.base in
  let fm = font_metrics_of el.font_family in
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
  let line_height_px = font_size_f *. el.line_height *. fm.height_ratio in
  (* Baseline y from the top of the element, accounting for font metrics *)
  let baseline_y = b.y +. font_size_f *. fm.baseline_ratio in
  let lines = String.split_on_char '\n' el.text in
  let tspans = List.mapi (fun i line ->
      let ly = baseline_y +. (float_of_int i *. line_height_px) in
      Printf.sprintf "<tspan x=\"%.4g\" y=\"%.4g\">%s</tspan>" text_x ly line
    ) lines in
  let transform = if b.angle <> 0. then
    let cx = b.x +. (b.width /. 2.0) in
    let cy = b.y +. (b.height /. 2.0) in
    Printf.sprintf " transform=\"rotate(%.4g %.4g %.4g)\"" b.angle cx cy
  else "" in
  let text_style =
    if b.opacity <> 100 then
      [attr "style" (Printf.sprintf "opacity:%.2f" (float_of_int b.opacity /. 100.0))]
    else [] in
  let attrs = [
    float_attr "x" text_x;
    float_attr "y" baseline_y;
    attr "font-family" font_family;
    float_attr "font-size" (float_of_int font_size);
    attr "stroke" "none";
    attr "fill" b.stroke_color;
    attr "text-anchor" anchor;
  ] @ text_style @ (if transform <> "" then [transform] else []) in
  mk "text" ~children:(List.map (fun t -> t) tspans) attrs

(* --- Linear (line/arrow) element --- *)

let rough_linear_svg (b : base_element) stroke_paths =
  let style = if b.opacity <> 100 then
    [Printf.sprintf "opacity:%.2f" (float_of_int b.opacity /. 100.0)]
  else [] in
  let transform = Printf.sprintf "translate(%.0f %.0f) rotate(0 %.0f %.0f)"
    b.x b.y (b.width /. 2.0) (b.height /. 2.0) in
  let path_els = List.map (fun d ->
    void_el "path" [rough_path_attrs ~stroke_color:b.stroke_color ~w:b.raw_stroke_width ~d:d ~b]
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
      let h = match el with
        | Text te ->
          let fm = font_metrics_of te.font_family in
          let actual_h = b.height *. fm.height_ratio in
          (* The text extends above the top by (actual_h - stored_h) / 2 *)
          (* Actually: baseline = y + fontSize * baseline_ratio *)
          (* The visual top = baseline - (fontSize * 1) where 1 is the em height *)
          (* Actually from the reference, the visual bounds are:
             top = y - (fontSize * baseline_ratio - fontSize) = y + fontSize * (1 - baseline_ratio) 
             ... this is getting complex. Let's just use the height ratio directly. *)
          actual_h
        | _ -> b.height in
      (min minx b.x, min miny b.y, max maxx (b.x +. b.width), max maxy (b.y +. h))
    ) init elements

let render_element_raw = function
  | Rectangle el ->
    let fill_style = match el.base.fill_style with Solid -> "solid" | Hachure -> "hachure" | Cross_hatch -> "cross-hatch" in
    let (stroke_d, fill_d) = Rough.rectangle ~fill_style ~stroke_style:(match el.base.stroke_style with Solid -> "solid" | Dashed -> "dashed" | Dotted -> "dotted") ~roundness:(match el.base.roundness with Some r -> (match r.value with Some v -> v | None -> 0.0) | None -> 0.0) ~seed:el.base.seed ~roughness:el.base.roughness ~stroke_width:el.base.raw_stroke_width ~width:el.base.width ~height:el.base.height () in
    let b = el.base in
    let style = if b.opacity <> 100 then
      [Printf.sprintf "opacity:%.2f" (float_of_int b.opacity /. 100.0)]
    else [] in
    let attrs = [
      attr "stroke-linecap" "round";
      Printf.sprintf "transform=\"%s\"" (rough_transform b);
    ] @ (match style with [] -> [] | _ -> [Printf.sprintf " style=\"%s\"" (String.concat ";" style)]) in
    let stroke_path = void_el "path" [rough_path_attrs ~stroke_color:b.stroke_color ~w:b.raw_stroke_width ~d:stroke_d ~b] in
    if fill_style = "solid" && fill_color b <> "none" then begin
      let fill_shape = void_el "rect" [
        float_attr "x" 0.; float_attr "y" 0.;
        float_attr "width" b.width; float_attr "height" b.height;
        attr "fill" (fill_color b);
      ] in
      mk "g" ~children:[fill_shape; stroke_path] attrs
    end else if fill_d <> "" then begin
      let fill_path = void_el "path" [rough_path_attrs ~stroke_color:(fill_color b) ~w:(b.raw_stroke_width /. 2.0) ~d:fill_d ~b] in
      mk "g" ~children:[fill_path; stroke_path] attrs
    end else
      mk "g" ~children:[stroke_path] attrs
  | Ellipse el ->
    let fill_style = match el.base.fill_style with Solid -> "solid" | Hachure -> "hachure" | Cross_hatch -> "cross-hatch" in
    let (stroke_d, fill_d) = Rough.ellipse ~fill_style ~stroke_style:(match el.base.stroke_style with Solid -> "solid" | Dashed -> "dashed" | Dotted -> "dotted") ~seed:el.base.seed ~roughness:el.base.roughness ~stroke_width:el.base.raw_stroke_width ~width:el.base.width ~height:el.base.height () in
    let b = el.base in
    let style = if b.opacity <> 100 then
      [Printf.sprintf "opacity:%.2f" (float_of_int b.opacity /. 100.0)]
    else [] in
    let attrs = [
      attr "stroke-linecap" "round";
      Printf.sprintf "transform=\"%s\"" (rough_transform b);
    ] @ (match style with [] -> [] | _ -> [Printf.sprintf " style=\"%s\"" (String.concat ";" style)]) in
    let stroke_path = void_el "path" [rough_path_attrs ~stroke_color:b.stroke_color ~w:b.raw_stroke_width ~d:stroke_d ~b] in
    if fill_style = "solid" && fill_color b <> "none" then begin
      let fill_shape = void_el "ellipse" [
        float_attr "cx" (b.width /. 2.); float_attr "cy" (b.height /. 2.);
        float_attr "rx" (b.width /. 2.); float_attr "ry" (b.height /. 2.);
        attr "fill" (fill_color b);
      ] in
      mk "g" ~children:[fill_shape; stroke_path] attrs
    end else if fill_d <> "" then begin
      let fill_path = void_el "path" [rough_path_attrs ~stroke_color:(fill_color b) ~w:(b.raw_stroke_width /. 2.0) ~d:fill_d ~b] in
      mk "g" ~children:[fill_path; stroke_path] attrs
    end else
      mk "g" ~children:[stroke_path] attrs
  | Diamond el ->
    let fill_style = match el.base.fill_style with Solid -> "solid" | Hachure -> "hachure" | Cross_hatch -> "cross-hatch" in
    let (stroke_d, fill_d) = Rough.diamond ~fill_style ~stroke_style:(match el.base.stroke_style with Solid -> "solid" | Dashed -> "dashed" | Dotted -> "dotted") ~seed:el.base.seed ~roughness:el.base.roughness ~stroke_width:el.base.raw_stroke_width ~width:el.base.width ~height:el.base.height () in
    let b = el.base in
    let style = if b.opacity <> 100 then
      [Printf.sprintf "opacity:%.2f" (float_of_int b.opacity /. 100.0)]
    else [] in
    let attrs = [
      attr "stroke-linecap" "round";
      Printf.sprintf "transform=\"%s\"" (rough_transform b);
    ] @ (match style with [] -> [] | _ -> [Printf.sprintf " style=\"%s\"" (String.concat ";" style)]) in
    let stroke_path = void_el "path" [rough_path_attrs ~stroke_color:b.stroke_color ~w:b.raw_stroke_width ~d:stroke_d ~b] in
    if fill_style = "solid" && fill_color b <> "none" then begin
      let cx = b.width /. 2. in
      let cy = b.height /. 2. in
      let fill_shape = void_el "polygon" [
        attr "points" (Printf.sprintf "%.4g,%.4g %.4g,%.4g %.4g,%.4g %.4g,%.4g"
          cx 0. (b.width) cy cx (b.height) 0. cy);
        attr "fill" (fill_color b);
      ] in
      mk "g" ~children:[fill_shape; stroke_path] attrs
    end else if fill_d <> "" then begin
      let fill_path = void_el "path" [rough_path_attrs ~stroke_color:(fill_color b) ~w:(b.raw_stroke_width /. 2.0) ~d:fill_d ~b] in
      mk "g" ~children:[fill_path; stroke_path] attrs
    end else
      mk "g" ~children:[stroke_path] attrs
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
  pf " viewBox=\"0 0 %.15g %.15g\"" vb_w vb_h;
  pf " width=\"%.15g\" height=\"%.15g\">" (vb_w *. scale) (vb_h *. scale);
  pf "<!-- svg-source:excalidraw --><metadata></metadata><defs><style class=\"style-fonts\">\n      </style></defs>";
  (* Background *)
  pf "<rect x=\"0\" y=\"0\" width=\"%.15g\" height=\"%.15g\" fill=\"%s\"></rect>" (vb_w *. scale) (vb_h *. scale) bg_color;
  (* Elements *)
  List.iter (fun el ->
    Buffer.add_string buf (render_element_raw el)
  ) elements;
  pf "</svg>";
  Buffer.contents buf
