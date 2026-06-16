(* Seeded PRNG and rough path generation matching rough.js algorithm.
   The PRNG uses Math.imul(48271, seed) & 0x7FFFFFFF (Lehmer/Park-Miller LCG).
   Path generation follows rough.js's _singleLine, _doubleLine, and hachure fillers. *)

(* --- PRNG matching rough.js Random class --- *)
(* rough.js: (2^31 - 1) & (Math.imul(48271, this.seed)) / 2^31 *)

type rng = { mutable state : int }

let make_rng seed =
  let s = if seed = 0 then 1 else seed in
  { state = s }

let next rng =
  let s32 = Int32.mul (Int32.of_int 48271) (Int32.of_int rng.state) in
  let s32 = Int32.logand s32 0x7FFFFFFFl in
  let s = Int32.to_int s32 in
  rng.state <- s;
  float s /. 2147483648.0

(* --- Options type matching rough.js ResolvedOptions --- *)

type options = {
  rng : rng;
  maxRandomnessOffset : float;
  roughness : float;
  bowing : float;
  strokeWidth : float;
  fillWeight : float;
  hachureGap : float;
  hachureAngle : float;
  fillStyle : string;
  disableMultiStroke : bool;
  disableMultiStrokeFill : bool;
  preserveVertices : bool;
  curveStepCount : int;
  curveTightness : float;
}

let default_options ~seed ~roughness ~stroke_width =
  let rng = make_rng seed in
  {
    rng;
    maxRandomnessOffset = roughness *. 1.5;
    roughness;
    bowing = roughness *. 1.0;
    strokeWidth = stroke_width;
    fillWeight = stroke_width /. 2.0;
    hachureGap = stroke_width *. 4.0;
    hachureAngle = -41.0;
    fillStyle = "hachure";
    disableMultiStroke = false;
    disableMultiStrokeFill = false;
    preserveVertices = false;
    curveStepCount = 9;
    curveTightness = 0.0;
  }

(* --- Offset functions matching rough.js --- *)

let random o =
  o.maxRandomnessOffset *. (2.0 *. next o.rng -. 1.0)

(* --- Path operation types matching rough.js Op/OpSet --- *)

type op =
  | Move of float * float
  | BcurveTo of float * float * float * float * float * float
  | LineTo of float * float

type opset = {
  typ : string;
  ops : op list;
}

(* --- Core line drawing matching rough.js _singleLine --- *)

let _single_line x1 y1 x2 y2 o ~move ~overlay =
  let length_sq = (x2 -. x1) ** 2.0 +. (y2 -. y1) ** 2.0 in
  let length = sqrt length_sq in
  let _roughness_gain =
    if length > 500.0 then 0.4
    else if length > 0.0 then (-.0.0016668) *. length +. 1.233334
    else 1.0
  in
  let offset = o.maxRandomnessOffset in
  let offset =
    if offset *. offset *. 100.0 > length_sq
    then length /. 10.0
    else offset
  in
  let half_offset = offset /. 2.0 in
  let diverge_point = 0.2 +. random o *. 0.2 in
  let mid_disp_x = o.bowing *. o.maxRandomnessOffset *. (y2 -. y1) /. 200.0 in
  let mid_disp_y = o.bowing *. o.maxRandomnessOffset *. (x1 -. x2) /. 200.0 in
  let mid_disp_x = mid_disp_x *. (2.0 *. next o.rng -. 1.0) in
  let mid_disp_y = mid_disp_y *. (2.0 *. next o.rng -. 1.0) in
  let ops = ref [] in
  if move then begin
    if overlay then
      ops := Move (
        x1 +. (if o.preserveVertices then 0.0 else half_offset *. (2.0 *. next o.rng -. 1.0)),
        y1 +. (if o.preserveVertices then 0.0 else half_offset *. (2.0 *. next o.rng -. 1.0))
      ) :: !ops
    else
      ops := Move (
        x1 +. (if o.preserveVertices then 0.0 else offset *. (2.0 *. next o.rng -. 1.0)),
        y1 +. (if o.preserveVertices then 0.0 else offset *. (2.0 *. next o.rng -. 1.0))
      ) :: !ops
  end;
  if overlay then begin
    ops := BcurveTo (
      mid_disp_x +. x1 +. (x2 -. x1) *. diverge_point +. half_offset *. (2.0 *. next o.rng -. 1.0),
      mid_disp_y +. y1 +. (y2 -. y1) *. diverge_point +. half_offset *. (2.0 *. next o.rng -. 1.0),
      mid_disp_x +. x1 +. 2.0 *. (x2 -. x1) *. diverge_point +. half_offset *. (2.0 *. next o.rng -. 1.0),
      mid_disp_y +. y1 +. 2.0 *. (y2 -. y1) *. diverge_point +. half_offset *. (2.0 *. next o.rng -. 1.0),
      x2 +. (if o.preserveVertices then 0.0 else half_offset *. (2.0 *. next o.rng -. 1.0)),
      y2 +. (if o.preserveVertices then 0.0 else half_offset *. (2.0 *. next o.rng -. 1.0))
    ) :: !ops
  end else begin
    ops := BcurveTo (
      mid_disp_x +. x1 +. (x2 -. x1) *. diverge_point +. offset *. (2.0 *. next o.rng -. 1.0),
      mid_disp_y +. y1 +. (y2 -. y1) *. diverge_point +. offset *. (2.0 *. next o.rng -. 1.0),
      mid_disp_x +. x1 +. 2.0 *. (x2 -. x1) *. diverge_point +. offset *. (2.0 *. next o.rng -. 1.0),
      mid_disp_y +. y1 +. 2.0 *. (y2 -. y1) *. diverge_point +. offset *. (2.0 *. next o.rng -. 1.0),
      x2 +. (if o.preserveVertices then 0.0 else offset *. (2.0 *. next o.rng -. 1.0)),
      y2 +. (if o.preserveVertices then 0.0 else offset *. (2.0 *. next o.rng -. 1.0))
    ) :: !ops
  end;
  List.rev !ops

let _double_line x1 y1 x2 y2 o =
  let pass1 = _single_line x1 y1 x2 y2 o ~move:true ~overlay:false in
  if o.disableMultiStroke then pass1
  else pass1 @ _single_line x1 y1 x2 y2 o ~move:true ~overlay:true

(* --- Rectangle --- *)

let rectangle_ops x y width height o =
  let points = [
    (x, y);
    (x +. width, y);
    (x +. width, y +. height);
    (x, y +. height);
  ] in
  let ops = ref [] in
  let pts = points @ [List.hd points] in
  for i = 0 to List.length points - 1 do
    let (x1, y1) = List.nth pts i in
    let (x2, y2) = List.nth pts (i + 1) in
    ops := !ops @ _double_line x1 y1 x2 y2 o
  done;
  [{ typ = "path"; ops = !ops }]

(* --- Ellipse --- *)

let ellipse_ops cx cy width height o =
  let rx = width /. 2.0 in
  let ry = height /. 2.0 in
  let increment = (2.0 *. Float.pi) /. float_of_int o.curveStepCount in
  let points = ref [] in
  let angle = ref 0.0 in
  while !angle < 2.0 *. Float.pi do
    points := (cx +. rx *. cos !angle, cy +. ry *. sin !angle) :: !points;
    angle := !angle +. increment
  done;
  let points = List.rev !points in
  let ops = ref [] in
  let n = List.length points in
  for i = 0 to n - 1 do
    let (x1, y1) = List.nth points i in
    let (x2, y2) = List.nth points ((i + 1) mod n) in
    ops := !ops @ _double_line x1 y1 x2 y2 o
  done;
  [{ typ = "path"; ops = !ops }]

(* --- Diamond --- *)

let diamond_ops x y width height o =
  let cx = x +. width /. 2.0 in
  let cy = y +. height /. 2.0 in
  let points = [
    (cx, y);
    (x +. width, cy);
    (cx, y +. height);
    (x, cy);
  ] in
  let ops = ref [] in
  let pts = points @ [List.hd points] in
  for i = 0 to 3 do
    let (x1, y1) = List.nth pts i in
    let (x2, y2) = List.nth pts (i + 1) in
    ops := !ops @ _double_line x1 y1 x2 y2 o
  done;
  [{ typ = "path"; ops = !ops }]

(* --- Hachure fill matching rough.js hachure filler --- *)

let rotate_points points angle =
  let cos_a = cos angle in
  let sin_a = sin angle in
  List.map (fun (x, y) ->
    (x *. cos_a -. y *. sin_a,
     x *. sin_a +. y *. cos_a)
  ) points

let hachure_fill polygon_list o =
  let gap = o.hachureGap in
  if gap <= 0.0 then []
  else begin
    (* Convert hachure angle to radians *)
    let angle_rad = o.hachureAngle *. Float.pi /. 180.0 in
    (* Rotate all polygons by the hachure angle *)
    let rotated_polygons = List.map (fun polygon -> rotate_points polygon angle_rad) polygon_list in
    let min_y = ref max_float in
    let max_y = ref min_float in
    List.iter (fun polygon ->
      List.iter (fun (_, y) ->
        begin
          min_y := min !min_y y;
          max_y := max !max_y y
        end
      ) polygon
    ) rotated_polygons;
    let ops = ref [] in
    let y = ref (!min_y +. gap) in
    while !y < !max_y do
      let intersections = ref [] in
      List.iter (fun polygon ->
        let n = List.length polygon in
        for i = 0 to n - 1 do
          let (x1, y1) = List.nth polygon i in
          let (x2, y2) = List.nth polygon ((i + 1) mod n) in
          if (y1 <= !y && y2 > !y) || (y2 <= !y && y1 > !y) then begin
            let t = (!y -. y1) /. (y2 -. y1) in
            let ix = x1 +. t *. (x2 -. x1) in
            intersections := ix :: !intersections
          end
        done
      ) rotated_polygons;
      let intersections = List.sort compare !intersections in
      let rec draw_pairs = function
        | x1 :: x2 :: rest ->
          (* Draw the line in rotated space, then rotate back *)
          let cos_a = cos (-. angle_rad) in
          let sin_a = sin (-. angle_rad) in
          let y_f = !y in
          let rx1 = x1 *. cos_a -. y_f *. sin_a in
          let ry1 = x1 *. sin_a +. y_f *. cos_a in
          let rx2 = x2 *. cos_a -. y_f *. sin_a in
          let ry2 = x2 *. sin_a +. y_f *. cos_a in
          let line_ops = _double_line rx1 ry1 rx2 ry2 o in
          ops := !ops @ line_ops;
          draw_pairs rest
        | _ -> ()
      in
      draw_pairs intersections;
      y := !y +. gap
    done;
    [{ typ = "fillSketch"; ops = !ops }]
  end

(* --- Convert opset to SVG path d attribute string --- *)

let ops_to_path ops =
  let buf = Buffer.create 1024 in
  List.iter (function
    | Move (x, y) ->
      Buffer.add_string buf (Printf.sprintf "M%.2f %.2f " x y)
    | BcurveTo (cp1x, cp1y, cp2x, cp2y, ex, ey) ->
      Buffer.add_string buf (Printf.sprintf "C%.2f %.2f, %.2f %.2f, %.2f %.2f " cp1x cp1y cp2x cp2y ex ey)
    | LineTo (x, y) ->
      Buffer.add_string buf (Printf.sprintf "L%.2f %.2f " x y)
  ) ops;
  let s = Buffer.contents buf in
  let len = String.length s in
  if len > 0 && s.[len - 1] = ' ' then String.sub s 0 (len - 1) else s

let opset_to_path os = ops_to_path os.ops

(* --- Public API: shape generators returning (stroke_path, fill_path) --- *)

let rectangle ~seed ~roughness ~stroke_width ~width ~height =
  let o = default_options ~seed ~roughness:(float roughness) ~stroke_width in
  let stroke_opsets = rectangle_ops 0.0 0.0 width height o in
  let polygon = [(0.0, 0.0); (width, 0.0); (width, height); (0.0, height)] in
  let fill_opsets = hachure_fill [polygon] o in
  let stroke_d = String.concat " " (List.map opset_to_path stroke_opsets) in
  let fill_d = String.concat " " (List.map opset_to_path fill_opsets) in
  (stroke_d, fill_d)

let ellipse ~seed ~roughness ~stroke_width ~width ~height =
  let o = default_options ~seed ~roughness:(float roughness) ~stroke_width in
  let cx = width /. 2.0 in
  let cy = height /. 2.0 in
  let stroke_opsets = ellipse_ops cx cy width height o in
  let n = 20 in
  let rx = width /. 2.0 in
  let ry = height /. 2.0 in
  let polygon = Array.to_list (Array.init n (fun i ->
    let angle = float i *. (2.0 *. Float.pi) /. float n in
    (cx +. rx *. cos angle, cy +. ry *. sin angle)
  )) in
  let fill_opsets = hachure_fill [polygon] o in
  let stroke_d = String.concat " " (List.map opset_to_path stroke_opsets) in
  let fill_d = String.concat " " (List.map opset_to_path fill_opsets) in
  (stroke_d, fill_d)

let diamond ~seed ~roughness ~stroke_width ~width ~height =
  let o = default_options ~seed ~roughness:(float roughness) ~stroke_width in
  let cx = width /. 2.0 in
  let cy = height /. 2.0 in
  let stroke_opsets = diamond_ops 0.0 0.0 width height o in
  let polygon = [(cx, 0.0); (width, cy); (cx, height); (0.0, cy)] in
  let fill_opsets = hachure_fill [polygon] o in
  let stroke_d = String.concat " " (List.map opset_to_path stroke_opsets) in
  let fill_d = String.concat " " (List.map opset_to_path fill_opsets) in
  (stroke_d, fill_d)
