open Types

let get_string json field =
  match Yojson.Safe.Util.member field json with
  | `String s -> Some s
  | _ -> None

let get_string_exn json field =
  match get_string json field with
  | Some s -> s
  | None -> failwith ("Missing string field: " ^ field)

let get_float json field =
  match Yojson.Safe.Util.member field json with
  | `Float f -> f
  | `Int i -> float_of_int i
  | `Intlit i -> float_of_string i
  | _ -> failwith ("Missing float field: " ^ field)

let get_int json field =
  match Yojson.Safe.Util.member field json with
  | `Int i -> i
  | `Float f -> int_of_float f
  | _ -> failwith ("Missing int field: " ^ field)

let get_bool json field =
  match Yojson.Safe.Util.member field json with
  | `Bool b -> b
  | _ -> false

let get_string_list json field =
  match Yojson.Safe.Util.member field json with
  | `List items ->
    List.filter_map (function `String s -> Some s | _ -> None) items
  | _ -> []

let parse_base json =
  {
    id = get_string_exn json "id";
    x = get_float json "x";
    y = get_float json "y";
    width = get_float json "width";
    height = get_float json "height";
    angle = (match Yojson.Safe.Util.member "angle" json with
      | `Float f -> f | `Int i -> float_of_int i | _ -> 0.0);
    stroke_color = (match get_string json "strokeColor" with Some s -> s | None -> "#1e1e1e");
    background_color = (match get_string json "backgroundColor" with Some s -> s | None -> "transparent");
    fill_style = (match get_string json "fillStyle" with
      | Some "hachure" -> Hachure | Some "cross-hatch" -> Cross_hatch | _ -> Solid);
    stroke_width = (match Yojson.Safe.Util.member "strokeWidth" json with
      | `Int 2 -> Bold | `Int 3 -> Extra_bold | _ -> Normal);
    raw_stroke_width = (match Yojson.Safe.Util.member "strokeWidth" json with
      | `Int i -> float_of_int i
      | `Float f -> f
      | _ -> 1.0);
    stroke_style = (match get_string json "strokeStyle" with
      | Some "dashed" -> Dashed | Some "dotted" -> Dotted | _ -> Solid);
    roughness = (match Yojson.Safe.Util.member "roughness" json with
      | `Int i -> i | _ -> 1);
    opacity = (match Yojson.Safe.Util.member "opacity" json with
      | `Int i -> i | _ -> 100);
    group_ids = get_string_list json "groupIds";
    frame_id = (match Yojson.Safe.Util.member "frameId" json with
      | `String s -> Some s | _ -> None);
    roundness = (match Yojson.Safe.Util.member "roundness" json with
      | `Assoc _ as obj ->
        Some { value = (match Yojson.Safe.Util.member "value" obj with
          | `Float f -> Some f | `Int i -> Some (float_of_int i) | _ -> None) }
      | _ -> None);
    seed = (match get_int json "seed" with n -> n | exception _ -> 0);
    version = (match get_int json "version" with n -> n | exception _ -> 1);
    version_nonce = (match get_int json "versionNonce" with n -> n | exception _ -> 0);
    is_deleted = get_bool json "isDeleted";
    updated = (match get_int json "updated" with n -> n | exception _ -> 0);
  }

let parse_points json field =
  match Yojson.Safe.Util.member field json with
  | `List items ->
    List.filter_map (function
      | `List [ `Float x; `Float y ] -> Some (x, y)
      | `List [ `Int x; `Int y ] -> Some (float_of_int x, float_of_int y)
      | _ -> None) items
  | _ -> []

let parse_binding json field =
  match Yojson.Safe.Util.member field json with
  | `Assoc _ as obj ->
    Some {
      element_id = (match get_string obj "elementId" with Some s -> s | None -> "");
      focus = (match Yojson.Safe.Util.member "focus" obj with
        | `Float f -> f | `Int i -> float_of_int i | _ -> 0.0);
      gap = (match Yojson.Safe.Util.member "gap" obj with
        | `Float f -> f | `Int i -> float_of_int i | _ -> 0.0);
      fixed_point = None;
    }
  | _ -> None

let parse_element json =
  let typ = get_string_exn json "type" in
  let base = parse_base json in
  if base.is_deleted then Unknown (typ, json)
  else match typ with
  | "rectangle" ->
    Rectangle { base; bound_elements = None }
  | "ellipse" ->
    Ellipse { base; bound_elements = None }
  | "diamond" ->
    Diamond { base; bound_elements = None }
  | "text" ->
    Text {
      base;
      text = (match get_string json "text" with Some s -> s | None -> "");
      font_size = (match get_int json "fontSize" with n -> n | exception _ -> 20);
      font_family = (match get_int json "fontFamily" with n -> n | exception _ -> 1);
      text_align = (match get_string json "textAlign" with
        | Some "center" -> Center | Some "right" -> Right | _ -> Left);
      vertical_align = (match get_string json "verticalAlign" with
        | Some "middle" -> Middle | _ -> Top);
      original_text = (match get_string json "originalText" with Some s -> s | None -> "");
      line_height = (match Yojson.Safe.Util.member "lineHeight" json with
        | `Float f -> f | `Int i -> float_of_int i | _ -> 1.25);
      container_id = get_string json "containerId";
    }
  | "arrow" ->
    Arrow {
      base;
      points = parse_points json "points";
      last_committed_point = (match Yojson.Safe.Util.member "lastCommittedPoint" json with
        | `List [ `Float x; `Float y ] -> Some (x, y) | _ -> None);
      start_binding = parse_binding json "startBinding";
      end_binding = parse_binding json "endBinding";
      start_arrowhead = get_string json "startArrowhead";
      end_arrowhead = (match get_string json "endArrowhead" with
        | Some s -> Some s | None -> Some "arrow");
      bound_elements = None;
    }
  | "line" ->
    Line {
      base;
      points = parse_points json "points";
      last_committed_point = (match Yojson.Safe.Util.member "lastCommittedPoint" json with
        | `List [ `Float x; `Float y ] -> Some (x, y) | _ -> None);
      start_binding = parse_binding json "startBinding";
      end_binding = parse_binding json "endBinding";
      start_arrowhead = None;
      end_arrowhead = None;
      bound_elements = None;
    }
  | "image" ->
    Image { base; file_id = get_string json "fileId" }
  | "frame" ->
    Frame { base; name = (match get_string json "name" with Some s -> s | None -> "") }
  | other -> Unknown (other, json)

let of_yojson json =
  let elements_json = match Yojson.Safe.Util.member "elements" json with
    | `List items -> items | _ -> []
  in
  let elements = List.filter_map (fun el ->
    try Some (parse_element el)
    with Failure msg ->
      Printf.eprintf "Warning: skipping element: %s\n%!" msg;
      None
  ) elements_json in
  let app_state_json = match Yojson.Safe.Util.member "appState" json with
    | `Assoc _ as a -> a | _ -> `Assoc []
  in
  {
    doc_type = (match get_string json "type" with Some s -> s | None -> "excalidraw");
    version = (match get_int json "version" with n -> n | exception _ -> 2);
    source = (match get_string json "source" with Some s -> s | None -> "https://excalidraw.com");
    elements;
    app_state = {
      view_background_color = (match get_string app_state_json "viewBackgroundColor" with
        | Some s -> s | None -> "#ffffff");
      grid_size = (match Yojson.Safe.Util.member "gridSize" app_state_json with
        | `Int i -> Some i | _ -> None);
    };
    files = [];
  }

let parse_file path = of_yojson (Yojson.Safe.from_file path)
let parse_string s = of_yojson (Yojson.Safe.from_string s)
