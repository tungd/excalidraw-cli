type fill_style = Solid | Hachure | Cross_hatch
type stroke_style = Solid | Dashed | Dotted
type stroke_width = Normal | Bold | Extra_bold
type vertical_align = Top | Middle
type text_align = Left | Center | Right

type roundness = { value : float option }

type point = float * float

type binding = {
  element_id : string;
  focus : float;
  gap : float;
  fixed_point : float list option;
}

type base_element = {
  id : string;
  x : float;
  y : float;
  width : float;
  height : float;
  angle : float;
  stroke_color : string;
  background_color : string;
  fill_style : fill_style;
  stroke_width : stroke_width;
  raw_stroke_width : float;  (** Raw strokeWidth from JSON, used by rough.js *)
  stroke_style : stroke_style;
  roughness : int;
  opacity : int;
  group_ids : string list;
  frame_id : string option;
  roundness : roundness option;
  seed : int;
  version : int;
  version_nonce : int;
  is_deleted : bool;
  updated : int;
}

type rectangle_element = {
  base : base_element;
  bound_elements : Yojson.Safe.t list option;
}

type ellipse_element = {
  base : base_element;
  bound_elements : Yojson.Safe.t list option;
}

type diamond_element = {
  base : base_element;
  bound_elements : Yojson.Safe.t list option;
}

type text_element = {
  base : base_element;
  text : string;
  font_size : int;
  font_family : int;
  text_align : text_align;
  vertical_align : vertical_align;
  original_text : string;
  line_height : float;
  container_id : string option;
}

type linear_element = {
  base : base_element;
  points : point list;
  last_committed_point : point option;
  start_binding : binding option;
  end_binding : binding option;
  start_arrowhead : string option;
  end_arrowhead : string option;
  bound_elements : Yojson.Safe.t list option;
}

type image_element = {
  base : base_element;
  file_id : string option;
}

type frame_element = {
  base : base_element;
  name : string;
}

type element =
  | Rectangle of rectangle_element
  | Ellipse of ellipse_element
  | Diamond of diamond_element
  | Text of text_element
  | Arrow of linear_element
  | Line of linear_element
  | Image of image_element
  | Frame of frame_element
  | Unknown of string * Yojson.Safe.t

type app_state = {
  view_background_color : string;
  grid_size : int option;
}

type file_data = {
  mime_type : string;
  id : string;
  data_url : string;
  created : int;
  last_retrieved : int;
}

type document = {
  doc_type : string;
  version : int;
  source : string;
  elements : element list;
  app_state : app_state;
  files : (string * file_data) list;
}
