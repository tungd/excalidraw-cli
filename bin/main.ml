open Cmdliner

let version = "0.1.0"

let parse_input path =
  try Excalidraw_export.Excalidraw.parse_file path
  with
  | Sys_error msg ->
    Printf.eprintf "Error: %s\n%!" msg;
    exit 1
  | Yojson.Json_error msg ->
    Printf.eprintf "JSON parse error: %s\n%!" msg;
    exit 1

let run input_file output_file scale =
  let doc = parse_input input_file in
  let svg = Excalidraw_export.Render_svg.render ~scale doc in
  let out_path = match output_file with
    | Some p -> p
    | None ->
      let base = Filename.remove_extension input_file in
      base ^ ".svg" in
  let oc = open_out out_path in
  output_string oc svg;
  close_out oc;
  Printf.printf "Wrote %s\n%!" out_path

let input_file =
  let doc = "Input .excalidraw file." in
  Arg.required (Arg.pos 0 (Arg.some Arg.file) None (Arg.info [] ~docv:"INPUT" ~doc))

let output_file =
  let doc = "Output file path (default: input with .svg extension)." in
  Arg.value (Arg.opt (Arg.some Arg.string) None (Arg.info [ "o"; "output" ] ~docv:"PATH" ~doc))

let scale =
  let doc = "Output scale factor." in
  Arg.value (Arg.opt Arg.float 1.0 (Arg.info [ "s"; "scale" ] ~docv:"FLOAT" ~doc))

let cmd =
  let doc = "Export Excalidraw files to SVG" in
  let info = Cmd.info "excalidraw-export" ~version ~doc in
  Cmd.v info Term.(const run $ input_file $ output_file $ scale)

let () = exit (Cmd.eval cmd)
