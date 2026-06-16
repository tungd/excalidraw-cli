open Cmdliner

let version = "0.2.0"

(* --- Export subcommand --- *)

let parse_input path =
  try Excalidraw_export.Excalidraw.parse_file path
  with
  | Sys_error msg ->
    begin
      Printf.eprintf "Error: %s\n%!" msg;
      exit 1
    end
  | Yojson.Json_error msg ->
    begin
      Printf.eprintf "JSON parse error: %s\n%!" msg;
      exit 1
    end

let run_export input_file output_file scale =
  let doc = parse_input input_file in
  let svg = Excalidraw_export.Render_svg.render ~scale doc in
  let out_path = match output_file with
    | Some p -> p
    | None ->
      let base = Filename.remove_extension input_file in
      base ^ ".svg" in
  let oc = open_out out_path in
  begin
    output_string oc svg;
    close_out oc
  end;
  Printf.printf "Wrote %s\n%!" out_path

let export_input =
  let doc = "Input .excalidraw file." in
  Arg.required (Arg.pos 0 (Arg.some Arg.file) None (Arg.info [] ~docv:"INPUT" ~doc))

let export_output =
  let doc = "Output file path (default: input with .svg extension)." in
  Arg.value (Arg.opt (Arg.some Arg.string) None (Arg.info [ "o"; "output" ] ~docv:"PATH" ~doc))

let export_scale =
  let doc = "Output scale factor." in
  Arg.value (Arg.opt Arg.float 1.0 (Arg.info [ "s"; "scale" ] ~docv:"FLOAT" ~doc))

let export_cmd =
  let doc = "Export an Excalidraw file to SVG" in
  let info = Cmd.info "export" ~doc in
  Cmd.v info Term.(const run_export $ export_input $ export_output $ export_scale)

(* --- Serve subcommand --- *)

let run_serve input_file output_file theme port no_open =
  begin
    if not (Sys.file_exists input_file) then begin
      let oc = open_out input_file in
      output_string oc {|{"type":"excalidraw","version":2,"source":"excalidraw-export","elements":[],"appState":{"viewBackgroundColor":"#ffffff"}}|};
      close_out oc;
      Printf.printf "Created %s\n%!" input_file
    end;
    let module S = Excalidraw_server.Server in
    let cfg = S.(
      { file_path = input_file;
        output_path = output_file;
        theme;
        port;
        open_browser = not no_open;
      }
    ) in
    Printf.printf "Serving Excalidraw editor at http://localhost:%d\n%!" port;
    Printf.printf "Editing: %s\n%!" input_file;
    S.run cfg
  end

let serve_input =
  let doc = "Input .excalidraw file." in
  Arg.required (Arg.pos 0 (Arg.some Arg.file) None (Arg.info [] ~docv:"INPUT" ~doc))

let serve_output =
  let doc = "Output SVG file path (rendered after editing)." in
  Arg.value (Arg.opt (Arg.some Arg.string) None (Arg.info [ "o"; "output" ] ~docv:"PATH" ~doc))

let serve_theme =
  let doc = "Editor theme (light or dark)." in
  Arg.value (Arg.opt Arg.string "light" (Arg.info [ "t"; "theme" ] ~docv:"THEME" ~doc))

let serve_port =
  let doc = "Port for the local server." in
  Arg.value (Arg.opt Arg.int 3001 (Arg.info [ "p"; "port" ] ~docv:"PORT" ~doc))

let serve_no_open =
  let doc = "Don't automatically open the browser." in
  Arg.value (Arg.flag (Arg.info [ "no-open" ] ~doc))

let serve_cmd =
  let doc = "Launch Excalidraw editor in the browser for a file" in
  let info = Cmd.info "serve" ~doc in
  Cmd.v info Term.(const run_serve $ serve_input $ serve_output $ serve_theme $ serve_port $ serve_no_open)

(* --- Main command --- *)

let cmd =
  let doc = "Export and edit Excalidraw files" in
  let info = Cmd.info "excalidraw-export" ~version ~doc in
  Cmd.group info [ export_cmd; serve_cmd ]

let () = exit (Cmd.eval cmd)
