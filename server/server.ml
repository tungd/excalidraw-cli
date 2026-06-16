(* Eio functions are accessible via Eio. prefix *)

type serve_config = {
  file_path : string;
  output_path : string option;
  theme : string;
  port : int;
  open_browser : bool;
}

let config = ref None
let get_config () = Option.get !config

(* MIME type guessing *)
let mime_of_path path =
  if String.ends_with ~suffix:".html" path then "text/html; charset=utf-8"
  else if String.ends_with ~suffix:".js" path then "application/javascript"
  else if String.ends_with ~suffix:".mjs" path then "application/javascript"
  else if String.ends_with ~suffix:".css" path then "text/css"
  else if String.ends_with ~suffix:".svg" path then "image/svg+xml"
  else if String.ends_with ~suffix:".woff2" path then "font/woff2"
  else if String.ends_with ~suffix:".woff" path then "font/woff"
  else if String.ends_with ~suffix:".png" path then "image/png"
  else if String.ends_with ~suffix:".json" path then "application/json"
  else "application/octet-stream"

(* Read the .excalidraw file *)
let load_file () =
  let cfg = get_config () in
  if Sys.file_exists cfg.file_path then begin
    let ic = open_in cfg.file_path in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    Some (Bytes.to_string s)
  end else
    None

(* Write the .excalidraw file *)
let save_file json_str =
  let cfg = get_config () in
  let oc = open_out cfg.file_path in
  begin
    output_string oc json_str;
    close_out oc
  end

(* Read body from eio flow source *)
let read_body body =
  let buf = Buffer.create 4096 in
  let sink = Eio.Flow.buffer_sink buf in
  begin
    Eio.Flow.copy body sink;
    Buffer.contents buf
  end

(* Inject config into index.html *)
let inject_config html =
  let cfg = get_config () in
  let config_script = Printf.sprintf
    {|<script>window.__EXCALIDRAW_CONFIG__ = {"file":"%s","theme":"%s"};</script>|}
    (Filename.basename cfg.file_path) cfg.theme in
  try
    let idx = String.index html '<' in
    let rec find_head i =
      if i >= String.length html - 5 then String.length html
      else if String.sub html i 5 = "<head" then
        let close = try String.index_from html i '>' with Not_found -> i + 5 in
        close + 1
      else find_head (i + 1)
    in
    let insert_at = find_head idx in
    let result = Buffer.create (String.length html + 200) in
    Buffer.add_substring result html 0 insert_at;
    Buffer.add_string result config_script;
    Buffer.add_substring result html insert_at (String.length html - insert_at);
    Buffer.contents result
  with Not_found -> html

(* Build the cohttp-eio server callback *)
let make_callback done_resolver =
  fun _conn req body writer ->
  let method_ = Cohttp.Request.meth req in
  let target = Cohttp.Request.resource req in
  let respond ~status ~body ~headers =
    let headers = if headers = Http.Header.init () then
      Http.Header.of_list ["Content-Type", "application/json"]
    else headers in
    Cohttp_eio.Server.respond
      ~headers
      ~status
      ~body:(Cohttp_eio.Body.of_string body)
      () writer
  in
  let respond_json ~status ~body =
    respond ~status ~body ~headers:(Http.Header.of_list ["Content-Type", "application/json"])
  in
  let respond_static path =
    match Editor_static.read path with
    | None ->
      respond_json ~status:`Not_found ~body:{|"Not found"|}
    | Some content ->
      let mime = mime_of_path path in
      respond ~status:`OK ~body:content ~headers:(Http.Header.of_list ["Content-Type", mime])
  in
  match method_, target with
  | `GET, "/api/load" ->
    begin match load_file () with
    | Some json -> respond_json ~status:`OK ~body:json
    | None -> respond_json ~status:`Not_found ~body:{|{"error":"file not found"}|}
    end
  | `GET, "/api/config" ->
    let cfg = get_config () in
    let filename = Filename.basename cfg.file_path in
    let json = Printf.sprintf {|{"file":"%s","theme":"%s"}|} filename cfg.theme in
    respond_json ~status:`OK ~body:json
  | `POST, "/api/save" ->
    let json_str = read_body body in
    begin save_file json_str; respond_json ~status:`OK ~body:{|{"status":"saved"}|} end
  | `POST, "/api/done" ->
    begin
      Eio.Promise.resolve done_resolver ();
      respond_json ~status:`OK ~body:{|{"status":"done"}|}
    end
  | `GET, ("/" | "") ->
    begin match Editor_static.read "index.html" with
    | None -> respond_static "index.html"
    | Some content ->
      let html = inject_config content in
      respond ~status:`OK ~body:html ~headers:(Http.Header.of_list ["Content-Type", "text/html; charset=utf-8"])
    end
  | `GET, path ->
    let path = if String.starts_with ~prefix:"/" path
               then String.sub path 1 (String.length path - 1)
               else path in
    let path = if path = "" then "index.html" else path in
    respond_static path
  | _ ->
    respond ~status:`Method_not_allowed ~body:"Method not allowed" ~headers:(Http.Header.of_list [])

let run cfg =
  begin
    config := Some cfg;
    let done_promise, done_resolver = Eio.Promise.create () in
    let port = cfg.port in
    let server = Cohttp_eio.Server.make
      ~callback:(make_callback done_resolver)
      () in
    Eio_main.run @@ fun env ->
    let net = Eio.Stdenv.net env in
    Eio.Switch.run @@ fun sw ->
    let socket = Eio.Net.listen net ~reuse_addr:true ~backlog:128 ~sw
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
    if cfg.open_browser then
      let _ = Eio.Fiber.fork ~sw @@ fun () ->
        begin
          Eio.Time.sleep (Eio.Stdenv.clock env) 0.5;
          let cmd = Printf.sprintf "open http://localhost:%d" port in
          ignore (Sys.command cmd)
        end
      in ();
    Printf.printf "Serving Excalidraw editor at http://localhost:%d\n%!" port;
    Printf.printf "Editing: %s\n%!" cfg.file_path;
    Printf.printf "Press Ctrl+C or click Done in the editor to finish\n%!";
    let _ = Cohttp_eio.Server.run ~stop:done_promise
      ~on_error:(fun e -> Printf.eprintf "Server error: %s\n%!" (Printexc.to_string e))
      socket server
    in ()
  end
