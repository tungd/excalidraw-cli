(lang dune 3.14)
(name excalidraw-export)
(generate_opam_files true)
(package
 (name excalidraw-export)
 (synopsis "CLI tool to export Excalidraw files to SVG")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.14))
  (yojson (>= 2.0))
  (tyxml (>= 4.0))
  (cmdliner (>= 1.2))
  (alcotest (>= 1.5))))
