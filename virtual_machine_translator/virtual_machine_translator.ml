open Core
open Stdio

(** Returns `input_filename`, but with a ".asm" extension. *)
let get_asm_output_filename (input_filename : string) : string =
  let
    base_filename, (_ : string option) = Filename.split_extension input_filename
  in
  base_filename ^ ".asm"

(** Returns list of input .vm files.
 *
 *  If the input path is a .vm file, returns just that file If the input path is
 *  a directory, returns all .vm files at the top level of that directory. *)
let get_input_files (input_path : string) : string list =
  let is_vm_file (filename : string) : bool =
    match Filename.split_extension filename with
    | (_, Some extension) -> String.equal extension "vm"
    | (_, None) -> false
  in
  match Sys.is_directory input_path with
  | `Yes -> input_path |> Sys.readdir |> Array.filter ~f:is_vm_file
            |> Array.map ~f:(fun file -> String.concat [input_path ; "/" ; file ])
            |> Array.to_list
  | `No -> [ input_path ]
  | `Unknown -> failwith (Printf.sprintf "Can't determine file type: %s" input_path)

(** Runs the VM translator on the input path. *)
let run_translator (input_path : string) : unit =
  let input_files = get_input_files input_path in
  let output_file = get_asm_output_filename input_path in
  failwith "unimplemented"

let command : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Translates a VM program to Hack assembly code."
    [%map_open
      let input_path = anon ("INPUT_PATH" %: string) in
      fun () -> run_translator input_path
    ]

let () = Command.run command
