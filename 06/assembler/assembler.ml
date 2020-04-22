open Core
open Stdio

(** Returns `input_filename` but with a ".hack" extension. *)
let get_output_filename (input_filename : string) : string =
  let
    base_filename, (_ : string option) = Filename.split_extension input_filename
  in
  base_filename ^ ".hack"

(** Run the Hack assembler on the input file. **)
let run_assembler (input_filename : string) : unit =
  (* TODO(tomtseng): Add a real implementation *)
  let output_filename = get_output_filename input_filename in
  In_channel.with_file input_filename ~f:(fun input_channel ->
      Out_channel.with_file output_filename ~f:(fun output_channel ->
          In_channel.iter_lines input_channel ~f:(fun input_line ->
              Out_channel.output_string output_channel (input_line ^ "\n")
            )))

let command : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Translates a Hack assembly program to Hack machine code."
    [%map_open
      let input_filename = anon ("INPUT_FILENAME" %: string) in
      fun () -> run_assembler input_filename
    ]

let () = Command.run command
