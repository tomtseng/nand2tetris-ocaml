(** Contains compiler logic for transforming Jack AST to VM code. *)
open Core

(** Adds class symbol to symbol table. *)
let add_class_symbol_exn
    (symbol_table : Symbol_table.t)
    ((var_kind, (var_type, var_name)) : Ast_types.class_variable)
  : unit =
  match
    Symbol_table.add_symbol
      symbol_table var_name var_type (Class_scope var_kind)
  with
  | `Ok -> ()
  | `Duplicate -> failwith (Printf.sprintf "Duplicate symbol %s" var_name)

(** Compiles Jack code to VM code as a list of strings. *)
let compile (program : Ast_types.class_declaration) : string list =
  let symbol_table = Symbol_table.create () in
  List.iter program.class_variables ~f:(add_class_symbol_exn symbol_table);
  [ "TODO" ]
