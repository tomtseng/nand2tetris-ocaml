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

(* Gets the VM declaration of a subroutine. *)
let get_subroutine_declaration
    (class_name : string) (subroutine : Ast_types.subroutine) : string =
  let num_vm_args =
    match subroutine.function_type with
    | Constructor_type | Function_type -> List.length subroutine.parameters
    | Method_type -> (List.length subroutine.parameters) + 1
  in
  Printf.sprintf "function %s.%s %d"
    class_name subroutine.function_name num_vm_args

(* Gets VM code for setting up the `this` pointer for a subroutine definition.
*)
let get_this_pointer_setup
    (symbols : Symbol_table.t) (subroutine : Ast_types.subroutine) : string list =
  match subroutine.function_type with
  | Constructor_type ->
    let num_object_fields =
      Symbol_table.count_of_kind symbols (Class_scope Field)
    in
    [
      Printf.sprintf "push constant %d" num_object_fields ;
      "call Memory.alloc" ;
      "pop pointer 0" ;
    ]
  | Function_type -> []
  | Method_type -> [ "push argument 0" ; "pop pointer 0" ]

(** Compiles a subroutine. *)
let compile_subroutine
    (symbols : Symbol_table.t)
    (class_name : string)
    (subroutine : Ast_types.subroutine)
  : string list =
  Symbol_table.reset_subroutine_scope symbols;
  let declaration = get_subroutine_declaration class_name subroutine in
  let pointer_setup = get_this_pointer_setup symbols subroutine in
  declaration :: pointer_setup

(** Compiles Jack code to VM code as a list of strings. *)
let compile_program (program : Ast_types.class_declaration) : string list =
  let symbol_table = Symbol_table.create () in
  List.iter program.class_variables ~f:(add_class_symbol_exn symbol_table);
  List.(program.subroutines >>= compile_subroutine symbol_table program.name)
