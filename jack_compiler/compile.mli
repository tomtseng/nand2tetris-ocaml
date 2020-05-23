exception Compile_error of string

(** Compiles Jack code to VM code as a list of strings. *)
val compile_program: Ast_types.class_declaration -> string list
