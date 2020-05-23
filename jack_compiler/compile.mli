(** Compiles Jack code to VM code as a list of strings. *)
val compile: Ast_types.class_declaration -> string list
