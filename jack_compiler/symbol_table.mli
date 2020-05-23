(** Stores information about variables in current scope during compilation. *)

type t

type subroutine_symbol_kind =
  | Argument  (* function argument *)
  | Local  (* local variable *)

(** Scope and kind of a symbol. *)
type symbol_kind =
  | Class_scope of Ast_types.class_variable_kind
  | Subroutine_scope of subroutine_symbol_kind

(** Create empty symbol table. *)
val create : unit -> t

(** [add_symbol t var_name var_type var_kind] Adds variable [var_name] to table.
*)
val add_symbol :
  t -> string -> Ast_types.variable_type -> symbol_kind -> [ `Ok | `Duplicate ]

(** [count_of_kind t kind] Returns the number of symbols of kind [kind]. *)
val count_of_kind : t -> symbol_kind -> int

(** Clear subroutine symbols. *)
val reset_subroutine_scope : t -> unit
