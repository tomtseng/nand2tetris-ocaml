(** Stores information about variables in current scope during compilation. *)

type t

type subroutine_symbol_kind =
  | Argument  (* function argument *)
  | Local  (* local variable *)

(** Scope and kind of a symbol. *)
type symbol_kind =
  | Class_scope of Ast_types.class_variable_kind
  | Subroutine_scope of subroutine_symbol_kind

type symbol_info = {
  variable_type : Ast_types.variable_type ;
  kind : symbol_kind ;
  (* this symbol is the index-th of symbol of its kind in the table *)
  index : int ;
}

(** Create empty symbol table. *)
val create : unit -> t

(** [add t var_name var_type var_kind] Adds variable [var_name] to table.
*)
val add :
  t -> string -> Ast_types.variable_type -> symbol_kind -> [ `Ok | `Duplicate ]

(** [find t var_name] Finds data for variable [var_name] in the table. *)
val find : t -> string -> symbol_info option

(** [count_of_kind t kind] Returns the number of symbols of kind [kind]. *)
val count_of_kind : t -> symbol_kind -> int

(** Clear subroutine symbols. *)
val reset_subroutine_scope : t -> unit
