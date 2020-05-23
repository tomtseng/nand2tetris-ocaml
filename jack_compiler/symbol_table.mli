(** Stores information about variables in current scope during compilation. *)

type t

(** Kind of a symbol. *)
type symbol_kind =
  | Argument  (** subroutine argument *)
  | Local  (** subroutine local variable *)
  | Static  (** class-wide variable *)
  | Field  (** per-object variable for class *)

type symbol_info = {
  variable_type : Ast_types.variable_type ;
  kind : symbol_kind ;
  (** this symbol is the index-th of symbol of its kind in the table *)
  index : int ;
}

(** Empty symbol table. *)
val empty : t

(** [add t var_name var_type var_kind] Adds symbol [var_name] to table. *)
val add :
  t -> string -> Ast_types.variable_type -> symbol_kind
  -> [ `Ok of t | `Duplicate ]

(** [find t var_name] Finds data for symbol [var_name] in the table. *)
val find : t -> string -> symbol_info option

(** [count_of_kind t kind] Returns the number of symbols of kind [kind]. *)
val count_of_kind : t -> symbol_kind -> int
