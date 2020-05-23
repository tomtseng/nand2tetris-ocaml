open Core

(** Symbol kind for a particular scope. *)
module type Scoped_symbol_kind = sig
  type t
  val num_kinds : int

  (** Converts kind to an integer in the range [0, num_kinds). *)
  val to_int : t -> int
end

(** Symbol table for a particular scope. *)
module type Scoped_symbol_table_intf = sig
  type t
  type scoped_symbol_kind
  val create : unit -> t
  val add_symbol :
    t ->
    string ->
    Ast_types.variable_type ->
    scoped_symbol_kind ->
    [ `Ok | `Duplicate ]
end

module Make_scoped_symbol_table(Kind : Scoped_symbol_kind)
  : Scoped_symbol_table_intf with type scoped_symbol_kind := Kind.t = struct
  type symbol_info = {
    variable_type : Ast_types.variable_type ;
    kind : Kind.t ;
    (* this symbol is the index-th of symbol of its kind in the table *)
    index : int ;
  }

  type t = {
    table : (string, symbol_info) Hashtbl.t ;
    kind_counts : int array ;
  }

  let create () = {
    table = Hashtbl.create (module String) ;
    kind_counts = Array.create ~len:Kind.num_kinds 0
  }

  let add_symbol table var_name var_type var_kind =
    let counts_index = Kind.to_int var_kind in
    let count = Array.get table.kind_counts counts_index in
    Array.set table.kind_counts counts_index (count + 1);
    Hashtbl.add
      table.table
      ~key:var_name
      ~data:{ variable_type = var_type ; kind = var_kind ; index = count ; }
end

type subroutine_symbol_kind = Argument | Local
type symbol_kind =
  | Class_scope of Ast_types.class_variable_kind
  | Subroutine_scope of subroutine_symbol_kind

module Class_symbol_table = Make_scoped_symbol_table(struct
    type t = Ast_types.class_variable_kind
    let num_kinds = 2
    let to_int = function
      | Ast_types.Static -> 0
      | Ast_types.Field -> 1
  end)

module Subroutine_symbol_table = Make_scoped_symbol_table(struct
    type t = subroutine_symbol_kind
    let num_kinds = 2
    let to_int = function
      | Argument -> 0
      | Local -> 1
  end)

type t = {
  class_symbols : Class_symbol_table.t ;
  mutable subroutine_symbols : Subroutine_symbol_table.t ;
}

let create () = {
  class_symbols = Class_symbol_table.create () ;
  subroutine_symbols = Subroutine_symbol_table.create () ;
}

let add_symbol table var_name var_type var_kind =
  match var_kind with
  | Class_scope kind ->
    Class_symbol_table.add_symbol
      table.class_symbols var_name var_type kind
  | Subroutine_scope kind ->
    Subroutine_symbol_table.add_symbol
      table.subroutine_symbols var_name var_type kind

let reset_subroutine_scope table =
  table.subroutine_symbols <- Subroutine_symbol_table.create ()
