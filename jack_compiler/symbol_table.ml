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
  type scoped_symbol_info = {
    variable_type : Ast_types.variable_type ;
    kind : scoped_symbol_kind ;
    index : int ;
  }

  val create : unit -> t
  val add :
    t ->
    string ->
    Ast_types.variable_type ->
    scoped_symbol_kind ->
    [ `Ok | `Duplicate ]
  val find : t -> string -> scoped_symbol_info option
  val count_of_kind : t -> scoped_symbol_kind -> int
end

module Make_scoped_symbol_table(Kind : Scoped_symbol_kind)
  : Scoped_symbol_table_intf with type scoped_symbol_kind := Kind.t = struct
  type scoped_symbol_info = {
    variable_type : Ast_types.variable_type ;
    kind : Kind.t ;
    (* this symbol is the index-th of symbol of its kind in the table *)
    index : int ;
  }

  type t = {
    table : (string, scoped_symbol_info) Hashtbl.t ;
    kind_counts : int array ;
  }

  let create () = {
    table = Hashtbl.create (module String) ;
    kind_counts = Array.create ~len:Kind.num_kinds 0
  }

  let add table var_name var_type var_kind =
    let counts_index = Kind.to_int var_kind in
    let count = Array.get table.kind_counts counts_index in
    Array.set table.kind_counts counts_index (count + 1);
    Hashtbl.add
      table.table
      ~key:var_name
      ~data:{ variable_type = var_type ; kind = var_kind ; index = count ; }

  let find table = Hashtbl.find table.table

  let count_of_kind table var_kind =
    let counts_index = Kind.to_int var_kind in
    Array.get table.kind_counts counts_index
end

type subroutine_symbol_kind = Argument | Local
type symbol_kind =
  | Class_scope of Ast_types.class_variable_kind
  | Subroutine_scope of subroutine_symbol_kind
type symbol_info = {
  variable_type : Ast_types.variable_type ;
  kind : symbol_kind ;
  index : int ;
}

module Class_symbols = Make_scoped_symbol_table(struct
    type t = Ast_types.class_variable_kind
    let num_kinds = 2
    let to_int = function
      | Ast_types.Static -> 0
      | Ast_types.Field -> 1
  end)

module Subroutine_symbols = Make_scoped_symbol_table(struct
    type t = subroutine_symbol_kind
    let num_kinds = 2
    let to_int = function
      | Argument -> 0
      | Local -> 1
  end)

type t = {
  class_symbols : Class_symbols.t ;
  mutable subroutine_symbols : Subroutine_symbols.t ;
}

let create () = {
  class_symbols = Class_symbols.create () ;
  subroutine_symbols = Subroutine_symbols.create () ;
}

let add table var_name var_type var_kind =
  match var_kind with
  | Class_scope kind ->
    Class_symbols.add table.class_symbols var_name var_type kind
  | Subroutine_scope kind ->
    Subroutine_symbols.add table.subroutine_symbols var_name var_type kind

let find table var_name =
  (* Variables in the subroutine scope shadow those in the class scope. *)
  match Subroutine_symbols.find table.subroutine_symbols var_name with
  | Some info ->
    Some {
      variable_type = info.variable_type ;
      kind = Subroutine_scope info.kind ;
      index = info.index ;
    }
  | None ->
    match Class_symbols.find table.class_symbols var_name with
    | Some info ->
      Some {
        variable_type = info.variable_type ;
        kind = Class_scope info.kind ;
        index = info.index ;
      }
    | None -> None

let count_of_kind table var_kind =
  match var_kind with
  | Class_scope kind -> Class_symbols.count_of_kind table.class_symbols kind
  | Subroutine_scope kind ->
    Subroutine_symbols.count_of_kind table.subroutine_symbols kind

let reset_subroutine_scope table =
  table.subroutine_symbols <- Subroutine_symbols.create ()
