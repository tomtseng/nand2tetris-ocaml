open Core

type symbol_kind = Argument | Local | Static | Field [@@deriving compare, sexp_of]

module Symbol_kind = struct
  module T = struct
    type t = symbol_kind [@@deriving compare, sexp_of]
  end
  include T
  include Comparator.Make(T)
end

type symbol_info = {
  variable_type : Ast_types.variable_type ;
  kind : symbol_kind ;
  index : int ;
}

type t = {
  symbols : (string, symbol_info, String.comparator_witness) Map.t ;
  (** Number of symbols of each kind in [symbols]. *)
  kind_counts : (symbol_kind, int, Symbol_kind.comparator_witness) Map.t ;
}

let empty = {
  symbols = Map.empty (module String) ;
  kind_counts = Map.empty (module Symbol_kind) ;
}

let add table var_name var_type var_kind =
  let kind_count =
    match Map.find table.kind_counts var_kind with
    | Some count -> count
    | None -> 0
  in
  match
    Map.add table.symbols
      ~key:var_name
      ~data:{ variable_type = var_type ; kind = var_kind ; index = kind_count }
  with
  | `Duplicate -> `Duplicate
  | `Ok new_symbols ->
    `Ok {
      symbols = new_symbols ;
      kind_counts =
        Map.update table.kind_counts var_kind ~f:(function
            | None -> 1
            | Some count -> count + 1) ;
    }

let find table = Map.find table.symbols

let count_of_kind table kind =
  match Map.find table.kind_counts kind with
  | None -> 0
  | Some count -> count
