(** Contains compiler logic for transforming Jack AST to VM code. *)
open Core

exception Compile_error of string

(** Adds class symbol to symbol table. *)
let add_class_symbol_exn
    (symbols : Symbol_table.t)
    ((var_kind, (var_type, var_name)) : Ast_types.class_variable)
  : unit =
  match Symbol_table.add symbols var_name var_type (Class_scope var_kind) with
  | `Ok -> ()
  | `Duplicate ->
    raise (Compile_error (Printf.sprintf "Duplicate symbol %s" var_name))

let add_subroutine_symbol_exn
    (symbols : Symbol_table.t)
    (var_kind : Symbol_table.subroutine_symbol_kind)
    ((var_type, var_name) : Ast_types.typed_variable)
  : unit =
  match
    Symbol_table.add symbols var_name var_type (Subroutine_scope var_kind)
  with
  | `Ok -> ()
  | `Duplicate ->
    raise (Compile_error (Printf.sprintf "Duplicate symbol %s" var_name))

let find_symbol_exn
    (symbols : Symbol_table.t)
    (var_name : string)
  : Symbol_table.symbol_info =
  match Symbol_table.find symbols var_name with
  | Some info -> info
  | None ->
    raise (Compile_error (Printf.sprintf "Unknown variable %s" var_name))

(* Gets the VM declaration of a subroutine. *)
let get_subroutine_declaration
    (class_name : string) (subroutine : Ast_types.subroutine) : string list =
  let num_vm_args =
    match subroutine.function_type with
    | Constructor_type | Function_type -> List.length subroutine.parameters
    | Method_type -> (List.length subroutine.parameters) + 1
  in
  [ Printf.sprintf "function %s.%s %d"
      class_name subroutine.function_name num_vm_args ]

(* Gets VM code for setting up the [this] pointer for a subroutine definition.
*)
let get_this_pointer_setup
    (symbols : Symbol_table.t)
    (subroutine : Ast_types.subroutine)
  : string list =
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

(** VM code for popping and ignoring the top of the stack. *)
let ignore_stack_top = "pop temp 7"

(** Converts symbol info to the string representing the VM memory location
    holding the symbol. *)
let symbol_info_to_memory_location (info : Symbol_table.symbol_info) : string =
  let memory_segment =
    match info.kind with
    | Class_scope Static -> "static"
    | Class_scope Field -> "this"
    | Subroutine_scope Argument -> "argument"
    | Subroutine_scope Local -> "local"
  in
  Printf.sprintf "%s %d" memory_segment info.index

(** Converts a binary operator to the corresponding VM command. *)
let binary_operator_command : Ast_types.binary_operator -> string = function
  | Plus -> "add"
  | Minus -> "sub"
  | Multiply -> "call Math.multiply"
  | Divide -> "call Math.divide"
  | Bitwise_and -> "and"
  | Bitwise_or -> "or"
  | Less_than -> "lt"
  | Greater_than -> "gt"
  | Equals -> "eq"

(** Converts a unary operator to the corresponding VM command. *)
let unary_operator_command : Ast_types.unary_operator -> string = function
  | Negative -> "neg"
  | Bitwise_negation -> "not"

(** Compiles an expression, leaving the result of the expression at the top of
    the VM stack. *)
let rec compile_expression
    (symbols: Symbol_table.t) (e : Ast_types.expression) : string list =
  match e with
  | Integer_constant i -> [ Printf.sprintf "push constant %d" i ]
  | String_constant str ->
    List.concat
      [
        [
          (* Allocate string and store as a temporary variable . *)
          Printf.sprintf "push constant %d" (String.length str) ;
          "call String.new" ;
          "pop temp 0" ;
        ] ;
        List.(
          str
          |> String.to_list
          >>= (fun ch ->
              [
                "push temp 0" ;
                Printf.sprintf "push constant %d" (Char.to_int ch) ;
                "call String.appendChar" ;
                ignore_stack_top ;  (* ignore return value *)
              ])
        ) ;
        [ "push temp 0" ] ;
      ]
  | Keyword_constant keyword ->
    begin
      match keyword with
      | True -> [ "push constant 1" ; "neg" ]
      | False | Null -> [ "push constant 0" ]
      | This -> [ "push pointer 0" ]
    end
  | Lvalue lval ->
    let (lval_setup, lval_location) = compile_lvalue symbols lval in
    lval_setup @ [ "push " ^ lval_location ]
  | Subroutine_call (name, parameters) ->
    compile_subroutine_call symbols name parameters
  | Binary_operator (op, e1, e2) ->
    List.concat [
      compile_expression symbols e1 ;
      compile_expression symbols e2 ;
      [binary_operator_command op] ;
    ]
  | Unary_operator (op, e1) ->
    (compile_expression symbols e1) @ [unary_operator_command op]

(** Compiles an l-value expression, returning
    (1) VM instructions to set up the expression,
    (2) "<memory segment> <index>" at which the l-value may be pushed/popped.

    The VM instructions may modify the VM [that] pointer. *)
and compile_lvalue
    (symbols: Symbol_table.t) (lval : Ast_types.lvalue)
  : string list * string =
  match lval with
  | Variable var_name ->
    let var_info = find_symbol_exn symbols var_name in
    let var_location = symbol_info_to_memory_location var_info in
    ([], var_location)
  | Array_element (arr_name, index) ->
    let var_info = find_symbol_exn symbols arr_name in
    let arr_base_location = symbol_info_to_memory_location var_info in
    (List.concat
       [
         (* Store address of array element in [that] pointer. *)
         [ "push " ^ arr_base_location ] ;
         compile_expression symbols index ;
         [
           "add" ;
           "pop pointer 1" ;
         ]
       ],
     "that 0")

(** Compiles a subroutine call, leaving the result at the top of the VM stack.
*)
and compile_subroutine_call
    (symbols: Symbol_table.t)
    (name : Ast_types.subroutine_name)
    (parameters : Ast_types.expression list)
  : string list =
  (* If this is a method call from an object, push the object as the first
     argument. *)
  let push_object_command_opt : string option =
    Option.(
      return name
      >>= (function
          | Function_name _ -> None
          | Method_name (qualifier, _method_name) -> Some qualifier)
      >>= Symbol_table.find symbols
      >>= (fun symbol_info ->
          match symbol_info.variable_type with
          | Integer_type | Char_type | Boolean_type -> None
          | Object_type _obj_type -> Some symbol_info)
      >>| (fun symbol_info ->
          "push " ^ (symbol_info_to_memory_location symbol_info))
    )
  in
  let push_object : string list =
    match push_object_command_opt with
    | Some x -> [ x ]
    | None -> []
  in
  let name_str =
    match name with
    | Function_name str -> str
    | Method_name (qualifier, method_name) ->
      String.concat ~sep:"." [ qualifier ; method_name ]
  in
  List.concat [
    push_object ;
    List.(parameters >>= compile_expression symbols) ;
    [ "call " ^ name_str ] ;
  ]

(** Compiles a let statement assigning [rval] to [lval]. *)
let compile_let_statement
    (symbols : Symbol_table.t)
    (lval : Ast_types.lvalue)
    (rval : Ast_types.expression)
  : string list =
  let compiled_rval = compile_expression symbols rval in
  let (lval_setup, lval_location) = compile_lvalue symbols lval in
  List.concat [ compiled_rval ; lval_setup ; ["pop " ^ lval_location] ]

(** Compiles a statement. *)
let compile_statement
    (symbols : Symbol_table.t) (statement : Ast_types.statement) : string list =
  match statement with
  | Let_statement (lval, rval) -> compile_let_statement symbols lval rval
  | If_statement _ | While_statement _ | Do_statement _ | Return_statement _ ->
    ["TODO"]

(** Compiles a subroutine. *)
let compile_subroutine
    (symbols : Symbol_table.t)
    (class_name : string)
    (subroutine : Ast_types.subroutine)
  : string list =
  Symbol_table.reset_subroutine_scope symbols;
  let declaration = get_subroutine_declaration class_name subroutine in
  let pointer_setup = get_this_pointer_setup symbols subroutine in
  List.iter
    subroutine.parameters ~f:(add_subroutine_symbol_exn symbols Argument);
  List.iter
    subroutine.local_variables ~f:(add_subroutine_symbol_exn symbols Local);
  let body = List.(subroutine.function_body >>= compile_statement symbols) in
  List.concat [ declaration ; pointer_setup ; body ]

(** Compiles Jack code to VM code as a list of strings. *)
let compile_program (program : Ast_types.class_declaration) : string list =
  let symbol_table = Symbol_table.create () in
  List.iter program.class_variables ~f:(add_class_symbol_exn symbol_table);
  List.(program.subroutines >>= compile_subroutine symbol_table program.name)
