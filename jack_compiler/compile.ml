(** Contains compiler logic for transforming Jack AST to VM code. *)
open Core

exception Compile_error of string

type compiler_state = {
  class_name : string ;
  symbols : Symbol_table.t ;
  (** Number of statements in the subroutine that have used a goto jump. *)
  goto_count : int
}

(** Adds a symbol to the symbol table, raising an exception if the symbol is
    already in the table. *)
let add_symbol_exn
    (symbols : Symbol_table.t)
    ((var_type, var_name) : Ast_types.typed_variable)
    ~kind:(var_kind : Symbol_table.symbol_kind)
  : Symbol_table.t =
  match Symbol_table.add symbols var_name var_type var_kind with
  | `Ok new_symbols -> new_symbols
  | `Duplicate ->
    raise (Compile_error (Printf.sprintf "Duplicate symbol %s" var_name))

(** Adds class variable to symbol table, raising an exception if the symbol is already
    in the table. *)
let add_class_symbol_exn
    (symbols : Symbol_table.t)
    ((class_var_kind, typed_var) : Ast_types.class_variable)
  : Symbol_table.t =
  let var_kind : Symbol_table.symbol_kind =
    match class_var_kind with
    | Ast_types.Static -> Symbol_table.Static
    | Ast_types.Field -> Symbol_table.Field
  in
  add_symbol_exn symbols typed_var ~kind:var_kind

(** Find symbol in symbol table, raising an exception if the symbol is not in
    the table. *)
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
  let num_local_variables = List.length subroutine.local_variables in
  [ Printf.sprintf "function %s.%s %d"
      class_name subroutine.function_name num_local_variables ]

(* Gets VM code for setting up the [this] pointer for a subroutine definition.
*)
let get_this_pointer_setup
    (symbols : Symbol_table.t)
    (subroutine : Ast_types.subroutine)
  : string list =
  match subroutine.function_type with
  | Constructor_type ->
    let num_object_fields = Symbol_table.count_of_kind symbols Field in
    [
      Printf.sprintf "push constant %d" num_object_fields ;
      "call Memory.alloc 1" ;
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
    | Argument -> "argument"
    | Local -> "local"
    | Static -> "static"
    | Field -> "this"
  in
  Printf.sprintf "%s %d" memory_segment info.index

(** Converts a binary operator to the corresponding VM command. *)
let binary_operator_command : Ast_types.binary_operator -> string = function
  | Plus -> "add"
  | Minus -> "sub"
  | Multiply -> "call Math.multiply 2"
  | Divide -> "call Math.divide 2"
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
    (state : compiler_state) (e : Ast_types.expression) : string list =
  match e with
  | Integer_constant i -> [ Printf.sprintf "push constant %d" i ]
  | String_constant str ->
    List.concat
      [
        [
          (* Allocate string and store as a temporary variable . *)
          Printf.sprintf "push constant %d" (String.length str) ;
          "call String.new 1" ;
          "pop temp 0" ;
        ] ;
        List.(
          str
          |> String.to_list
          >>= (fun ch ->
              [
                "push temp 0" ;
                Printf.sprintf "push constant %d" (Char.to_int ch) ;
                "call String.appendChar 2" ;
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
    let (lval_setup, lval_location) = compile_lvalue state lval in
    lval_setup @ [ "push " ^ lval_location ]
  | Subroutine_call (name, parameters) ->
    compile_subroutine_call state name parameters
  | Binary_operator (op, e1, e2) ->
    List.concat [
      compile_expression state e1 ;
      compile_expression state e2 ;
      [binary_operator_command op] ;
    ]
  | Unary_operator (op, e1) ->
    (compile_expression state e1) @ [unary_operator_command op]

(** Compiles an l-value expression, returning
    (1) VM instructions to set up the expression,
    (2) "<memory segment> <index>" at which the l-value may be pushed/popped.

    The VM instructions may modify the VM [that] pointer. *)
and compile_lvalue
    (state : compiler_state) (lval : Ast_types.lvalue) : string list * string =
  match lval with
  | Variable var_name ->
    let var_info = find_symbol_exn state.symbols var_name in
    let var_location = symbol_info_to_memory_location var_info in
    ([], var_location)
  | Array_element (arr_name, index) ->
    let var_info = find_symbol_exn state.symbols arr_name in
    let arr_base_location = symbol_info_to_memory_location var_info in
    (List.concat
       [
         (* Store address of array element in [that] pointer. *)
         [ "push " ^ arr_base_location ] ;
         compile_expression state index ;
         [
           "add" ;
           "pop pointer 1" ;
         ]
       ],
     "that 0")

(** Compiles a subroutine call, leaving the result at the top of the VM stack.
*)
and compile_subroutine_call
    (state : compiler_state)
    (subroutine_name : Ast_types.subroutine_name)
    (parameters : Ast_types.expression list)
  : string list =
  (* (1) Name of class for the subroutine.
     (2) Name of the function within the class.
     (3) Command to push object as the first argument if function is a
         method call on an object. *)
  let (func_class_name, func_method_name, push_object_command_opt)
    : string * string * string option =
    match subroutine_name with
    | This_call method_name ->
      (state.class_name, method_name, Some "push pointer 0")
    | Other_call (qualifier, method_name) ->
      match Symbol_table.find state.symbols qualifier with
      | None -> (qualifier, method_name, None)
      | Some symbol_info ->
        match symbol_info.variable_type with
        | Integer_type | Char_type | Boolean_type ->
          (qualifier, method_name, None)
        | Object_type obj_type ->
          (obj_type,
           method_name,
           Some ("push " ^ (symbol_info_to_memory_location symbol_info)))
  in
  let full_subroutine_name =
    String.concat ~sep:"." [ func_class_name ; func_method_name ]
  in
  let push_object_command =
    match push_object_command_opt with
    | Some x -> [ x ]
    | None -> []
  in
  let num_args =
    match push_object_command_opt with
    | Some _ -> (List.length parameters) + 1
    | None -> List.length parameters
  in
  List.concat [
    push_object_command ;
    List.(parameters >>= compile_expression state) ;
    [ Printf.sprintf "call %s %d" full_subroutine_name num_args ] ;
  ]

(** Compiles a statement. *)
let rec compile_statement
    (state : compiler_state) (statement : Ast_types.statement)
  : compiler_state * string list =
  match statement with
  | Let_statement (lval, rval) ->
    let compiled_rval = compile_expression state rval in
    let (lval_setup, lval_location) = compile_lvalue state lval in
    (state,
     List.concat [ compiled_rval ; lval_setup ; ["pop " ^ lval_location] ])
  | Do_statement (func_name, params) ->
    (state,
     (compile_subroutine_call state func_name params)
     @ [ ignore_stack_top  (* ignore return value *) ])
  | Return_statement return_value_opt ->
    let compiled_return =
      match return_value_opt with
      | None -> [ "push constant 0" ; "return" ]
      | Some expression -> (compile_expression state expression) @ [ "return" ]
    in
    (state, compiled_return)
  | If_statement (branch_condition, true_block, false_block) ->
    compile_if_statement state branch_condition true_block false_block
  | While_statement (while_condition, do_block) ->
    compile_while_statement state while_condition do_block

(** Compiles an if statement. *)
and compile_if_statement
    (state : compiler_state)
    (branch_condition : Ast_types.expression)
    (true_block : Ast_types.statement list)
    (false_block : Ast_types.statement list)
  : compiler_state * string list =
  let label_id = state.goto_count in
  let state = { state with goto_count = state.goto_count + 1 } in
  let true_label = Printf.sprintf "if_true_%d" label_id in
  let end_label = Printf.sprintf "if_end_%d" label_id in
  let compiled_condition = compile_expression state branch_condition in
  let (state, compiled_true_block) = compile_statements state true_block in
  let (state, compiled_false_block) = compile_statements state false_block in
  (state,
   List.concat [
     compiled_condition ;
     [ Printf.sprintf "if-goto %s" true_label ] ;
     compiled_false_block ;
     [
       Printf.sprintf "goto %s" end_label ;
       Printf.sprintf "label %s" true_label ;
     ] ;
     compiled_true_block ;
     [ Printf.sprintf "label %s" end_label ] ;
   ])

(** Compiles a while statement. *)
and compile_while_statement
    (state : compiler_state)
    (while_condition : Ast_types.expression)
    (do_block : Ast_types.statement list)
  : compiler_state * string list =
  let label_id = state.goto_count in
  let state = { state with goto_count = state.goto_count + 1 } in
  let condition_label = Printf.sprintf "while_cond_%d" label_id in
  let do_label = Printf.sprintf "while_do_%d" label_id in
  let compiled_condition = compile_expression state while_condition in
  let (state, compiled_do_block) = compile_statements state do_block in
  (state,
   List.concat [
     [
       Printf.sprintf "goto %s" condition_label ;
       Printf.sprintf "label %s" do_label ;
     ] ;
     compiled_do_block ;
     [ Printf.sprintf "label %s" condition_label ] ;
     compiled_condition ;
     [ Printf.sprintf "if-goto %s" do_label ] ;
   ])

(** Compiles several statements. *)
and compile_statements
    (state : compiler_state) (statements : Ast_types.statement list)
  : compiler_state * string list =
  let (next_state, compilation_rev) =
    List.fold
      statements
      ~init:(state, [])
      ~f:(fun (state_acc, compilation_acc) -> fun statement ->
          let (next_state, compiled_statement) =
            compile_statement state_acc statement
          in
          (next_state, compiled_statement :: compilation_acc))
  in
  (next_state, compilation_rev |> List.rev |> List.concat)

(** Compiles a subroutine. *)
let compile_subroutine
    (state : compiler_state)
    (subroutine : Ast_types.subroutine)
  : string list =
  let declaration = get_subroutine_declaration state.class_name subroutine in
  let pointer_setup = get_this_pointer_setup state.symbols subroutine in
  (* Add [this] as argument to methods to make the argument indexing correct. *)
  let params : Ast_types.typed_variable list =
    match subroutine.function_type with
    | Constructor_type | Function_type -> subroutine.parameters
    | Method_type ->
      (Object_type state.class_name, "this") :: subroutine.parameters
  in
  let subroutine_symbols =
    List.fold
      params ~init:state.symbols ~f:(add_symbol_exn ~kind:Symbol_table.Argument)
  in
  let subroutine_symbols =
    List.fold
      subroutine.local_variables
      ~init:subroutine_symbols
      ~f:(add_symbol_exn ~kind:Symbol_table.Local)
  in
  let subroutine_state = { state with symbols = subroutine_symbols } in
  let (_, body) =
    compile_statements subroutine_state subroutine.function_body
  in
  List.concat [
    declaration ;
    pointer_setup ;
    body
  ]

(** Compiles Jack code to VM code as a list of strings. *)
let compile_program (program : Ast_types.class_declaration) : string list =
  let symbol_table = Symbol_table.empty in
  let symbol_table =
    List.fold program.class_variables ~init:symbol_table ~f:add_class_symbol_exn
  in
  let state = {
    class_name = program.name ;
    symbols = symbol_table ;
    goto_count = 0 ;
  } in
  List.(program.subroutines >>= compile_subroutine state)
