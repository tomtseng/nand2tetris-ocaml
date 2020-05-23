type variable_type =
  | Integer_type
  | Char_type
  | Boolean_type
  | Object_type of string  (* class name *)

type typed_variable = variable_type * string

type class_variable_kind =
  | Static  (** one instance of variable for class *)
  | Field  (** one instance of variable for each object *)

type class_variable = class_variable_kind * typed_variable

type subroutine_type =
  | Constructor_type
  | Function_type
  | Method_type

type subroutine_name =
  | This_call of string  (** method call from [this] pointer *)
  | Other_call of string * string

type keyword_constant =
  | True
  | False
  | Null
  | This

type binary_operator =
  | Plus
  | Minus
  | Multiply
  | Divide
  | Bitwise_and
  | Bitwise_or
  | Less_than
  | Greater_than
  | Equals

type unary_operator =
  | Negative
  | Bitwise_negation

type expression =
  | Integer_constant of int
  | String_constant of string
  | Keyword_constant of keyword_constant
  | Lvalue of lvalue
  | Subroutine_call of subroutine_name * expression list
  | Binary_operator of binary_operator * expression * expression
  | Unary_operator of unary_operator * expression
and lvalue =
  | Variable of string  (** name of variable *)
  | Array_element of string * expression  (** name of array, index into array *)

type statement =
  | Let_statement of lvalue * expression
  (** condition, statements if true, statements if false *)
  | If_statement of expression * statement list * statement list
  | While_statement of expression * statement list
  | Do_statement of subroutine_name * expression list
  | Return_statement of expression option

type subroutine = {
  function_type : subroutine_type ;
  return_type : variable_type option ;
  function_name : string ;
  parameters : typed_variable list ;
  local_variables : typed_variable list ;
  function_body : statement list ;
}

type class_declaration = {
  name : string ;
  class_variables: class_variable list ;
  subroutines : subroutine list ;
}
