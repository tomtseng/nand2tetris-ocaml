open Sexplib.Std

type memory_location =
  | D_register
  | A_register
  | Memory_at_A
[@@deriving equal, sexp_of]

type jump_type =
  | Jgt
  | Jeq
  | Jge
  | Jlt
  | Jne
  | Jle
  | Jmp

type operator =
  | Plus
  | Minus
  | Bit_and
  | Bit_or
  | Unary_minus
[@@deriving sexp_of]

type expression =
  | Int of int
  | Memory of memory_location
  | Bit_negation of expression
  | Negative of expression
  | Operator of operator * expression * expression
[@@deriving sexp_of]

type c_instruction = {
  destination: memory_location list;
  computation: expression;
  jump: jump_type option;
}

type a_instruction =
  | Set_to_int of int
  | Set_to_symbol of string

type statement =
  | A_instruction of a_instruction
  | C_instruction of c_instruction
  | Label_definition of string
