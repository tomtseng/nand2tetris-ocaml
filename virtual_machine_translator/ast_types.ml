type memory_segment =
  | Argument
  | Local
  | Static
  | Constant
  | This
  | That
  | Pointer
  | Temp

type memory_location = {
  segment : memory_segment;
  index : int;
}

type binary_expression =
  | Add
  | Subtract
  | Bitwise_and
  | Bitwise_or

type unary_expression =
  | Negative
  | Bitwise_not

type comparison_command =
  | Equals
  | Greater_than
  | Less_than

type command =
  | Pop of memory_location
  | Push of memory_location
  | Binary_expression of binary_expression
  | Unary_expression of unary_expression
  | Comparison of comparison_command
