exception InvalidConvertion

type ast =
  | Const of int
  | Add of ast * ast
  | Sub of ast * ast
  | Mul of ast * ast
  | Div of ast * ast
;;

let decimal_of_hex hex = match (int_of_string_opt hex) with
  | Some a -> a
  | None -> raise InvalidConvertion 
;;

