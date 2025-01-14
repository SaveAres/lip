type ast = 
  | Nothing
  | Const of int
  | Survive of ast * ast
  | Born of ast * ast
;;