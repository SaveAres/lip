open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = Ok 9

(* YOUR TESTS HERE *)
let%test "test_eval_2" = parse "(9 + 9) + 2 + ((3 + 4) + 3)" |> eval = Ok 30
let%test "test_eval_3" = parse "7 + (((((1 + 2) + 3) + 4) + 5) + 6)" |> eval = Ok 28
let%test "test_eval_4" = parse "1 - 2" |> eval = Ok (-1)