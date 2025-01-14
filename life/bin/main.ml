(* Apertura Moduli *)
module T = ANSITerminal
open Life.Main

(* Loop di Programma Principale *)
let _ = match Array.length(Sys.argv) with

  (* dune exec life <n_rounds> *)
  | 2 -> 
    let k = int_of_string (Sys.argv.(1)) in
    T.erase T.Screen;
    T.save_cursor();
    Random.self_init();
    let w = loop init_w k conway in
    display w;
    ignore(read_line());
    T.restore_cursor();
    print_newline()

  (* dune exec life <rule> <n_rounds> *)
  | 3 ->
    let k = int_of_string (Sys.argv.(2)) in
    let rule = eval (parse Sys.argv.(1)) in
    T.erase T.Screen;
    T.save_cursor();
    Random.self_init();
    let w = loop init_w k rule in
    display w;
    ignore(read_line());
    T.restore_cursor();
    print_newline()

  (* Debug: Ast Print *)
  | 4 when (Sys.argv.(3) = "print") ->
    let rule = eval (parse Sys.argv.(1)) in
    print_string("Survive: " ^ string_of_intlist(extract rule 1) ^ " Born: " ^ string_of_intlist(extract rule 2))

  (* Wrong Usage *)
  | _ -> failwith "Usage: dune exec life <n_rounds>"
