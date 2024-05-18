open Ast

(* On prend le programme à lire en argument. *)
let lexbuf = Lexing.from_channel (open_in Sys.argv.(1))

let init_var_list =
  [
    ("A", 0);
    ("B", 0);
    ("C", 0);
    ("D", 0);
    ("E", 0);
    ("F", 0);
    ("G", 0);
    ("H", 0);
    ("I", 0);
    ("J", 0);
    ("K", 0);
    ("L", 0);
    ("M", 0);
    ("N", 0);
    ("O", 0);
    ("P", 0);
    ("Q", 0);
    ("R", 0);
    ("S", 0);
    ("T", 0);
    ("U", 0);
    ("V", 0);
    ("W", 0);
    ("X", 0);
    ("Y", 0);
    ("Z", 0);
  ]

(* Fonction pour initialiser l'environnement *)
let initialize_env programme =
  { Ast.programme; Ast.index = 0; Ast.var_liste = init_var_list }

(* Fonction récursive pour évaluer les instructions *)
let rec eval_aux env =
  let (Ast.Ligne (_, instr)) = List.nth env.Ast.programme env.Ast.index in
  Ast.eval_instruction env instr;
  eval_aux env

(* Fonction principale pour évaluer le programme *)
let eval (p : Ast.programme) =
  let programme =
    List.sort (fun (Ast.Ligne (x, _)) (Ast.Ligne (y, _)) -> compare x y) p
  in
  if programme = [] then raise Empty_program
  else
    let env = initialize_env programme in
    eval_aux env

let ast = Parser.programme Lexer.main lexbuf
let () = eval ast
