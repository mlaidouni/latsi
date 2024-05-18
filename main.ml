open Ast

(* On prend le programme à lire en argument. *)
let lexbuf = Lexing.from_channel (open_in Sys.argv.(1))

(* Fonction pour initialiser l'environnement *)
let initialize_env programme =
  { Ast.programme; Ast.index = 0; Ast.var_liste = [] }

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
