open Ast

let lexbuf = Lexing.from_channel stdin 

let eval (p : Ast.programme) =
  let programme =
    List.sort (fun (Ast.Ligne(x, _)) (Ast.Ligne(y, _)) -> compare x y) p
    |> List.filter (fun x -> match x with Ast.Ligne(_, Ast.Rem _ )-> false | _ -> true)
  in
  if programme = [] then raise Empty_program
  else
    let env = { Ast.programme; Ast.index =  0; Ast.var_liste =  [] } in
    let rec aux () =
      let Ast.Ligne (_, instr) = List.nth programme env.Ast.index in
Ast.eval_instruction env instr
;
      aux ()
    in
    aux ()

let ast = Parser.programme Lexer.main lexbuf 


let () = eval ast 
    