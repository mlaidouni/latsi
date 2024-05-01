open Ast

let lexbuf = Lexing.from_channel stdin 

let rec eval_expr env  = function
  | 
    (* | Var x -> List.assoc x env
       | True -> true
       | False -> false
       | Or(l,r) -> eval env l|| eval env r
       | And(l,r) -> eval env l && eval env r
       | Let(l,e) -> eval  ((List.map (fun (x,y) -> (x, eval env y)) l)@env)   e *)

    let ast = Parser.input Lexer.main lexbuf 
(* 
let _ = Printf.printf "Parse:\n%s\n" (Ast.as_string ast); 

      Printf.printf "Eval:\n%b\n" (eval [] ast); *)

let imprime env exprs =
  List.iter (fun expr ->
      let result = eval env expr in
      print_int result;
      print_newline ();
    ) exprs  


