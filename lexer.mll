{
open Parser
}

let layout = [ ' ' '\t' '\n' ]
let string = ['[' ' ' ',' ''' '_' ';' ':' '(' ')' '.' 'a'-'z' 'A'-'Z' ']' '*']
(* let relop = '<' ['>' | '=']? | '>' ['<' | '=']?  | '=' *)
let var = ['A'-'Z']
let chiffre = ['0'-'9']
let nombre = chiffre+

rule main = parse
  |  layout    { main lexbuf }
  | "IMPRIME"  { IMPRIME }
  | "SI"       { SI }
  | "ALORS"    { ALORS }
  | "VAVERS"   { VAVERS }
  | "ENTREE"   { ENTREE }
  | "FIN"      { FIN }
  | "REM"      { REM }
  | "NL"       { NL }
  | eof        { EOF }
  | '\n'       { CR }
  | ')'        { RPAR }
  | '('        { LPAR }
  | '+'        { PLUS }
  | '-'        { MOINS }
  | '*'        { MULT }
  | '/'        { DIV }
  | '='        { EQUAL }
  | '<'        { INF }
  | '>'        { SUPP }
  | '"'        { QUOTE }
  | ','        { VIRGULE }
  | var        { VAR (Lexing.lexeme lexbuf) }
  | nombre     { NOMBRE (Lexing.lexeme lexbuf) }
  | string+    { STRING (Lexing.lexeme lexbuf) }
  | _          { failwith "unexpected character" }
