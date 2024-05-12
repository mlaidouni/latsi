{
open Parser
}

let layout = [ ' ' '\t']
let string = ['"'] ([' ' ''' 'a'-'z' 'A'-'Z' '0'-'9' '+' '*' ',' ';' ':' '(' ')' '.' '_'] | '\\' ['"'])* ['"']
let var = ['A'-'Z']
let nombre = ['0'-'9']+

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
  | '>'        { SUP }
  | "<>"       { NEQ }
  | "<="       { INFEQ }
  | ">="       { SUPEQ }
  (* | '"'        { QUOTE } *)
  | ','        { VIRGULE }
  | var        { VAR (Lexing.lexeme lexbuf) }
  | nombre     { NOMBRE (int_of_string(Lexing.lexeme lexbuf)) }
  | string+    { STRING (Lexing.lexeme lexbuf) }
  | _          { failwith "unexpected character" }
