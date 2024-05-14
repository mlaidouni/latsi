%{
open Ast  
%}

%token IMPRIME SI ALORS VAVERS ENTREE FIN REM NL EOF CR RPAR LPAR PLUS MOINS
%token MULT DIV EQUAL INF SUP VIRGULE SUPEQ INFEQ NEQ
%token<int> NOMBRE    
%token<string> VAR
%token<string> STRING


%start<Ast.programme> programme

%%

// Définition: {<ligne>} EOF
programme:
| ls = list(ligne) EOF { ls }

// Définition: <nombre> <instr> CR
ligne:
| n = NOMBRE i = instr CR {Ligne (n, i) }

// Définition d'une instruction
instr:
| IMPRIME el = exprlist { Imprime el }
| SI e1 = expression r = relop e2 = expression ALORS i = instr {SiAlors ( e1, r, e2, i)}
| VAVERS e = expression { Vavers e }
| ENTREE vl = varlist { Entree vl }
| v = VAR EQUAL e = expression { Affectation (v, e) }
| FIN { Fin }
| REM s = STRING { Rem s }
| NL { Nl }

// Définition: (<string> | <expression>) {, (<string> | <expression>)}
exprlist:
| ls = separated_list(VIRGULE, stringexpr) { ls }

// Définition: (<string> | <expression> ) pour les <exprlist>
stringexpr:
| s = STRING { String s }
| e = expression { Print_expression e }

// Définition: <var> {, <var>}
varlist:
| ls = separated_list(VIRGULE, VAR) { ls }

// Définition: <expression> {<plusmoins> <term>}
expression:
| t = term rest = expr_tail { (t, rest) }

expr_tail:
| pm = plusmoins t = term tail = expr_tail { (pm, t) :: tail }
| { [] }

term:
| f = facteur rest = term_tail { (f, rest) }

term_tail:
| md = multdiv f = facteur tail = term_tail { (md, f) :: tail }
| { [] }

// Définition: <var> | <nombre> | "("<expression>")" 
facteur:
| v = VAR { Var v }
| n = NOMBRE { Nombre n }
| LPAR e = expression RPAR { Expression e }

// Définition: <relop> pour les <instr>
relop:
| EQUAL { Equal }
| INF { Inf }
| SUP { Sup }
| INFEQ {Infeq}
| SUPEQ {Supeq}
| NEQ {Neq}

// Définition: opérateurs pour les <expression>
plusmoins:
| PLUS { Plus }
| MOINS { Moins }

// Définition: opérateurs pour les <term>
multdiv:
| MULT { Mult }
| DIV { Div }
