type plusmoins = Plus | Moins
type multdiv = Mult | Div


type instr =
  | Imprime of (expression list)
  | SiAlors of plusmoins * expression * expression * instr
  | Vavers of expression
  | Affectation of var * expression
  | Fin
  | Rem of string
  | Nl

type ligne = int * instr

type relop = Equal | Inf | Infeq | Sup | Supeq | Neq

type var = Var of string

type nombre = Nombre of int

type facteur = Var of string | Nombre of int
