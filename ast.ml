(* Définition des types *)
type plusmoins = Plus | Moins

type multdiv = Mult | Div

type var = string 

type facteur = Var of var | Nombre of int | Expression of expression


and relop = Equal | Inf | Infeq | Sup | Supeq | Neq


and term=facteur * (multdiv * facteur) list 


and ligne = int * instr

and expression = term * (plusmoins * term) list

and type_print =String of string | Print_expression of expression

and instr =
  | Imprime of (type_print list)
  | SiAlors of  expression * relop * expression * instr
  | Vavers of expression
  | Affectation of var * expression
  | Fin
  | Rem of string
  | Nl

(* Définition des exceptions *)
exception Undifined_var of string

(* Définition de la variable qui env qui stock la ligne actuel et les variables qui ont été assigées *)
type env = {
    ligne_actuelle : int * instr;
    (* curr_i : int ref; location in the list *)
    var_liste : (var*int) list;
  }

  (* Vérifie que la variable s est bien défini *)
let rec get_var s = function 
|[]->None 
|(a,b)::t-> if (String.equal a s) then Some b else get_var s t

(* Fonction d'évaluation pour les différent types *)
let eval_plusmoins = function 
|Plus-> ( + )
|Moins -> ( - )

let eval_multdiv = function 
|Mult-> ( * )
(* Gérer la division par zéro *)
| Div -> fun x y -> if y = 0 then failwith "Division par zéro" else x / y

let eval_relop = function 
|Equal -> ( == )
|Inf -> ( < )
|Infeq -> ( <= )
|Sup ->( > )
|Supeq -> ( >= )
|Neq-> (<>)

let rec eval_facteur env = function 
|Nombre x -> x
|Var v->( match get_var v env.var_liste with 
        (* Si la variable n'est pas définie on lève une excepetion *)
        |None -> raise (Undifined_var "La variable n'est pas défini")
        |Some x -> x)
|Expression e-> eval_expression env e 

and eval_term env= function 
|(a,[])-> eval_facteur env a
|(a, ((b,c)::t))-> eval_term env (Nombre (eval_multdiv b (eval_facteur env a) (eval_facteur env c)),t) 

and eval_expression (env : env)= function 
|(a,[])-> eval_term env a
|(a, ((b,c)::t))-> eval_expression env ((Nombre (eval_plusmoins b (eval_term env a) (eval_term env c)),[]),t)

(* Vérifie quel type doit être imprimé*)
let print_expression env = function 
|String s -> print_string s
|Print_expression e -> print_int (eval_expression env e) 