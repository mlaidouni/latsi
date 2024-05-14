type plusmoins = Plus | Moins

type multdiv = Mult | Div

type var = string 
and varlist= var list

type nombre = int 

type facteur = Var of var | Nombre of int | Expression of expression

and relop = Equal | Inf | Infeq | Sup | Supeq | Neq

and term=facteur * (multdiv * facteur) list 

and ligne = Ligne of (nombre  * instr)

and programme = ligne list

and expression = term * (plusmoins * term) list

and type_print =String of string | Print_expression of expression

and instr =
  | Imprime of (type_print list)
  | SiAlors of  expression * relop * expression * instr
  | Vavers of expression
  | Entree of varlist
  | Affectation of var * expression
  | Fin
  | Rem of string
  | Nl

(* Définition des exceptions *)
exception Undifined_var of string
exception Empty_program

(* Définition de la variable qui env qui stock la ligne actuel et les variables qui ont été assigées *)
type env = {
    programme : programme;
    mutable var_liste : (var*int) list ;
    mutable index : int;
  }

(* Vérifie que la variable s est bien défini *)
let rec get_var s = function 
|[]->None 
|(a,b)::t-> if (String.equal s a) then Some b else get_var s t

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
let rec print_expression env = function 
|[]->()
|(String s)::t -> print_string s; print_expression env t
|(Print_expression e)::t -> print_int (eval_expression env e);print_expression env t

let new_var s v env = match (get_var s env.var_liste ) with 
|None -> env.var_liste <- ((s,v) :: env.var_liste) 
|Some x ->env.var_liste<- (s,v)::List.filter (fun a -> a!=(s,x)) env.var_liste

let rec eval_instruction env = function 
|Imprime l -> print_expression env l; print_newline(); env.index<-env.index+1
|SiAlors (e1, r, e2, instruction)-> if (eval_relop r (eval_expression env e1) (eval_expression env e2 )) then 
  eval_instruction env instruction 
  else env.index<-env.index+1
|Vavers _ -> ()
|Entree _ -> ()
|Affectation (s,e)-> new_var s (eval_expression env e ) env ; env.index<-env.index+1
|Fin -> exit 0
|Rem _-> env.index<-env.index+1
|Nl-> print_newline(); env.index<-env.index+1


let eval_ligne env =function
|Ligne(_,i)-> eval_instruction env i