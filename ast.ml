(* Définitions de types pour les opérations *)
type plusmoins = Plus | Moins
type multdiv = Mult | Div

(* Définition du type nombre *)
type nombre = int

(* Définitions de type pour les variables *)
type varlist = var list
and var = string

(* Définitions de type pour les expressions, termes, facteurs & imprimables *)
type facteur = Var of var | Nombre of int | Expression of expression
and term = facteur * (multdiv * facteur) list
and expression = term * (plusmoins * term) list
and relop = Equal | Inf | Infeq | Sup | Supeq | Neq
and type_print = String of string | Print_expression of expression

(* Définition de type pour une instruction *)
type instr =
  | Imprime of type_print list
  | SiAlors of expression * relop * expression * instr
  | Vavers of expression
  | Entree of varlist
  | Affectation of var * expression
  | Fin
  | Rem of string
  | Nl

(* Définition de type pour un programme & une ligne *)
type programme = ligne list
and ligne = Ligne of (nombre * instr)

(* Définition des exceptions *)
exception Undifined_var of string
exception Empty_program

(* Définition de la variable qui env qui stock la ligne actuel et les variables qui ont été assigées *)
type env = {
  programme : programme;
  mutable var_liste : (var * nombre) list;
  mutable index : int;
}

(* Vérifie que la variable s est bien défini *)
let rec get_var s = function
  | [] -> None
  | (a, b) :: t -> if String.equal s a then Some b else get_var s t

(* Fonction d'évaluation pour les opérations de termes *)
let eval_plusmoins = function Plus -> ( + ) | Moins -> ( - )

(* Fonction d'évaluation pour les opérations de facteurs *)
let eval_multdiv = function
  | Mult -> ( * )
  (* Gérer la division par zéro *)
  | Div ->
      fun x y -> if y = 0 then failwith "WARNING: Division by zero !" else x / y

(* Fonction d'évaluation des opérateurs relop *)
let eval_relop = function
  | Equal -> ( == )
  | Inf -> ( < )
  | Infeq -> ( <= )
  | Sup -> ( > )
  | Supeq -> ( >= )
  | Neq -> ( <> )

(* Fonction d'évaluation d'un facteur *)
let rec eval_facteur env = function
  | Nombre x -> x
  | Var v -> (
      match get_var v env.var_liste with
      (* Si la variable n'est pas définie on lève une excepetion *)
      | None -> raise (Undifined_var "WARNING: Variable not defined !")
      | Some x -> x)
  | Expression e -> eval_expression env e

(* Fonction d'évaluation d'un terme *)
and eval_term env = function
  | a, [] -> eval_facteur env a
  | a, (b, c) :: t ->
      eval_term env
        (Nombre (eval_multdiv b (eval_facteur env a) (eval_facteur env c)), t)

(* Fonction d'évaluation d'une expression *)
and eval_expression (env : env) = function
  | a, [] -> eval_term env a
  | a, (b, c) :: t ->
      eval_expression env
        ((Nombre (eval_plusmoins b (eval_term env a) (eval_term env c)), []), t)

(* Vérifie quel type doit être imprimé *)
let rec print_expression env = function
  | [] -> ()
  | String s :: t ->
      print_string s;
      print_expression env t
  | Print_expression e :: t ->
      print_int (eval_expression env e);
      print_expression env t

(* Fonction qui crée une nouvelle variable *)
let new_var s v env =
  match get_var s env.var_liste with
  | None -> ()
  | Some x ->
      env.var_liste <-
        (s, v) :: List.filter (fun a -> a != (s, x)) env.var_liste

(* Fonction qui gère l'entrée de valeurs pour des variables *)
let rec enter env = function
  | v :: t -> (
      try
        print_endline ("Reading input for variable \"" ^ v ^ "\" ...");
        (* Utilisation de input_line avec stdin *)
        let nb = input_line stdin in
        let x = int_of_string nb in
        new_var v x env;
        enter env t
      with
      | End_of_file -> print_endline "WARNING: End of input !"
      (* Si l'entrée est invalide (pas un entier), alors on redemande. *)
      | Failure _ ->
          print_endline "WARNING: Invalid input format !";
          enter env (v :: t))
  | [] -> ()

(* Renvoie l'indice, dans la liste env.programme, de la ligne d'index demandée, ou -1 sinon *)
let rec find_index listligne index i =
  match listligne with
  | [] -> -1
  | Ligne (n, _) :: t -> if n = index then i else find_index t index (i + 1)

let find_ligne_index listligne index = find_index listligne index 0

(* Fonction d'évaluation de l'instruction VAVERS *)
let eval_vavers env e =
  if find_ligne_index env.programme (eval_expression env e) != -1 then
    env.index <- find_ligne_index env.programme (eval_expression env e)
  else raise (Undifined_var "WARNING: Line of index requested is not defined !")

(* Fonction d'évaluation des instructions *)
let rec eval_instruction env = function
  | Imprime l ->
      print_expression env l;
      print_newline ();
      env.index <- env.index + 1
  | SiAlors (e1, r, e2, instruction) ->
      if eval_relop r (eval_expression env e1) (eval_expression env e2) then
        eval_instruction env instruction
      else env.index <- env.index + 1
  | Vavers e -> eval_vavers env e
  | Entree vl ->
      enter env vl;
      env.index <- env.index + 1
  | Affectation (s, e) ->
      new_var s (eval_expression env e) env;
      env.index <- env.index + 1
  | Fin -> exit 0
  | Rem _ -> env.index <- env.index + 1
  | Nl ->
      print_newline ();
      env.index <- env.index + 1

(* Fonction d'évaluation d'une ligne *)
let eval_ligne env = function Ligne (_, i) -> eval_instruction env i
