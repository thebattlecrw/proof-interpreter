(** 1. La logique des propositions *)

(** Q1 - Exemples de propositions

    i   - "S'il pleut alors j'ai mon parapluie, et si j'ai mon parapluie alors je ne suis pas mouillé."
    ii  - "Si ni le roi peut s'enfuir, ni on peut interposer une pièce entre le roi et l'attaquant, alors c'est pas échec et mat."
    iii - "Soit Aurore viendra et Martin ne viendra pas, soit Aurore ne viendra pas et Martin viendra."
    iv  - "Si je gagne, et que Mathieu perd ou fait match nul, alors je gagnerai le tournoi."
    v   - "S'il pleut et je n'ai pas mon parapluie alors soit je suis mouillé, soit il ne pleut pas".
    
 *)

(** 1.1. Syntaxe de la logique des propositions *)

(** Q1 - Propositions sous la forme d'expression de la logique propositionnelle

    i   - "(P -> Q) /\ (Q -> ~R)"
    ii  - "~(P \/ Q) -> R"
    iii - "(P /\ ~Q) \/ (~P \/ Q)"
    iv  - "(P /\ (Q \/ R)) -> S" 
    v   - "(P /\ ~Q) -> (R \/ ~P)"
    
 *)

(** Q2 - Représentation des expressions de la logique propositionnelle *)

type tformula =
  | True
  | False
  | Prop of string
  | Not of tformula
  | Impl of (tformula * tformula)
  | And of (tformula * tformula)
  | Or of (tformula * tformula)
;;

let prop_i = And(Impl(Prop("P"), Prop("Q")), Impl(Prop("Q"), Not(Prop("R"))));;
let prop_ii = Impl(Not(Or(Prop("P"), Prop("Q"))), Prop("R"));;
let prop_iii = Or(And(Prop("P"), Not(Prop("Q"))), And(Not(Prop("P")), Prop("Q")));;
let prop_iv = Impl(And(Prop("P"), Or(Prop("Q"), Prop("R"))), Prop("S"));;
let prop_v = Impl(And(Prop("P"), Not(Prop("Q"))), Or(Prop("R"), Not(Prop("P"))));;

(** Q3 - Transformation de tformula en chaine de caractères *)

let rec string_of_formula f =

  let add_brackets e str =
    match e with
    | Impl (_, _) | And (_, _) | Or (_, _) -> "(" ^ str ^ ")"
    | _ -> str
  in
  
  match f with
  | True -> "True"
  | False -> "False"
  | Prop(x) -> x
  | Not(e) -> "~" ^ add_brackets e (string_of_formula e)
  | Impl(e1, e2) -> (add_brackets e1 (string_of_formula e1)) ^ " ==> " ^ (add_brackets e2 (string_of_formula e2))
  | And(e1, e2) -> (add_brackets e1 (string_of_formula e1)) ^ " /\\ " ^ (add_brackets e2 (string_of_formula e2))
  | Or(e1, e2) -> (add_brackets e1 (string_of_formula e1)) ^ " \\/ " ^ (add_brackets e2 (string_of_formula e2))
;;

print_endline (string_of_formula prop_i);;
print_endline (string_of_formula prop_ii);;
print_endline (string_of_formula prop_iii);;
print_endline (string_of_formula prop_iv);;
print_endline (string_of_formula prop_v);;

(** 2.1. Interpréteur pour les formules *)

(** Q1 - Type valuation *)

type valuation = (tformula * bool) list;;

(** Q2 - Obtenir la valeur de vérité d'une formule *)

let give_value (f : tformula) (v : valuation) = List.assoc f v ;;

(** Q3 - Evaluation d'une expression logique **)

let rec eval f v =
  match f with
  | True -> true
  | False -> false
  | Prop(_) -> give_value f v
  | Not(e) -> not (eval e v)
  | Impl(e1, e2) -> (not (eval e1 v)) || (eval e2 v)
  | And(e1, e2) -> (eval e1 v) && (eval e2 v)
  | Or(e1, e2) -> (eval e1 v) || (eval e2 v)
;;

(** Q4 - Exemples *)

let valu : valuation = [(Prop "P", false); (Prop "Q", true); (Prop "R", true); (Prop"S", false)];;

print_endline (string_of_formula prop_i);;
eval prop_i valu ;;

print_endline (string_of_formula prop_ii);;
eval prop_ii valu ;;

print_endline (string_of_formula prop_iii);;
eval prop_iii valu ;;

print_endline (string_of_formula prop_iv);;
eval prop_iv valu ;;

print_endline (string_of_formula prop_v);;
eval prop_v valu ;;

(** 2.2. Sémantique via les tables de vérité *)

(** Q1 - Exemples de table de vérité
    
    ii. ~(P \/ Q) ==> R

    | P | Q | R | ~(P \/ Q) | ~(P \/ Q) ==> R |
    ===========================================
    | 0 | 0 | 0 |     1     |        0        |
    | 0 | 0 | 1 |     1     |        1        |
    | 0 | 1 | 0 |     0     |        1        |
    | 0 | 1 | 1 |     0     |        1        |
    | 1 | 0 | 0 |     0     |        1        |
    | 1 | 0 | 1 |     0     |        1        |
    | 1 | 1 | 0 |     0     |        1        |
    | 1 | 1 | 1 |     0     |        1        |

    iii. (P /\ ~Q) \/ (~P /\ Q)

    | P | Q | P /\ ~Q | ~P /\ Q | (P /\ ~Q) \/ (~P /\ Q) |
    ======================================================
    | 0 | 0 |   0     |    0    |           0            |
    | 0 | 1 |   0     |    1    |           1            |
    | 1 | 0 |   1     |    0    |           1            |
    | 1 | 1 |   0     |    0    |           0            |
    
 *)

(** Q2, Q3 - Liste des variables dans une expression

    La fonction [vars] éffectue un parcours en profondeur de l'expression [e] et met dans une liste toutes les variables
    (constructeurs [Prop]) trouvées dans l'expression.

 *)

let vars f =
  let rec aux e l =
    match e with
    | Prop(_) -> e::l
    | Not(e) -> aux e l
    | Impl(e1, e2) | And(e1, e2) | Or(e1, e2) -> aux e1 (aux e2 l)   
    | _ -> l
  in
  aux f []
;;
 
let drop l =
  List.fold_left (fun acc x -> if List.mem x acc then acc else x::acc) [] l 
;;

let set_vars f = drop (vars f);;

(** Q4 - Liste des valeurs de vérité

    La fonction [enumerate_one] calcule une ligne de la table de vérité
    pour une formule [e], une valuation [v] donnée, et une liste de variables [l].
    Elle retourne une liste de booléens correspondant aux valeurs des variables
    dans [v], suivies de la valeur de la formule [e] sous cette valuation.

    La fonction [enumerate_cases] construit la table de vérité complète
    de la formule [e]. Elle génère toutes les valuations possibles des variables
    de [e], puis applique [enumerate_one] à chacune d’elles.

    Le résultat est une liste de listes de booléens, où chaque sous-liste
    représente une ligne de la table de vérité.
    
 *)

let rec enumerate_one e v l =
  match l with
  | [] -> [eval e v]
  | hd::tl ->  (give_value hd v)::(enumerate_one e v tl)
;;

let enumerate_cases e =

  let rec aux l v res =
    match l with
    | [] -> (enumerate_one e v (set_vars e))::res
    | hd::tl ->
       aux tl ((hd, false)::v) (aux tl ((hd, true)::v) res)
  in

  aux (set_vars e) [] []
;;


(** Q5 - Exemples de tables de vérité *)

print_endline (string_of_formula prop_i) ;;
enumerate_cases prop_i ;;

print_endline (string_of_formula prop_ii);;
enumerate_cases prop_ii ;;

print_endline (string_of_formula prop_iii) ;;
enumerate_cases prop_iii ;;

print_endline (string_of_formula prop_iv) ;;
enumerate_cases prop_iv ;;

print_endline (string_of_formula prop_v) ;;
enumerate_cases prop_v ;;

(** 2.3. Tautologies *)

(** Q1 - Exemples de tautologies

    P \/ ~P :

    | P |~P | P \/ ~P |
    ===================
    | 0 | 1 |    1    |
    | 1 | 0 |    1    |


    P ==> P :

    | P | P ==> P |
    ===============
    | 0 |    1    |
    | 1 |    1    |
    
    
    (P /\ Q) ==> P

    | P | Q | P /\ Q |  (P /\ Q) ==> P  |
    =====================================
    | 0 | 0 |   0    |        1         |
    | 0 | 1 |   0    |        1         |
    | 1 | 0 |   0    |        1         |
    | 1 | 1 |   1    |        1         |

 *)

let taut_i = Or(Prop "P", Not(Prop "P")) ;;
let taut_ii = Impl(Prop "P", Prop "P");;
let taut_iii = Impl (And (Prop "P", Prop "Q"), Prop "P");;

print_endline(string_of_formula taut_i);;
enumerate_cases taut_i;;

print_endline(string_of_formula taut_ii);;
enumerate_cases taut_ii;;

print_endline(string_of_formula taut_iii);;
enumerate_cases taut_iii;;

(** Q2 - Tester si une formule est une tautologie

    La fonction [tautology] a une structure très similaire à la fonction [enumerate_cases], sauf qu'elle ne prend en compte que l'évaluation de
    l'expression pour chaque valuation sans devoir lire dans sa table de vérité. Elle retourne vrai si [f] est une tautologie.

 *)

(* Alternatif - Il est possible d'utiliser la fonction enumerate_cases mais cela serait moins efficace *)
let tautology f =

  let rec aux l v =
    match l with
    | [] -> (eval f v)
    | hd::tl ->
       (aux tl ((hd, true)::v)) && (aux tl ((hd, false)::v))
  in

  aux (set_vars f) []
;;

print_endline (string_of_formula prop_ii) ;;
tautology prop_ii;;

print_endline (string_of_formula prop_iii);;
tautology prop_iii;;

print_endline(string_of_formula taut_i);;
tautology taut_i;;

print_endline(string_of_formula taut_ii);;
tautology taut_ii;;

print_endline(string_of_formula taut_iii);;
tautology taut_iii;;

(** Q3 -

    L'inconvénient de la fonction [tautology] est sa complexité exponentielle selon le nombre de variables dans l'expression. Pour chaque variable
    dans l'expression, on multiplie par deux le nombre de valuations à tester.

    Par exemple, pour 3 variables P, Q, R, la fonction génère toutes les valuations possibles :
    (P, Q, R) --> (0,0,0); (0,0,1); (0,1,0); (0,1,1); ...; (1,1,1)
    soit 2^3 = 8 cas à traiter.
    Ainsi, pour 10 variables, cela donne 2^10 = 1024 valuations, ce qui montre une croissance exponentielle du nombre de cas à traiter.
    
    Cette complexité en O(2^n) rend la fonction impraticable dès que le nombre de variables devient trop grand, car le temps de calcul
    devient trop important.

 *)

(** 3. Preuves de formules propositionnelles *)

(** Q1 - (P \/ Q ==> R) ==> (P ==> R) /\ (Q ==> R)

    1. Impl_Intro (assume P ∨ Q ⇒ R)
    2. And_Intro  (Donne deux sous buts (P ==> R) et (Q ==> R)
       2.1 Impl_Intro (assume P)
          Assume     (assume P \/ Q)
          Impl_Elim  (récupère R de P \/ Q ==> R)
          Or_Intro_1 (récupère P de P \/ Q)
       2.2 Impl_Intro (assume Q)
          Assume     (assume P \/ Q)
          Impl_Elim  (récupère R de P \/ Q ==> R)
          Or_Intro_2 (récupère Q de P \/ Q)
 *)

(** 3.1. Les buts de preuve *)

(** Q1, Q2 - Représentation et affichages des buts de preuve

    Les entiers sont plus efficaces et simples à manipuler pour la représentation interne des identifiants. Par exemple pour
    créer des nouveaux identifiants, il s'agit juste de parcourir le contexte une fois pour récupérer l'identifiant maximal
    et puis de l'incrémenter.
    De plus, les entiers permettent une comparaison rapide et évitent les problèmes liés à la manipulation de chaînes de caractères

    L'inconvénient de cette approche est que identificateurs entiers ne sont pas directement lisibles par l’utilisateur, ce qui nécessite
    une fonction d’affichage pour produire des noms comme "H", "H0", etc. 

 *)

type ident = int;;

type goal =
  ((ident * tformula) list * tformula)
;;

let string_of_ident i =
  if i < -1 then failwith "print_ident error : identifier >= -1"
  else if i = -1 then "H"
  else "H" ^ (string_of_int i)
;;

let print_goal g =
  let rec aux context goal =
    match context with
    | hd::tl ->
       (
         print_string (string_of_ident(fst hd));
         print_string " : ";
         print_endline (string_of_formula (snd hd));
         aux tl goal
       )
    | [] ->
       (
         print_endline "======================";
         print_endline (string_of_formula goal)
       )
  in

  (
    print_endline "";
    aux (fst g) (snd g)
  )
;;

(* Exemple *)
let context = [(-1, Impl (Or (Prop "P", Prop "Q"), Prop "R")); (2, Prop "P")] ;;
let goal = Or (Prop "P", Prop "Q");;

print_goal (context, goal);;

(** 3.2. Les tactiques de preuve *)

type tactic =
  | And_Intro
  | Impl_Intro
  | Or_Intro_1
  | Or_Intro_2
  | Not_Intro
  | And_Elim_1 of string
  | And_Elim_2 of string
  | Or_Elim of string 
  | Impl_Elim of string * string
  | Not_Elim of string * string
  | Exact of string
  | Assume of tformula
;;

(** 3.3. Attribution des tactiques à un but *)

(** Q1 - Recherche d'un identifiant non-utilisé *)

let fresh_ident context =
  let curr = List.fold_left (fun acc x -> if (fst x) > acc then (fst x) else acc) (-1) context in
  curr + 1
;;


(** Q2 - Vérification d'identifiant *)

let ident_of_string s =
  if s = "H" then -1
  else if String.length s > 1 && s.[0] = 'H' then
    try int_of_string (String.sub s 1 (String.length s - 1)) (* Récupère l'entier présent dans l'identifiant *)
    with Failure _ -> -2
  else -2
;;

let rec valid_ident id context =
  if id = -2 then false
  else    
    match context with
    | [] -> false
    | hd::tl -> if id = (fst hd) then true else valid_ident id tl
;;


(** Q3 - Fonction d'application de tactiques *)

let apply_tactic t g =
  let context = fst g
  and goal = snd g
  in

  match t with
  | And_Intro -> ( (* Create 2 new subgoals for each element of And *)
    match goal with
    | And (e1, e2) -> [(context, e1); (context, e2)]
    | _ -> failwith "apply_tactic : Goal is not an And-formula" )

  | Or_Intro_1 -> ( (* Leave left element in goal, remove right element *)
    match goal with
    | Or (e, _) -> [(context, e)]
    | _ -> failwith "apply_tactic : Goal is not an Or-formula" )

  | Or_Intro_2 -> ( (* Leave right element in goal, remove left element *)
    match goal with
    | Or (_, e) -> [(context, e)]
    | _ -> failwith "apply_tactic : Goal is not an Or-formula" ) 

  | Impl_Intro -> ( (* Left element becomes new hypothesis, right element as goal *)
    match goal with
    | Impl (e1, e2) -> [((fresh_ident context, e1)::context, e2)]
    | _ -> failwith "apply_tactic : Goal is not an Implication-formula" )

  | Not_Intro -> ( (* Introduce the the element as hypothesis, goal becomes False *)
    match goal with
    | Not (e) -> [((fresh_ident context, e)::context, False)]
    | _ -> failwith "apply_tactic : Goal is not a Not-formula" )

  | And_Elim_1 (hyp) -> ( (* Introduce left side of And formula as hypothesis *)
    let id = ident_of_string hyp in
    if (valid_ident id context) then (
      let expr = List.assoc id context in
      match expr with
      | And (e1, e2) -> [((fresh_ident context, e1)::context, goal)]
      | _ -> failwith "apply_tactic : Hypothesis is not an And-formula" )
    else failwith "apply_tactic : Invalid hypothesis"  )
    
  | And_Elim_2 (hyp) -> ( (* Introduce right side of And formula as hypothesis *)
    let id = ident_of_string hyp in 
    if (valid_ident id context) then (
      let expr = List.assoc id context in
      match expr with
      | And (e1, e2) -> [((fresh_ident context, e2)::context, goal)]
      | _ -> failwith "apply_tactic : Hypothesis is not an And-formula" )
    else failwith "apply_tactic : Invalid hypothesis"  )

  | Or_Elim (hyp) -> ( (* Create 2 subgoals where hyp becomes either side of the Or formula *)
    let id = ident_of_string hyp in
    if (valid_ident id context) then (
      let expr = List.assoc id context in
      match expr with
      | Or (e1, e2) ->
         let context1 = List.map (fun e -> if (fst e) = (ident_of_string hyp) then (fst e, e1) else e) context
         and context2 = List.map (fun e -> if (fst e) = (ident_of_string hyp) then (fst e, e2) else e) context in
         [(context1, goal); (context2, goal)]
      | _ -> failwith "apply_tactic : Hypothesis is not an Or-formula" )
    else failwith "apply_tactic : Invalid hypothesis"  )

  | Impl_Elim (h1, h2) -> ( (* Check if left side of h1 = h2, create new hypothesis with right side of h1 *)
    let id1 = ident_of_string h1 and id2 = ident_of_string h2 in 
    if (valid_ident id1 context) && (valid_ident id2 context) then (
      let expr1 = List.assoc id1 context
      and expr2 = List.assoc id2 context in
      match expr1 with
      | Impl (e1, e2) -> (
        if expr2 = e1 then [((fresh_ident context, e2)::context, goal)]
        else failwith "apply_tactic : Second hypothesis doesn't match the assumption of the first hypothesis." )
      | _ -> failwith "apply_tactic : Hypothesis is not an Implication-formula" )
    else failwith "apply_tactic : Invalid hypothesis" )

  | Not_Elim (h1, h2) -> ( (* Check if the not element = h2, create new hypothesis with false *)
    let id1 = ident_of_string h1 and id2 = ident_of_string h2 in 
    if (valid_ident id1 context) && (valid_ident id2 context) then (
      let expr1 = List.assoc id1 context
      and expr2 = List.assoc id2 context in
      match expr1 with
      | Not (e) -> (
        if expr2 = e then [((fresh_ident context, False)::context, goal)]
        else failwith "apply_tactic : Second hypothesis doesn't match the body of the first hypothesis." )
      | _ -> failwith "apply_tactic : Hypothesis is not a Not-formula" )
    else failwith "apply_tactic : Invalid hypothesis" )

  | Exact (hyp) -> ( (* Check if the hypothesis = goal *)
    let id = ident_of_string hyp in
    if (valid_ident id context) then (
      let expr = List.assoc id context in
      if expr = goal then []
      else failwith ( "apply_tactic : The term " ^ hyp ^ " doesn't match the expected term in goal" ) )
    else failwith "apply_tactic : Invalid hypothesis" )

  | Assume (form) -> ( (* Add new formula to context, add new goal with form in old context *)
    [((fresh_ident context, form)::context, goal); (context, form)] )
;;


(** Q4 - Preuve de (P \/ Q ==> R) ==> (P ==> R) /\ (Q ==> R)

    - [print_current_goal] : Prend la liste des buts et affiche le premier élément si elle n'est pas vide.
    - [run_proof] : La fonction [apply_tactic] prend la tête de la liste des sous-buts et la transforme en 0, 1, ou 2 sous-buts. La fonction
    [run_proof] va donc permettre de gérer l'état globale de la preuve, en applicant une tactique au premier sous-but de la liste et en remplacant
    ce but par les sous-buts générés, tout en conservant les autres sous-buts inchangés.

 *)

let print_current_goal l =
  match l with
  | [] -> print_endline "All goals completed."
  | hd::tl -> print_goal hd
;;

let run_proof t goals =
  match goals with
  | [] -> []
  | goal :: rest ->
      let new_goals = apply_tactic t goal in
      new_goals @ rest
;;
    
let goal = ([], Impl (Impl (Or (Prop "P", Prop "Q"), Prop "R"), And (Impl (Prop "P", Prop "R"), Impl (Prop "Q", Prop "R")))) ;;
let goals = [goal] ;;

let g1 = run_proof Impl_Intro goals ;;
print_current_goal g1 ;;

let g2 = run_proof And_Intro g1 ;;
print_current_goal g2 ;;

let g3 = run_proof Impl_Intro g2 ;;
print_current_goal g3 ;;

let g4 = run_proof (Assume (Or (Prop "P", Prop "Q"))) g3 ;;
print_current_goal g4 ;;

let g5 = run_proof (Impl_Elim ("H0", "H2")) g4 ;;
print_current_goal g5 ;;

let g6 = run_proof (Exact ("H3")) g5 ;;
print_current_goal g6 ;;

let g7 = run_proof Or_Intro_1 g6 ;;
print_current_goal g7 ;;

let g8 = run_proof (Exact ("H1")) g7 ;;
print_current_goal g8 ;;

let g9 = run_proof Impl_Intro g8 ;;
print_current_goal g9 ;;

let g10 = run_proof (Assume (Or (Prop "P", Prop "Q"))) g9 ;;
print_current_goal g10 ;;

let g11 = run_proof (Impl_Elim ("H0", "H2")) g10 ;;
print_current_goal g11 ;;

let g12 = run_proof (Exact ("H3")) g11 ;;
print_current_goal g12 ;;

let g13 = run_proof Or_Intro_2 g12 ;;
print_current_goal g13 ;;

let g14 = run_proof (Exact ("H1")) g13 ;;
print_current_goal g14 ;;


(** 4. Analyse lexicale et syntaxique et boucle d’interaction

    cf. README.md

 *)
