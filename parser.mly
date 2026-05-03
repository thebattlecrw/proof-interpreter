%{
    open Prover ;;
    
    try Sys.command "clear" with Failure _ -> -2 ;;
    flush stdout ;;
    let goals : goal list ref = ref [] ;;
    let set_goals (goal : tformula) = 
        goals := [([], goal)] ;  
        print_current_goal !goals ;;
    let run_tactic t = goals := run_proof t !goals;;
    let qed () =
      match !goals with
      | [] -> print_endline "Proof completed successfully. Qed."
      | _ -> ( print_endline "Error : there are still open goals."; print_current_goal !goals )
    ;;
%}

%token <string> PROP
%token <string> HYP
%token AND NOT OR IMPL EOF PROOF END
%token AND_INTRO OR_INTRO_1 OR_INTRO_2 IMPL_INTRO NOT_INTRO AND_ELIM_1 AND_ELIM_2 OR_ELIM IMPL_ELIM NOT_ELIM EXACT ASSUME
%token LPAREN RPAREN
%token QED

%right IMPL
%left OR
%left AND
%right NOT

%start main
%type <unit> main
%type <tformula> expr
%type <tactic> tactic

%%

main:
  | command { $1 }

command:
  | PROOF expr END      { set_goals $2 }
  | tactic END          { run_tactic $1; print_current_goal !goals }
  | expr END            { print_endline (string_of_formula $1) }
  | QED END             { qed () }
  | EOF                 { raise End_of_file }
  
tactic:  
  | AND_INTRO           { And_Intro }
  | OR_INTRO_1          { Or_Intro_1 }
  | OR_INTRO_2          { Or_Intro_2 }
  | IMPL_INTRO          { Impl_Intro }
  | NOT_INTRO           { Not_Intro }
  | AND_ELIM_1 HYP      { And_Elim_1 ($2) }
  | AND_ELIM_2 HYP      { And_Elim_2 ($2) }
  | OR_ELIM HYP         { Or_Elim ($2) }
  | IMPL_ELIM HYP HYP   { Impl_Elim ($2, $3) }
  | NOT_ELIM HYP HYP    { Not_Elim ($2, $3) }
  | EXACT HYP           { Exact ($2) }
  | ASSUME expr         { Assume ($2) }
  
expr:  
  | PROP                { Prop $1 }
  | expr AND expr       { And ($1, $3) }
  | expr OR expr        { Or ($1, $3) }
  | expr IMPL expr      { Impl ($1, $3) }
  | NOT expr            { Not ($2) }
  | LPAREN expr RPAREN  { $2 }

%%
