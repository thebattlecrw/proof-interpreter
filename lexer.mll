{
  open Parser
}

rule token = parse
    | [ ' ' '\t' '\n' ]		{ token lexbuf }
    | "/\\"             	{ AND }
    | "\\/" 			{ OR }
    | "==>" 			{ IMPL }
    | "~"			{ NOT }
    | ['A'-'Z']+ as s		{ PROP s }
    | eof 	    	       	{ EOF }
    | "."			{ END }
    | "Proof"			{ PROOF }

    | ('H'['0'-'9']*) as h 	{ HYP h }
    | "And_Intro" 	 	{ AND_INTRO }
    | "Or_Intro_1" 		{ OR_INTRO_1 }
    | "Or_Intro_2"     		{ OR_INTRO_2 }
    | "Impl_Intro" 		{ IMPL_INTRO }
    | "Not_Intro" 		{ NOT_INTRO }
    | "And_Elim_1"		{ AND_ELIM_1 }
    | "And_Elim_2" 		{ AND_ELIM_2 }
    | "Or_Elim"			{ OR_ELIM }
    | "Impl_Elim" 		{ IMPL_ELIM }
    | "Not_Elim" 		{ NOT_ELIM }
    | "Exact" 			{ EXACT }
    | "Assume" 			{ ASSUME }
    | _ as c { print_endline ("Unknown char: " ^ String.make 1 c); token lexbuf } 
    
 
