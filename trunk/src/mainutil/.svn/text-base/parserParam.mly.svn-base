/* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution */

%token TK_EOF
%token TK_COMMA TK_COLON TK_EQUAL
%token <string> TK_ID
%token <string> TK_STR

%type <string * (string, string) Mappe.t> param

%start param

%%

param: TK_ID TK_EOF { ($1,Mappe.empty) }
| TK_ID TK_COLON options TK_EOF { ($1,$3) }

options: option { let (v,e) = $1 in Mappe.add v e Mappe.empty }
| option TK_COMMA options { let (v,e) = $1 in  Mappe.add v e $3 }

option: TK_ID { ($1,"") }
| TK_ID TK_EQUAL TK_ID { ($1,$3) }
| TK_ID TK_EQUAL TK_STR { ($1,$3) }

/*Format.pp_print_string Format.std_formatter $3; */
