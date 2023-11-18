%{

  open Lexing
  open Kawa

%}

%token <int> INT
%token <string> IDENT
%token MAIN
%token LPAR RPAR BEGIN END SEMI COMMA
%token PRINT IF ELSE WHILE RETURN SET
%token VAR
%token CLASS EXTENDS ATTRIBUTE NEW THIS (*classe*)
%token TINT TBOOL TVOID
%token DOT
%token EOF
%token TRUE FALSE
%token PLUS STAR MINUS DIV MODULO (*arithmetique*)
%token LT LTE GT GTE ISEQUAL NOTEQUAL
%token AND OR
%token NOT

(* Priorités par ordre croissante de priorités*)
%left OR
%left AND
%right NOT
%left LT LTE GT GTE ISEQUAL NOTEQUAL
%left PLUS MINUS
%left STAR DIV MODULO
%nonassoc DOT  (*priorité la plus élevée pour accéder à un attribut ?*)

%start program
%type <Kawa.program> program

%%

program:
| global_var=list(var_global) cls=list(class_def) MAIN BEGIN main=list(instruction) END EOF
    { {classes=cls; globals=global_var; main} }
;

var_global :
|VAR t=type_decl name=IDENT SEMI {(name,t)}
;

type_decl:
| TINT { TInt }
| TBOOL { TBool }
| TVOID { TVoid }
| x=IDENT { TClass x }
;

(*rajouter les methodes*)
class_def:
| CLASS cls=IDENT BEGIN attr=list(attribute_declaration) END { {class_name=cls; attributes=attr; methods=[]; parent=None}}
| CLASS cls=IDENT EXTENDS parent_name=IDENT BEGIN attr=list(attribute_declaration) END {{class_name=cls; attributes=attr; methods=[]; parent=Some parent_name}}
;

attribute_declaration:
| ATTRIBUTE t=type_decl attr=IDENT SEMI {(attr, t)}
;

else_branch:
| ELSE BEGIN seq2=list(instruction) END { seq2 }
;

instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
(*branchement conditionnel*)
| IF LPAR e=expression RPAR BEGIN seq=list(instruction) END seq2=loption(else_branch) { If(e, seq, seq2) }
(*boucle while*)
| WHILE LPAR e=expression RPAR BEGIN seq=list(instruction) END { While(e, seq) }
(*expression utilisee comme instruction : pour l'appel de fonctions*)
| e=expression SEMI { Expr e }
(*return*)
| RETURN e=expression SEMI { Return e}
(*affectation*)
| x=memory_access SET e=expression SEMI {Set(x, e)}
;

memory_access:
| x=IDENT {Var x}
| e=expression DOT attr=IDENT { Field(e, attr) }
;

expression:
| n=INT { Int n }
| TRUE  { Bool true }
| FALSE { Bool false }
| LPAR e=expression RPAR {e}
| v=memory_access {Get(v)}
(*operations*)
| e1=expression op=binop e2=expression { Binop(op, e1, e2) }
| op=unop e=expression { Unop(op, e) }
(*classes*)
| NEW x=IDENT {New x}
| NEW x=IDENT LPAR params=separated_list(COMMA, expression) RPAR {NewCstr(x,params) } 
;

%inline binop:
| PLUS { Add }
| STAR { Mul }
| MINUS { Sub }
| DIV { Div }
| MODULO { Rem }
| LT { Lt }
| LTE { Le }
| GT { Gt }
| GTE {Ge} 
| ISEQUAL {Eq}
| NOTEQUAL {Neq}
| AND {And}
| OR {Or}
;

%inline unop:
| NOT { Not }
| MINUS { Opp }
;
