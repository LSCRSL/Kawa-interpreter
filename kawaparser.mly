%{

  open Lexing
  open Kawa

%}

%token <int> INT
%token <string> IDENT
%token MAIN
%token LPAR RPAR BEGIN END SEMI COMMA
%token PRINT IF ELSE WHILE RETURN SET
%token CLASS EXTENDS ATTRIBUTE
%token TINT
%token DOT
%token EOF
%token TRUE FALSE
%token PLUS STAR MINUS DIV MODULO (*arithmetique*)
%token LT LTE GT GTE ISEQUAL NOTEQUAL
%token AND OR
%token NOT

(* Priorités *)
%left OR
%left AND
%right NOT
%left LT LTE GT GTE ISEQUAL NOTEQUAL
%left PLUS MINUS
%left STAR DIV MODULO

%start program
%type <Kawa.program> program

%%

program:
| cls=list(class_def) MAIN BEGIN main=list(instruction) END EOF
    { {classes=cls; globals=[]; main} }
;

(*à completer*)
type_decl:
| TINT { TInt }
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
(*expression utilisee comme instruction*)
| e=expression SEMI { Expr e }
| RETURN e=expression SEMI { Return e}
(*affectation*)
| x=memory_access SET e=expression SEMI {Set(x, e)}
;

memory_access:
| x=IDENT {Var x}
| e=expression DOT attr=IDENT { Field(e, attr) }

expression:
| n=INT { Int n }
| TRUE  { Bool true }
| FALSE { Bool false }
| LPAR e=expression RPAR {e}
(*operations*)
| e1=expression op=binop e2=expression { Binop(op, e1, e2) }
| op=unop e=expression { Unop(op, e) }
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

%inline unop:
| NOT { Not }
| MINUS { Opp }
;
