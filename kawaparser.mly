%{

  open Lexing
  open Kawa
  open Helper

%}

%token <int> INT
%token <string> IDENT
%token MAIN
%token LPAR RPAR BEGIN END SEMI COMMA
%token PRINT IF ELSE WHILE RETURN SET
%token VAR
%token CLASS METHOD EXTENDS ATTRIBUTE NEW THIS FINAL (*classe*)
%token TINT TBOOL TVOID
%token DOT
%token EOF
%token TRUE FALSE
%token PLUS STAR MINUS DIV MODULO (*arithmetique*)
%token LT LTE GT GTE ISEQUAL NOTEQUAL
%token EQ_STRUCT NEQ_STRUCT
%token AND OR
%token NOT

(* Priorités par ordre croissante de priorités*)
%left OR
%left AND
%right NOT
%left LT LTE GT GTE ISEQUAL NOTEQUAL EQ_STRUCT NEQ_STRUCT
%left PLUS MINUS
%left STAR DIV MODULO
%left DOT  (*priorité la plus élevée pour accéder à un attribut ?*)

%start program
%type <Kawa.program> program

%%

program:
| global_var=list(var_decl) cls=list(class_def) MAIN BEGIN main_code=list(instruction) END EOF
    { (*concaténer les listes de déclarations*)
      let acc_tuple = aux global_var [] [] in
      {classes=cls; globals=(fst acc_tuple); main=(snd acc_tuple) @ main_code} }
;

var_decl:
| VAR t=type_decl names=separated_list(COMMA,IDENT) SEMI { 
      let tuples_list = List.map (fun name -> (name,t)) names 
      in (tuples_list, []) 
  }
| VAR t=type_decl names=separated_list(COMMA,IDENT) SET values=separated_list(COMMA,expression) SEMI { 
      map_ident_val_variables t names values [] []
 }
;

type_decl:
| TINT { TInt }
| TBOOL { TBool }
| TVOID { TVoid }
| x=IDENT { TClass x }
;

(*rajouter les methodes*)
class_def:
| CLASS cls=IDENT BEGIN attr=list(attribute_declaration) mthd=list(method_def) END { 
    let decl_list, set_list = aux attr [] [] in    
    let attr_typ_list, final_attr_names = get_final_attr decl_list [] [] in
    {class_name=cls; init_instr=set_list; attributes=attr_typ_list; attributes_final=final_attr_names; methods=mthd; parent=None}
  }
| CLASS cls=IDENT EXTENDS parent_name=IDENT BEGIN attr=list(attribute_declaration) mthd=list(method_def) END {
    let decl_list, set_list = aux attr [] [] in    
    let attr_typ_list, final_attr_names = get_final_attr decl_list [] [] in
    {class_name=cls; init_instr=set_list; attributes=attr_typ_list; attributes_final=final_attr_names; methods=mthd; parent=Some parent_name}
  }
;

attribute_declaration:
| ATTRIBUTE FINAL t= type_decl attrs=separated_list(COMMA,IDENT) SEMI {  let tuples_list = List.map (fun name -> (name,t,true)) attrs in (tuples_list, []) }
| ATTRIBUTE t=type_decl attrs=separated_list(COMMA,IDENT) SEMI {  let tuples_list = List.map (fun name -> (name,t,false)) attrs in (tuples_list, []) }
| ATTRIBUTE t=type_decl names=separated_list(COMMA,IDENT) SET values=separated_list(COMMA,expression) SEMI { 
    map_ident_val_attributes t false names values [] []
 }
| ATTRIBUTE FINAL t=type_decl names=separated_list(COMMA,IDENT) SET values=separated_list(COMMA,expression) SEMI { 
    map_ident_val_attributes t true names values [] []
 }
 
;

method_def:
| METHOD t=type_decl name=IDENT LPAR param=separated_list(COMMA,arg) RPAR BEGIN var=list(var_decl) instr=list(instruction) END {
      let acc_tuple = aux var [] [] in
      {method_name=name; code=(snd acc_tuple)@instr; params=param; locals=fst acc_tuple; return=t}
  }
;

arg:
| t=type_decl name=IDENT {(name,t)}
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
(*method*)
| e=expression DOT name=IDENT LPAR param=separated_list(COMMA,expression) RPAR {MethCall(e,name,param)}
(*this pour param implicite dans la classe*)
| THIS { This }
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
| EQ_STRUCT { Eq_struct }
| NEQ_STRUCT { Neq_struct }
;

%inline unop:
| NOT { Not }
| MINUS { Opp }

;
