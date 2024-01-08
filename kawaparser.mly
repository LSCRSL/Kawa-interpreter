%{

  open Lexing
  open Kawa
  open Helper

%}

%token <int> INT
%token <string> IDENT
%token <string> CLASS_NAME
%token MAIN
%token LPAR RPAR BEGIN END SEMI COMMA RBRACKET LBRACKET 
%token NEWARR (*allocation d'un tableau*)
%token PRINT IF ELSE WHILE RETURN SET
%token VAR
%token CLASS METHOD EXTENDS ATTRIBUTE NEW THIS FINAL INSTANCE_OF PRIVATE PROTECTED SUPER ABSTRACT (*classe*)
%token TINT TBOOL TVOID
%token DOT
%token EOF
%token TRUE FALSE
%token PLUS STAR MINUS DIV MODULO (*arithmetique*)
%token LT LTE GT GTE ISEQUAL NOTEQUAL
%token EQ_STRUCT NEQ_STRUCT
%token AND OR
%token NOT

(* Opérateurs par ordre croissant de priorités*)
%left OR
%left AND
%left NOTEQUAL EQ_STRUCT NEQ_STRUCT
%left LT LTE GT GTE ISEQUAL INSTANCE_OF
%left PLUS MINUS
%left STAR DIV MODULO
%right NOT
%right RPAR
%left LBRACKET (* LBRACKET ???*)
%left DOT  (*priorité la plus élevée pour accéder à un attribut*)

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
| VAR t=type_decl names=separated_nonempty_list(COMMA,IDENT) SEMI { 
      let tuples_list = List.map (fun name -> (name,t)) names 
      in (tuples_list, []) 
  }
| VAR t=type_decl names=separated_nonempty_list(COMMA,IDENT) SET values=separated_nonempty_list(COMMA,expression) SEMI { 
      map_ident_val_variables t names values [] []
 }
 ;

type_decl:
| TINT { TInt }
| TBOOL { TBool }
| TVOID { TVoid }
| x=CLASS_NAME { TClass x }
| t=type_decl LBRACKET RBRACKET { TArr (t) }
;

class_def:
| ABSTRACT CLASS cls=CLASS_NAME BEGIN attr=list(attribute_declaration) mthd=list(method_def) END { 
    let decl_list, set_list = aux attr [] [] in    
    let attr_typ_list, final_attr_names, private_attr_names, protected_attr_names = separate_attributes decl_list [] [] [] [] in
    {
      class_name=cls; 
      init_instr=set_list; 
      attributes=attr_typ_list; 
      attributes_final=final_attr_names;
      attributes_private=private_attr_names; 
      attributes_protected=protected_attr_names;
      methods=mthd; 
      parent=None;
      is_abstract=true;
    }
  }
| ABSTRACT CLASS cls=CLASS_NAME EXTENDS parent_name=CLASS_NAME BEGIN attr=list(attribute_declaration) mthd=list(method_def) END { 
    let decl_list, set_list = aux attr [] [] in    
    let attr_typ_list, final_attr_names, private_attr_names, protected_attr_names = separate_attributes decl_list [] [] [] [] in
    {
      class_name=cls; 
      init_instr=set_list; 
      attributes=attr_typ_list; 
      attributes_final=final_attr_names;
      attributes_private=private_attr_names; 
      attributes_protected=protected_attr_names;
      methods=mthd; 
      parent=Some parent_name;
      is_abstract=true;
    }
  }
| CLASS cls=CLASS_NAME BEGIN attr=list(attribute_declaration) mthd=list(method_def) END { 
    let decl_list, set_list = aux attr [] [] in    
    let attr_typ_list, final_attr_names, private_attr_names, protected_attr_names = separate_attributes decl_list [] [] [] [] in
    {
      class_name=cls; 
      init_instr=set_list; 
      attributes=attr_typ_list; 
      attributes_final=final_attr_names;
      attributes_private=private_attr_names; 
      attributes_protected=protected_attr_names;
      methods=mthd; 
      parent=None;
      is_abstract=false;
    }
  }
| CLASS cls=CLASS_NAME EXTENDS parent_name=CLASS_NAME BEGIN attr=list(attribute_declaration) mthd=list(method_def) END {
    let decl_list, set_list = aux attr [] [] in    
    let attr_typ_list, final_attr_names, private_attr_names, protected_attr_names = separate_attributes decl_list [] [] [] [] in
    {
      class_name=cls; 
      init_instr=set_list; 
      attributes=attr_typ_list; 
      attributes_final=final_attr_names; 
      attributes_private=private_attr_names; 
      attributes_protected=protected_attr_names;
      methods=mthd; 
      parent=Some parent_name;
      is_abstract=false;
    }
  }
;

attribute_declaration:
| ATTRIBUTE FINAL t= type_decl attrs=separated_nonempty_list(COMMA,IDENT) SEMI {  let tuples_list = List.map (fun name -> (name, t, true, Public)) attrs in (tuples_list, []) }
| ATTRIBUTE PRIVATE t= type_decl attrs=separated_nonempty_list(COMMA,IDENT) SEMI {  let tuples_list = List.map (fun name -> (name, t, false, Private)) attrs in (tuples_list, []) }
| ATTRIBUTE PROTECTED t= type_decl attrs=separated_nonempty_list(COMMA,IDENT) SEMI {  let tuples_list = List.map (fun name -> (name, t, false, Protected)) attrs in (tuples_list, []) }
| ATTRIBUTE t=type_decl attrs=separated_nonempty_list(COMMA,IDENT) SEMI {  let tuples_list = List.map (fun name -> (name, t, false, Public)) attrs in (tuples_list, []) }
| ATTRIBUTE t=type_decl names=separated_nonempty_list(COMMA,IDENT) SET values=separated_nonempty_list(COMMA,expression) SEMI { 
    map_ident_val_attributes t false Public names values [] []
 }
| ATTRIBUTE FINAL t=type_decl names=separated_nonempty_list(COMMA,IDENT) SET values=separated_nonempty_list(COMMA,expression) SEMI { 
    map_ident_val_attributes t true Public names values [] []
 }
;

method_def:
| METHOD t=type_decl name=IDENT LPAR param=separated_list(COMMA,arg) RPAR BEGIN var=list(var_decl) instr=list(instruction) END {
      let acc_tuple = aux var [] [] in
      { 
        method_name=name; 
        code=(snd acc_tuple)@instr; 
        params=param; 
        locals=fst acc_tuple; 
        return=t;
        visib=Public;
        is_abstract=false;
      }
  }

| METHOD PRIVATE t=type_decl name=IDENT LPAR param=separated_list(COMMA,arg) RPAR BEGIN var=list(var_decl) instr=list(instruction) END {
      let acc_tuple = aux var [] [] in
      { 
        method_name=name; 
        code=(snd acc_tuple)@instr; 
        params=param; 
        locals=fst acc_tuple; 
        return=t;
        visib=Private;
        is_abstract=false;
      }
  }

| METHOD PROTECTED t=type_decl name=IDENT LPAR param=separated_list(COMMA,arg) RPAR BEGIN var=list(var_decl) instr=list(instruction) END {
      let acc_tuple = aux var [] [] in
      { 
        method_name=name; 
        code=(snd acc_tuple)@instr; 
        params=param; 
        locals=fst acc_tuple; 
        return=t;
        visib=Protected;
        is_abstract=false;
      }
  }
| METHOD ABSTRACT t=type_decl name=IDENT LPAR param=separated_list(COMMA,arg) RPAR BEGIN END {
      { 
        method_name=name; 
        code=[]; 
        params=param; 
        locals=[]; 
        return=t;
        visib=Public;
        is_abstract=true;
      }
  }
  | METHOD ABSTRACT PROTECTED t=type_decl name=IDENT LPAR param=separated_list(COMMA,arg) RPAR BEGIN END {
      { 
        method_name=name; 
        code=[]; 
        params=param; 
        locals=[]; 
        return=t;
        visib=Protected;
        is_abstract=true;
      }
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
| RETURN e=expression SEMI { Return e }
(*affectation*)
| x=memory_access SET e=expression SEMI {Set(x, e)}
| missing_semi { raise Missing_semi }
;

missing_semi:
| PRINT LPAR e=expression RPAR follow {}
| expression follow {}
| RETURN expression follow {}
| memory_access SET expression follow {}
;

follow:
| PRINT {}
| IF {}
| WHILE {}
| RETURN {}
| IDENT {}
| NEW {}
| SUPER {}
| END {}
| THIS {}
| NEWARR {}
;

memory_access:
| x=IDENT {Var x}
| e=expression DOT attr=IDENT { Field(e, attr) }
| e1=expression LBRACKET e2=expression RBRACKET { ArrElem(e1, e2) }
;

expression:
| n=INT { Int n }
| TRUE  { Bool true }
| FALSE { Bool false }
| LPAR e=expression RPAR {e}
| v=memory_access {Get(v)}
| e=expression INSTANCE_OF t=type_decl {Instance_of(e,t)}
(*| CAST LPAR e=expression COMMA t=type_decl RPAR  {Transtyp(e, t)}*)
| LPAR t=type_decl RPAR e=expression {Transtyp(e, t)}
(*operations*)
| e1=expression op=binop e2=expression { Binop(op, e1, e2) }
| op=unop e=expression { Unop(op, e) }
(*classes*)
| NEW x=CLASS_NAME {New x}
| NEW x=CLASS_NAME LPAR params=separated_list(COMMA, expression) RPAR {NewCstr(x,params) } 
| NEWARR t=type_decl LBRACKET e=expression RBRACKET { ArrayNull(t, e) }
(*method*)
| e=expression DOT name=IDENT LPAR param=separated_list(COMMA,expression) RPAR {MethCall(e,name,param)}
| SUPER DOT name=IDENT LPAR param=separated_list(COMMA,expression) RPAR { Super(name, param) }
(*this pour param implicite dans la classe*)
| THIS { This }
| LBRACKET elems=separated_list(COMMA, expression) RBRACKET {Array (Array.of_list elems)}
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
| GTE { Ge } 
| ISEQUAL { Eq }
| NOTEQUAL { Neq }
| AND { And }
| OR { Or }
| EQ_STRUCT { Eq_struct }
| NEQ_STRUCT { Neq_struct }
;

%inline unop:
| NOT { Not }
| MINUS { Opp }
;
