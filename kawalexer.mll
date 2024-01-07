{

  open Lexing
  open Kawaparser

  exception Error of string

  let keyword_or_ident =
  let h = Hashtbl.create 17 in
  List.iter (fun (s, k) -> Hashtbl.add h s k)
    [ "print",    PRINT;
      "main",     MAIN;
      "void",     TVOID;
      "bool",     TBOOL;
      "true",     TRUE;
      "false",    FALSE;
      "int",      TINT;
      "var",      VAR;
      "if",       IF;
      "else",     ELSE;
      "while",    WHILE; 
      "return",   RETURN;
      "class",    CLASS;
      "method",   METHOD;
      "extends",  EXTENDS;
      "attribute", ATTRIBUTE;
      "new",       NEW;
      "this",      THIS;
      "final",     FINAL;
      "instanceof", INSTANCE_OF;
      "private", PRIVATE;
      "protected", PROTECTED;
      "super", SUPER;
    ] ;

  fun s ->
    try  Hashtbl.find h s
    with Not_found -> IDENT(s)
        
}

let digit = ['0'-'9']
(*
let number = ['-']? digit+
*)
let number = digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
let class_name = ['A'-'Z'] (alpha | '_' | digit)*
  
rule token = parse
  | ['\n']            { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  | "//" [^ '\n']* "\n"  { new_line lexbuf; token lexbuf }
  | "/*"                 { comment lexbuf; token lexbuf }

  | number as n  { INT(int_of_string n) }
  | ident as id  { keyword_or_ident id }
  | class_name as cn { CLASS_NAME(cn) }

  | ";"  { SEMI }
  | ","  { COMMA }
  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { BEGIN }
  | "}"  { END }
  | "["  { LBRACKET }
  | "]"  { RBRACKET }
  (*arithmetique*)
  | "+"  { PLUS }
  | "*"  { STAR }
  | "-"  { MINUS }
  | "/"  { DIV }
  | "%"  { MODULO }
  (*opérateurs booléens*)
  | "<"  { LT }
  | "<=" { LTE }
  | ">"  { GT }
  | ">=" { GTE }
  | "==" { ISEQUAL }
  | "!=" { NOTEQUAL }
  | "&&" { AND }
  | "||" { OR }
  | "!"  { NOT }
  (*affectation*)
  | "="  { SET }
  (*acces a un attribut*)
  | "." { DOT }
  | "===" {EQ_STRUCT}
  | "=/=" {NEQ_STRUCT}

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { EOF }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
