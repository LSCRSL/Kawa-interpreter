open Format
open Lexing
open Helper

let file = Sys.argv.(1)

let report b e file missing_semi_error =
   let c  = open_in file in
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in

  (* on extrait la ligne où il y a une erreur*)
   let rec boucle_lignes l = 
      if l = 1 then input_line c
      else let _ = input_line c in boucle_lignes (l-1)
   in let line = boucle_lignes l in

   (* on détermine le mot qui comporte l'erreur *)
   let word = String.sub line (fc-1) (lc-fc) in 
   
   (*si les mots clés sont correctement écrits -> il manque un ";"*)
   if word = "var" || word = "attribute" || word = "class"  || word = "abstract" || word = "main" || word = "method" || word = "}" then  
      eprintf "File \"%s\", line %d, characters %d-%d; Missing semicolon ?\n" file l fc lc
   else 
      if missing_semi_error then eprintf "File \"%s\", line %d or %d:\n" file (l-1) l 
      else eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  try
    let prog = Kawaparser.program Kawalexer.token lb in
    
    Typechecker.typecheck_prog prog;
    Interpreter.exec_prog prog;
    exit 0
  with
  | Kawalexer.Error s ->
      close_in c;
     report (lexeme_start_p lb) (lexeme_end_p lb) file false;
     eprintf "lexical error: %s@." s;
     exit 1
  | Missing_semi -> close_in c;
                  report (lexeme_start_p lb) (lexeme_end_p lb) file true;  
                  eprintf "syntax error: missing semicolon@.";
                  exit 1
  | Kawaparser.Error ->
   report (lexeme_start_p lb) (lexeme_end_p lb) file false;
   eprintf "syntax error@.";
     exit 1
  | Interpreter.Error s ->
      close_in c;
     eprintf "interpreter error: %s@." s;
     exit 1
  | Typechecker.Error s ->
      close_in c;
      eprintf "typechecker error: %s@." s;
      exit 1
  | e ->
    close_in c;
     eprintf "Anomaly: %s\n@." (Printexc.to_string e);
     exit 2
