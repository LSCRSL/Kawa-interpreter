open Kawa

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)
type tenv = typ Env.t

let add_env l tenv =
  List.fold_left (fun env (x, t) -> Env.add x t env) tenv l

let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in

  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error typ_e typ
  
  
  and type_mthcall class_name mthd_name params =
    (*chercher dans programme la définition de classe correspondante*)
    let get_first_element class_def_filter = 
      match class_def_filter with 
        | [] ->  failwith "the class is not implemented"
        | e::[] -> e
        | _ -> failwith "multiple classes have the same name"
    in 
    (*liste des classes dans le programme*)
    let class_list = p.classes in 
    (*recuperer la classe de objet*)
    let class_def_filter =  (*retourne une liste*)
        List.filter (fun cls_def -> (cls_def.class_name = class_name)) class_list in
    (*premier element de la liste*)
    let class_def = get_first_element class_def_filter in
    (*a modifier si on veut autoriser la surcharge*)
    let method_list = List.filter (fun method_def -> (method_def.method_name = mthd_name)) class_def.methods in
    let mthd = get_first_element method_list in
    let args = mthd.params in 
    let rec verif_type_params args params = 
      match args,params with
      |(name, t)::l1, e::l2 -> check e t tenv; verif_type_params l1 l2
      | [], [] -> ()
      | [], _::_ | _::_, [] -> failwith "number of parameters is incorrect"
    in 
    let () = verif_type_params args params in
    mthd.return

  and type_expr e tenv = match e with
    | Int _  -> TInt
    | Bool _ -> TBool
    | Unop (op, exp) ->
        (match op with
        | Opp -> check exp TInt tenv; TInt
        | Not -> check exp TBool tenv; TBool)
    | Binop (op, e1, e2) -> (match op with
    | Add | Sub | Mul | Div | Rem -> check e1 TInt tenv; check e2 TInt tenv; TInt
    | Lt  | Le  | Gt | Ge -> check e1 TInt tenv; check e2 TInt tenv; TBool;
    | Eq  | Neq -> let t1 = type_expr e1 tenv in check e2 t1 tenv; TBool
    | And | Or -> check e1 TBool tenv; check e2 TBool tenv; TBool; )
    | Get mem -> type_mem_access mem tenv
    (* Objet courant *)
    | This -> Env.find "this" tenv (*comment mettre à jour tenv pour qu'il contienne this*)
    | New class_name -> TClass class_name
    | NewCstr (class_name, params) -> (*vérifier que les params sont cohérents*)
                                    let t = type_mthcall class_name "constructor" params in
                                    let () = (match t with
                                      | TVoid -> ()
                                      | _ -> failwith "Constructor should have return type void")
                                    in
                                    TClass class_name 
    | MethCall (e, method_name, params) -> let class_type = type_expr e tenv in
            (*recup la nom de la classe*)
            let class_name = (match class_type with
            | TClass x -> x
            | _ -> assert false ) in
            type_mthcall class_name method_name params

            
  and type_mem_access (m : mem_access) tenv = match m with
    | Var x -> (try  Env.find x tenv  with Not_found -> failwith "undeclared variabe")
    | _ -> failwith "case not implemented in type_mem_access"
  in

  let rec check_instr i ret tenv = match i with
    | Print e -> (match type_expr e tenv with
                  | TInt | TBool -> ()
                  | _ -> failwith "à compléter")
    | Set (x, e) -> let t = type_mem_access x tenv in check e t tenv
    | If (e, s1, s2) -> check e TBool tenv; check_seq s1 ret tenv; check_seq s2  ret tenv
    | While(e, s) -> check e TBool tenv; check_seq s ret tenv;
    (* Fin d'une fonction *)
    | Return e -> check e ret tenv
  (* Expression utilisée comme instruction *)
    | Expr e -> check e TVoid tenv
  and check_seq s ret tenv =
    List.iter (fun i -> check_instr i ret tenv) s
  in

  check_seq p.main TVoid tenv
