open Kawa
open Helper

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
  let tenv_ = add_env p.globals Env.empty in
  

  let rec check e typ tenv =
    let rec explore_parents node cls target = 
      if cls = target then ()
      else let class_def = find_cls_def cls p in 
      match class_def.parent with
      | Some parent -> explore_parents node parent target 
      | None -> type_error (TClass node) (TClass target) 
    in 

    let typ_e = type_expr e tenv in
    match typ_e with
    | TInt | TBool | TVoid -> if typ_e <> typ then type_error typ_e typ
    | TClass class_name -> (match typ with
        | TBool | TInt | TVoid -> type_error typ_e typ
        | TClass class_target -> explore_parents class_name class_name class_target)

  and check_class cls_def tenv = 
    (*ajouter les attributs à l'env*)
    (*let tenv_attr = add_env (List.map (fun (x, t) -> ("this." ^ x, t)) (cls_def.attributes @ cls_def.attributes_final)) tenv in*)
    (*boucler sur les methodes, rajouter les attibuts à l'env + appel check_mcall*)
    List.iter 
          (fun method_def -> check_mdef method_def (add_env method_def.params tenv)) 
          cls_def.methods

  and check_mdef method_def tenv = 
    (*TODO : s'assurer qu'il y a bien un return sinon lever une exception*)


    (*rajouter les variables locales*)
    let tenv_locals = add_env method_def.locals tenv in 
    (*appeler checkseq sur le code de la fonction*)
    if method_def.method_name = "constructor" then 
      check_seq method_def.code method_def.return tenv_locals false
    else 
      check_seq method_def.code method_def.return tenv_locals true
  
  
  and type_mthcall (class_name : string) (mthd_name : string) (params : expr list) tenv : typ =
    (*recherche de la methode dans parent*)
    let mthd = find_mthd_def class_name mthd_name p in
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
      | Eq  | Neq | Eq_struct | Neq_struct -> let t1 = type_expr e1 tenv in check e2 t1 tenv; TBool
      | And | Or -> check e1 TBool tenv; check e2 TBool tenv; TBool; )
      | Get mem -> type_mem_access mem tenv false
    (* Objet courant *)
    | This -> Env.find "this" tenv (*comment mettre à jour tenv pour qu'il contienne this*)
    | New class_name -> let _ = find_cls_def class_name p in TClass class_name
    | NewCstr (class_name, params) -> (*vérifier que les params sont cohérents*)
                                    let t = type_mthcall class_name "constructor" params tenv in
                                    (*verifier que le type de retour du constructeur est void*)
                                    let () = (match t with
                                      | TVoid -> ()
                                      | _ -> failwith "Constructor should have return type void")
                                    in
                                    TClass class_name 
    | MethCall (e, method_name, params) -> let class_type = type_expr e tenv in
            (*recup la nom de la classe*)
            let class_name = (match class_type with
            | TClass x -> x;
            | _ -> assert false ) in
            (*ajouter les attributs à l'environnement*)
            type_mthcall class_name method_name params tenv

            
  and type_mem_access (m : mem_access) tenv (check_final : bool) = match m with
    | Var x ->
          (try Env.find x tenv with
          Not_found -> failwith "undeclared variabe")
          
    | Field (obj, x) -> 
      (*get class name of obj*)
      let class_name = (match type_expr obj tenv with
                        | TClass name -> name
                        | _ -> failwith "not a class"
                      )
      
      in 
      
      (*check if attribute is defined in a class*)
      (*return : 
         - bool : found attribute
         - type : TVoid if not found
         - bool : true if found in final attributes list 
      *)
      let search_for_attribute_in_class class_def = 
        (*return type of attribute, and boolean = true if found*)
        let rec search_attr_list list : typ * bool = match list with 
            | [] -> (TVoid, false)
            | (name, t)::suite -> if name = x then (t, true) else search_attr_list suite 
        in 
        let rec is_final class_name = 
          let class_def = find_cls_def class_name p in 
          if List.exists (fun name -> name = x) class_def.attributes_final then true 
          else 
          match class_def.parent with
          | None -> false 
          | Some parent_class_name -> is_final parent_class_name
        in let t, found = search_attr_list class_def.attributes
        in if found then (true, t, is_final class_name) 
           else (false, TVoid, false)
      
      in 
      (*search for attribute in parents*)
      let rec explore_parents class_name = 
        (*look in current class*)
        let class_def = find_cls_def class_name p in
        let found, t, is_final = search_for_attribute_in_class class_def in 
        if found then 
                                (*attribute should not be final*)
            if check_final then if is_final then failwith "Can't modify final attribute outside of constructor"
                                else t 
            else t 
        else match class_def.parent with
        | None -> failwith "attribute undefined"
        | Some parent_class_name -> explore_parents parent_class_name

      in explore_parents class_name

  and check_instr i ret tenv (check_final:bool) = match i with
    | Print e -> (match type_expr e tenv with
                  | TInt | TBool -> ()
                  | _ -> failwith "à compléter")
    | Set (x, e) -> let t = type_mem_access x tenv check_final in check e t tenv 
    | If (e, s1, s2) -> check e TBool tenv; check_seq s1 ret tenv check_final; check_seq s2 ret tenv check_final
    | While(e, s) -> check e TBool tenv; check_seq s ret tenv check_final;
    (* Fin d'une fonction *)
    | Return e -> check e ret tenv
    (* Expression utilisée comme instruction *)
    | Expr e -> check e TVoid tenv

  and check_seq s ret tenv (check_final : bool) =
    List.iter (fun i -> check_instr i ret tenv check_final) s

  in

  List.iter (fun cls_def -> check_class cls_def (Env.add "this" (TClass cls_def.class_name) tenv_)) p.classes;
  check_seq p.main TVoid tenv_ true
