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
  let tenv_ = add_env p.globals Env.empty in
  

  let rec find_cls_def class_name =
    let get_first_element class_def_filter = 
      match class_def_filter with 
        | [] ->  failwith "the class is not implemented"
        | e::[] -> e
        | _ -> failwith "multiple classes have the same name"
    in
    let class_list = p.classes in 
    let class_def_filter = List.filter (fun cls_def -> (cls_def.class_name = class_name)) class_list in
    get_first_element class_def_filter 
  
  and find_mthd_def class_name mthd_name =
    let class_def = find_cls_def class_name in
    let method_list = List.filter (fun method_def -> (method_def.method_name = mthd_name)) class_def.methods in

    let get_mthd mthd_list = 
      match mthd_list with 
      | [] -> (match class_def.parent with 
              | None -> failwith "the method is not implemented"
              | Some parent_cls_name -> find_mthd_def parent_cls_name mthd_name)
      | e::[] -> e
      | _ -> failwith "surcharge non implémentée"
    in 
    get_mthd method_list

  and check e typ tenv =
    let rec explore_parents node cls target = 
      if cls = target then ()
      else let class_def = find_cls_def cls in 
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
    let tenv_attr = add_env cls_def.attributes tenv in
    (*boucler sur les methodes, rajouter les attibuts à l'env + appel check_mcall*)
    List.iter (fun method_def -> check_mdef method_def (add_env method_def.params tenv_attr)) cls_def.methods

  and check_mdef method_def tenv = 
    (*rajouter les variables locales*)
    let tenv_locals = add_env method_def.locals tenv in 
    (*appeler checkseq sur le code de la fonction*)
    if method_def.method_name == "constructor" then 
      check_seq method_def.code method_def.return tenv_locals false
    else 
      check_seq method_def.code method_def.return tenv_locals true
  
  
  and type_mthcall (class_name : string) (mthd_name : string) (params : expr list) tenv : typ =
    (*recherche de la methode dans parent*)
    let mthd = find_mthd_def class_name mthd_name in
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
      | Get mem -> type_mem_access mem tenv
    (* Objet courant *)
    | This -> Env.find "this" tenv (*comment mettre à jour tenv pour qu'il contienne this*)
    | New class_name -> TClass class_name
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

            
  and type_mem_access (m : mem_access) tenv = match m with
    | Var x ->
          (*let () = Env.iter (fun key value -> Printf.printf "%s : " key) tenv in 
          Printf.printf "\n";*)
          (*try Env.find x tenv with Not_found -> failwith "undelcared variables"*)
          (try  Env.find x tenv with
            | t -> (t,false) (*t n'est pas du bon type*)
            | Not_found -> failwith "undeclared variabe")
          
    | Field (obj, x) -> 
      let class_name = (match type_expr obj tenv with
      | TClass name -> name
      | _ -> failwith "not a class")
      in
      let rec look_for_attribute_and_parents class_name = 
        let class_def = find_cls_def class_name in
        (*verifier qu'il y a un attribut x*)
        let rec find_attribute attr_list is_final = 
            match attr_list with 
            | [] -> (match class_def.parent with 
                    | None -> raise Not_found
                    | Some parent_class_name -> look_for_attribute_and_parents parent_class_name)
            | (attr_name, attr_type)::suite -> if attr_name = x then (attr_type, is_final) else find_attribute suite is_final
        (*in try find_attribute class_def.attributes false with 
          | Not_found -> find_attribute class_def.attributes_final true
          | _ -> failwith "ce cas ne devrait pas être atteignable"*)
        in try find_attribute class_def.attributes false with 
        | Not_found -> (try find_attribute class_def.attributes_final true with
                      | Not_found -> failwith "la variable n'existe pas"
                      | _ -> failwith "ce cas ne devrait pas être atteignable")
        | _ -> failwith "ce cas ne devrait pas être atteignable"
      in look_for_attribute_and_parents class_name (* car type_mem_access est de type : typ on veut qu'il soit de type (typ,bool)*)

  and check_instr i ret tenv (check_final:bool) = match i with
    | Print e -> (match type_expr e tenv with
                  | TInt | TBool -> ()
                  | _ -> failwith "à compléter")
    | Set (x, e) -> if check_final then 
                          (*pas le droit de modifier un attribut final*)
                          let t = type_mem_access x tenv in check e t tenv
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
  check_seq p.main TVoid tenv_
