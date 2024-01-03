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
  let class_level = ref "" in

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
    class_level := cls_def.class_name;
    List.iter (fun instr -> check_instr instr TVoid tenv false) cls_def.init_instr;
    
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
  
  
  and type_mthcall (obj_class_name : string) (mthd_name : string) (params : expr list) tenv : typ =
    (*recherche de la methode dans parent*)
    let mthd = find_mthd_def obj_class_name mthd_name p in
    (*verifier qu'on a le droit d'appeler la méthode*)
    let () = match mthd.visib with
    | Private -> if obj_class_name <> (!class_level) then failwith "the method is private and can't be accessed outside the class"
    | Protected -> if (is_sub_class (!class_level) obj_class_name p) = false then failwith "the method is protected and is not accessible"
    | Public -> () 
    in
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
        | Not -> check exp TBool tenv; TBool
        )
    | Binop (op, e1, e2) -> (match op with
      | Add | Sub | Mul | Div | Rem -> check e1 TInt tenv; check e2 TInt tenv; TInt
      | Lt  | Le  | Gt | Ge -> check e1 TInt tenv; check e2 TInt tenv; TBool;
      | Eq  | Neq | Eq_struct | Neq_struct -> let t1 = type_expr e1 tenv in check e2 t1 tenv; TBool
      | And | Or -> check e1 TBool tenv; check e2 TBool tenv; TBool; )
      | Get mem -> type_mem_access mem tenv false false
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
    | MethCall (e, method_name, params) -> 
            let class_type = type_expr e tenv in
            (*recup la nom de la classe*)
            let class_name = (match class_type with
            | TClass x -> x;
            | _ -> assert false ) in
            (*vérifier si on a le droit d'appeler la méthode*)
            type_mthcall class_name method_name params tenv
    | Instance_of(e, t) -> let type_e = type_expr e tenv in 
            (match type_e with
            | TInt | TBool | TVoid -> failwith "cannot use instanceof operator on primitive types"
            | TClass _ -> (match t with
                            | TInt | TBool | TVoid  -> failwith "cannot use instanceof operator on primitive types"
                            | TClass _ -> TBool))
    | Transtyp(e, t) -> let class_e = (match type_expr e tenv with
                                        | TClass x -> x
                                        | _ -> failwith "cannot use transtype operator on primitive types") in 
                        let class_t = (match t with
                                        | TClass x -> x 
                                        | _ -> failwith "cannot use transtype operator on primitive types") in
                         
                          if (is_sub_class class_e class_t p) || (is_sub_class class_t class_e p) 
                                then t 
                          else 
                                failwith "target classes are in different branches" 

            
  and type_mem_access (m : mem_access) tenv (check_final : bool) (set_op : bool) = match m with
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
      let search_for_attribute_in_class class_def x = 
        (*return type of attribute, and boolean = true if found*)
        let rec search_attr_list list : typ * bool = match list with 
            | [] -> (TVoid, false)
            | (name, t)::suite -> if name = x then (t, true) else search_attr_list suite 
        in search_attr_list class_def.attributes
    
      
      in 

      let rec check_is_final class_name = 
        let class_def = find_cls_def class_name p in 
        if List.exists (fun name -> name = x) class_def.attributes_final then true 
        else 
        match class_def.parent with
        | None -> false 
        | Some parent_class_name -> check_is_final parent_class_name

        in 

      let rec check_is_protected class_name = 
        let class_def = find_cls_def class_name p in 
        if List.exists (fun name -> name = x) class_def.attributes_protected then true 
        else 
        match class_def.parent with
        | None -> false 
        | Some parent_class_name -> check_is_protected parent_class_name

      in

      let check_private is_private attr_name found_class = 
          match obj with
          | This -> (*si l'attribut est privé, il ne doit pas être hérité*)
                    if (is_private && class_name != found_class) then 
                          failwith "attribute is private, cannot be used in a subclass" 
          | _ ->  (*si l'attribut est private alors on n'a pas le droit de l'utiliser si on est à l'extérieur de la classe *)
                    if is_private && (!class_level <> class_name) then 
                        let () = Printf.printf "current class : %s - object class : %s\n" (!class_level) class_name in 
                        failwith "attribute is private, cannot be used outside the class"
      in

      let check_protected is_protected attr_name = (*j'ai besoin de savoir dans quelle classe je suis*)
        match obj with
        | This -> ()  (*this.attr donc je suis à l'intérieur d'une classe et je peux utiliser un attribut hérité*)
        | _ -> let class_level_name = !class_level     (*a.x : si x est protected alors vérifier que la classe courante est une sous-classe de celle de a*)
              in if is_protected && (is_sub_class class_level_name class_name p) = false then failwith "can't access protected attribute"
      in

      let check_final_aux is_final check_final = 
          if set_op then
            match obj with
            | This -> if check_final (*à l'extérieur d'un constructeur*)
                      && is_final then failwith "Can't modify final attribute outside of constructor"
            | _ -> if is_final then  (*je modifie un attribut final d'une classe A dans le constructeur d'une autre classe B*)
                      failwith "Can't modify final attribute outside of constructor"
          
      in 

      let rec explore_parents_for_attr class_name = 
        (*look in current class*)
        let current_class_def = find_cls_def class_name p in
        let t, found = search_for_attribute_in_class current_class_def x in 
        if found then 
            let is_private = List.exists (fun attr_name -> x = attr_name) current_class_def.attributes_private in 
            let is_final = check_is_final current_class_def.class_name in
            let is_protected = check_is_protected current_class_def.class_name in
            check_final_aux is_final check_final;
            check_private is_private x current_class_def.class_name;
            check_protected is_protected x;
            t
        else match current_class_def.parent with
        | None -> failwith "attribute undefined"
        | Some parent_class_name -> explore_parents_for_attr parent_class_name

      in explore_parents_for_attr class_name

  and check_instr i ret tenv (check_final:bool) = match i with
    | Print e -> (match type_expr e tenv with
                  | TInt | TBool -> ()
                  | _ -> failwith "à compléter")
    | Set (x, e) -> let t = type_mem_access x tenv check_final true in check e t tenv 
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
