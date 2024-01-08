open Kawa
open Helper

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

let type_undefined_error t = 
  error (Printf.sprintf "Type %s is undefined" (typ_to_string t))

module Env = Map.Make(String)
type tenv = typ Env.t


let typecheck_prog p =

  let rec type_exists t = 
    match t with
    | TInt | TBool | TVoid | EmptyArr -> true 
    | TClass x -> List.exists (fun class_def -> class_def.class_name = x) p.classes 
    | TArr (type_elem) -> type_exists type_elem 

  in 
  let add_env l tenv =
    List.fold_left (fun env (x, t) -> if type_exists t then Env.add x t env else type_undefined_error t) tenv l
  in 

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
    | TInt | TBool | TVoid | TArr _ -> if typ_e <> typ then type_error typ_e typ
    | TClass class_name -> (match typ with
        | TBool | TInt | TVoid | TArr _ | EmptyArr -> type_error typ_e typ
        | TClass class_target -> explore_parents class_name class_name class_target)
    | EmptyArr -> match typ with
        | EmptyArr | TArr _ -> ()
        | _ -> type_error typ_e typ

  and check_class cls_def tenv = 
    (*ajouter les attributs à l'env*)
    (*let tenv_attr = add_env (List.map (fun (x, t) -> ("this." ^ x, t)) (cls_def.attributes @ cls_def.attributes_final)) tenv in*)
    (*boucler sur les methodes, rajouter les attibuts à l'env + appel check_mcall*)
    class_level := cls_def.class_name;

    let () = if cls_def.is_abstract then   
        begin
            let rec check_parents_abstraction cls_def = 
              match cls_def.parent with
              | None -> ()
              | Some parent_name -> let parent_cls_def = find_cls_def parent_name p in  
                      if parent_cls_def.is_abstract = false then error "An asbtract class cannot inherit from a non-abstract class"
                      else check_parents_abstraction parent_cls_def
            in check_parents_abstraction cls_def;
        end
    else 
        begin
          let seen_methods = ref [] in
          (*verifier qu'aucune méthode définie dans la classe n'est pas abstraite*) 
          let rec check_abstract_inherited cls_def = 
            let () = List.iter (fun (mdef : method_def) -> if mdef.is_abstract 
                                                  && (List.exists (fun mthd_name -> mthd_name = mdef.method_name) !seen_methods) = false 
                                                  then error "A non-abstract class cannot have an abstract method even if inherited"  
                                                  else if (List.exists (fun mthd_name -> mthd_name = mdef.method_name) !seen_methods) = false then 
                                                      seen_methods := mdef.method_name::(!seen_methods)) 
                                cls_def.methods

            in
              match cls_def.parent with
              | None -> ()
              | Some parent_name -> let parent_cls_def = find_cls_def parent_name p in 
              check_abstract_inherited parent_cls_def
         
        in check_abstract_inherited cls_def
        end  
    in 
    List.iter (fun instr -> check_instr instr TVoid tenv false) cls_def.init_instr;
    
    List.iter 
          (fun method_def -> check_mdef method_def (add_env method_def.params tenv)) 
          cls_def.methods

  and check_mdef method_def tenv = 
    let is_return instr = match instr with
    | Return _ -> true 
    | _ -> false 
    in 
    (*s'assurer qu'il y a bien un return sinon lever une exception*)
    let () = match method_def.return with
    | TVoid -> ()
    | _ -> if method_def.method_name = "constructor" then  error "Constructor should have return type void"
           else if List.exists (fun instr -> is_return instr) method_def.code then ()
                else error (Printf.sprintf "Missing return statement")
                
    in 

    (*si la méthode est abstraite, alors la classe doit être abstraite*)
    
    (*vérifier si c'est une redéfinition*)
    let current_class_def = find_cls_def (!class_level) p in 
    let () = match current_class_def.parent with 
    | None -> ()
    | Some parent_name -> let redef = find_mthd_def parent_name method_def.method_name p in 
                          (match redef.visib with
                          | Private -> error (Printf.sprintf "Method %s is private and cannot be overridden in subclasses" method_def.method_name) 
                          | _ -> ())
    in

    (*rajouter les variables locales*)
    let tenv_locals = add_env method_def.locals tenv in 
    (*appeler checkseq sur le code de la fonction*)
    if method_def.method_name = "constructor" then 
      check_seq method_def.code method_def.return tenv_locals false
    else 
      check_seq method_def.code method_def.return tenv_locals true
  
  
  and type_mthcall (obj_class_name : string) 
                   (mthd_name : string) 
                   (params : expr list) 
                   tenv : typ =
    (*recherche de la methode dans parent*)
    let mthd = find_mthd_def obj_class_name mthd_name p in
    (*verifier qu'on a le droit d'appeler la méthode*)
    let () = match mthd.visib with
    | Private -> if obj_class_name <> (!class_level) then  error (Printf.sprintf "The method %s is private and can't be accessed outside the class" mthd_name)
    | Protected -> if (is_sub_class (!class_level) obj_class_name p) = false then error (Printf.sprintf "The method %s is protected and is not accessible" mthd_name)
    | Public -> () 
    in
    let args = mthd.params in 
    let rec verif_type_params args params = 
      match args,params with
      |(name, t)::l1, e::l2 -> check e t tenv; verif_type_params l1 l2
      | [], [] -> ()
      | [], _::_  -> error "Too many arguments in method call"
      | _::_, [] -> error "Missing arguments in method call"
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
    | New class_name -> let cls_def = find_cls_def class_name p in 
                        if cls_def.is_abstract then error (Printf.sprintf "Class %s is abstract" class_name)
                        else TClass class_name
    | NewCstr (class_name, params) -> (*vérifier que les params sont cohérents*)
                                    let cls_def = find_cls_def class_name p in 
                                    if cls_def.is_abstract then error (Printf.sprintf "Class %s is abstract" class_name)
                                    else
                                          let t = type_mthcall class_name "constructor" params tenv in
                                          (*verifier que le type de retour du constructeur est void*)
                                          let () = (match t with
                                            | TVoid -> ()
                                            | _ -> error "Constructor should have return type void")
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
    | Super (method_name, params) -> 
            (*check if direct parent of current class has this method*)
            let current_class_name = !class_level in 
            let current_class_def = find_cls_def current_class_name p in
            (match current_class_def.parent with
            | None -> error (Printf.sprintf "Class %s does not have a parent : cannot use super" current_class_name)
            | Some parent_class_name -> let parent_class_def = find_cls_def parent_class_name p in 
                                        
                                        let methods = List.filter (fun method_def -> method_def.method_name = method_name) parent_class_def.methods in 
                                        let method_exists = (match methods with
                                        | [] -> false
                                        | e::[] -> true 
                                        | _ -> failwith "multiple methods have the same name")
                                            in if method_exists then type_mthcall parent_class_name method_name params tenv
                                            else error (Printf.sprintf "Method %s not found in direct parent of class %s" method_name current_class_name))
    | Instance_of(e, t) -> let type_e = type_expr e tenv in 
            (match type_e with
            | TInt | TBool | TVoid -> error "cannot use instanceof operator on primitive types"
            | TClass _ | TArr _ -> (match t with
                            | TInt | TBool | TVoid -> error "cannot use instanceof operator on primitive types"
                            | TClass _ | TArr _ -> TBool
                            | EmptyArr -> failwith "Instance_of : ce cas ne devrait pas être atteignable")
            | EmptyArr -> failwith "Instance_of : ce cas ne devrait pas être atteignable")
    | Transtyp(e, t) -> let class_e = (match type_expr e tenv with
                                        | TClass x -> x
                                        | _ -> error "cannot use transtype operator on primitive types") in 
                        let class_t = (match t with
                                        | TClass x -> x 
                                        | _ -> error "cannot use transtype operator on primitive types") in
                         
                          if (is_sub_class class_e class_t p) || (is_sub_class class_t class_e p) 
                                then t 
                          else 
                                error (Printf.sprintf "target classes %s and %s are in different branches" class_e class_t) 
    | Array(t) -> if Array.length t = 0 then
                        EmptyArr 
                  else let type_e1 = type_expr t.(0) tenv in 
                      let () = Array.iter (fun e -> check e type_e1 tenv) t in
                      TArr (type_e1)
    | ArrayNull (t, length) -> check length TInt tenv; 
                                TArr (t)
                  
            
  and type_mem_access (m : mem_access) tenv (check_final : bool) (set_op : bool) = match m with
    | Var x ->
          (try Env.find x tenv with
          Not_found -> error (Printf.sprintf "Variable %s is undefined" x))
          
    | Field (obj, x) -> 
      (*get class name of obj*)
      let class_name = (match type_expr obj tenv with
                        | TClass name -> name
                        | _ -> error "Cannot use dot operator on primtive types or arrays"
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
                          error (Printf.sprintf "The attribute %s is private, cannot be used in a subclass" x)
          | _ ->  (*si l'attribut est private alors on n'a pas le droit de l'utiliser si on est à l'extérieur de la classe *)
                    if is_private && (!class_level <> class_name) then 
                        (*let () = Printf.printf "current class : %s - object class : %s\n" (!class_level) class_name in *)
                        error (Printf.sprintf "The attribute %s is private, cannot be used outside the class" x)
      in

      let check_protected is_protected attr_name = (*j'ai besoin de savoir dans quelle classe je suis*)
        match obj with
        | This -> ()  (*this.attr donc je suis à l'intérieur d'une classe et je peux utiliser un attribut hérité*)
        | _ -> let class_level_name = !class_level     (*a.x : si x est protected alors vérifier que la classe courante est une sous-classe de celle de a*)
              in if is_protected && (is_sub_class class_level_name class_name p) = false then error  (Printf.sprintf "The attribute %s is protected and therefore not accessible" x)
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
        | None -> error (Printf.sprintf  "The attribute %s undefined" x)
        | Some parent_class_name -> explore_parents_for_attr parent_class_name

      in explore_parents_for_attr class_name
    
    | ArrElem(tab, index) -> check index TInt tenv; let t = type_expr tab tenv in  
                              (match t with
                              | TArr (type_elem) -> type_elem
                              | _ -> error "Cannot use [] operator on non-array expressions")

  and check_instr i ret tenv (check_final:bool) = match i with
    | Print e -> ()
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
