open Kawa

type value =
  | VInt  of int
  | VBool of bool
  | VObj  of obj
  | Null

and obj = {
  cls:    string;
  fields: (string, value) Hashtbl.t;
}

exception Error of string
exception Return of value

let print_value v = match v with
| VInt n -> Printf.printf "%d\n" n
| VBool b -> Printf.printf "%b\n" b
| VObj _ -> Printf.printf "class\n"
| Null -> Printf.printf "null\n"

let print_expr_option e_opt = match e_opt with
| None -> Printf.printf "None" 
| Some _ -> Printf.printf "Some"

let exec_prog (p: program): unit =
  let find_cls_def class_name =
    let get_first_element class_def_filter = 
      match class_def_filter with 
        | [] ->  failwith "the class is not implemented"
        | e::[] -> e
        | _ -> failwith "multiple classes have the same name"
    in
    let class_list = p.classes in 
    let class_def_filter = List.filter (fun cls_def -> (cls_def.class_name = class_name)) class_list in
    get_first_element class_def_filter 
  in
  let rec find_mthd_def class_name mthd_name =
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
  in
  let env = Hashtbl.create 16 in
  let return_exp = ref Null in
  (*rajouter les variables globales dans env*)
  List.iter (fun (x, _) -> Hashtbl.add env x Null) p.globals;
  
  let rec exec_seq s lenv =
    let rec evali e = match eval e with
      | VInt n -> n
      | _ -> assert false
    and evalb e = match eval e with
      | VBool b -> b
      | _ -> assert false
    and evalo e = match eval e with
      | VObj o -> o
      | _ -> assert false

    (*rajouter this à l'environnement local*)
    and eval_call (f: string) (this: obj) (args : expr list) =
        (*let objet = (evalo this) in*)
        let class_name = this.cls in 
        let mthd = find_mthd_def class_name f in
        (*environnement local pour exécuter la fonction*)
        let local_env = Hashtbl.create 16 in
        (*ajouter l'objet courant à l'environnement*)
        let () = Hashtbl.replace local_env "this" (VObj this) in

        let rec param_to_env args params = match args, params with 
          | e1::l1, (name, _)::l2 ->  Hashtbl.replace local_env name (eval e1);
                                        param_to_env l1 l2;
          | [], [] -> ()
          | _,_ -> failwith "the number of parameters is incorrect"
      in 
      let () = param_to_env args mthd.params in
      (*ajouter les variables locales*)
      let rec locals_to_env lcls = match lcls with 
      | (name,typ)::l -> let () = Hashtbl.replace local_env name Null in locals_to_env l;
      | [] -> ()
    in 
    let () = locals_to_env mthd.locals in 
    (*definir une ref pour la valeur de retour*)
    let () = exec_seq mthd.code local_env in 
    !return_exp (*dereference le pointeur*)
    
    and evalvar (m : mem_access) = match m with
      | Var x -> if Hashtbl.mem lenv x then
                    Hashtbl.find lenv x
                 else 
                  if Hashtbl.mem env x then 
                    Hashtbl.find env x
                  else
                    failwith "undefined variable"
                    
      | Field(e, attribute) -> 
              let obj_ = evalo e in 
              Hashtbl.find obj_.fields attribute
              

    and evalnew(x : expr) = 
      (*creer un nouvel objet*)
      
      let init_object class_name = 
        (*chercher dans classes la definition qu'on veut*)
        let class_def = find_cls_def class_name in 
        (*environnement pour les attributs*)
        let attr_env = Hashtbl.create 8 in 
        let update class_def = 
          List.iter (fun (attr_name, attr_type) -> Hashtbl.add attr_env attr_name Null) class_def.attributes in 
        let rec found_mother class_def =
          match class_def.parent with 
          | None -> ()
          | Some parent_class_name -> let mother_def = find_cls_def parent_class_name in 
                                      update mother_def;
                                      found_mother mother_def
        in
        let () = update class_def in
        let () = found_mother class_def in
        class_def,{cls=class_name; fields=attr_env};
      in
      match x with
        | New class_name -> let cls_def,obj = init_object class_name in
                            let _ = eval_call "constructor" obj [] in obj
        | NewCstr (class_name, param) -> let cls_def,obj = init_object class_name in
                                          (*let mthd_def_filter = List.filter (fun mthd_def -> (mthd_def.method_name = "constructor")) cls_def.methods in
                                          let constructor = get_first_element mthd_def_filter in*)
                                          let _ = eval_call "constructor" obj param in obj                         
        | _ -> assert false 

    and eval (e: expr): value = 
      match e with
      | Int n  -> VInt n
      | Bool b -> VBool b

      | Binop(op, e1, e2) -> (match op with
        | Add -> (*let () = print_value (eval e1) in *) VInt (evali e1 + evali e2)
        | Sub -> VInt (evali e1 - evali e2)
        | Mul -> VInt (evali e1 * evali e2)
        | Div -> VInt (evali e1 / evali e2)
        | Rem -> VInt (evali e1 mod evali e2) 
        | Lt  -> VBool (evali e1 < evali e2)
        | Le  -> VBool (evali e1 <= evali e2)
        | Gt -> VBool (evali e1 > evali e2)
        | Ge -> VBool (evali e1 >= evali e2)
        (*on a le droit de comparer tout en caml*)
        | Eq -> let ev1 = eval e1 in let ev2 = eval e2 in let b = (ev1 = ev2) in if b = false then VBool false else 
                  (match ev1, ev2 with
                  | VObj v1, VObj v2 -> VBool (v1.fields == v2.fields) (*comparer physiquement les tables de hachage*)
                  | _, _ -> VBool true
                )

        | Neq -> let vb = eval (Binop(Eq, e1, e2)) in (
              match vb with
              | VBool true -> VBool false
              | VBool false -> VBool true
              | _ -> failwith "error with operator !="
          ) 
        | And -> let v1 = evalb e1 in 
              if v1 = false then VBool false
              else VBool (evalb e2)
        | Or -> let v1 = evalb e1 in 
              if v1 = false then VBool true
              else VBool (evalb e2)
      )

      | Unop(op, e) -> (match op with
        | Not -> let b = evalb e in if b = true then VBool false else VBool true
        | Opp -> VInt (- (evali e))
      )

      | Get mem -> evalvar mem
      | New _ | NewCstr _  -> VObj (evalnew e)
      | MethCall (e,name,param) -> let objet = (evalo e) in
                                  eval_call name objet param  (*e : objet, param: parametre de la fonction*)
      | This -> Hashtbl.find lenv "this"
    in
  
    let rec exec (i: instr): unit = match i with
      | Print e -> 
          let v = eval e in (
            match v with 
            | VInt n -> Printf.printf "%d\n" n
            | VBool b -> Printf.printf "%b\n" b
            | VObj _ -> Printf.printf "objet\n"
            | Null -> Printf.printf "null\n"
          )
      | If(e, s1, s2) -> let v = evalb e in if v = true then exec_seq s1 else exec_seq s2
      | While(e, s) -> let v = evalb e in 
          if v = true then 
          begin 
            exec_seq s;
            exec (While(e, s));
          end 
      | Set(m, e) -> (match m with
          | Var x -> if Hashtbl.mem lenv x then
                        Hashtbl.replace lenv x (eval e)
                    else 
                      if Hashtbl.mem env x then 
                        Hashtbl.replace env x (eval e)
                      else
                        failwith "undefined variable"
          | Field (exp_obj, attribute) -> 
            let obj_ = evalo exp_obj in 

            Hashtbl.replace obj_.fields attribute (eval e);
        )
      | Expr(e) -> let _ = eval e in () (*faire un match : si c'est une méthode appeler eval sinon renvoyer une erreur*)
      | Return(e) -> return_exp := eval e
    and exec_seq s = 
      List.iter exec s
    in
    exec_seq s
  in
  exec_seq p.main (Hashtbl.create 5)
