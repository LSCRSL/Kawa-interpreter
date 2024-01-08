open Kawa
open Helper

type value =
  | VInt  of int
  | VBool of bool
  | VObj  of obj
  | VArr of value array
  | Null

and obj = {
  cls:    string;
  fields: (string, value) Hashtbl.t;
}

exception Error of string
exception Return of value

let error s = raise (Error s)

let print_value v = 
  let rec aux v = match v with
| VInt n -> Printf.printf "%d" n
| VBool b -> Printf.printf "%b" b
| VObj _ -> Printf.printf "class"
| VArr l -> Printf.printf "[ "; Array.iter (fun v -> aux v; Printf.printf "; ") l; Printf.printf " ]"
| Null -> Printf.printf "null"
  in aux v; Printf.printf "\n" 

let print_expr_option e_opt = match e_opt with
| None -> Printf.printf "None" 
| Some _ -> Printf.printf "Some"

let exec_prog (p: program): unit =
  let env = Hashtbl.create 16 in (* notre environnement global*)
  let return_exp = ref Null in (*référence représentant la valeur de retour initialisée à Null*)
  (*rajouter les variables globales dans env*)
  List.iter (fun (x, _) -> Hashtbl.add env x Null) p.globals;
  (*modifier les classes pour rajouter les final attributes dans final*)
  
  
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
    and evalarr e = match eval e with
    | VArr a  -> a 
    | _ -> assert false

    (*evalue un appel de méthode*)
    and eval_call (f: string) (this: obj) (args : expr list) (super : bool) =
        let class_name = this.cls in 
        let mthd = if super then 
          begin
            let class_def = find_cls_def class_name p in 
            (match class_def.parent with
            | Some parent_name -> let parent_class_def = find_cls_def parent_name p in 
                                  let methods = List.filter (fun method_def -> method_def.method_name = f) parent_class_def.methods in 
                                  (match methods with
                                  | [] -> failwith "eval_call : cas non atteignable"
                                  | e::[] -> e 
                                  | _ -> error (Printf.sprintf "There are more than one definition for the following method : %s" f))
            | None -> failwith "eval_call : cas non atteignable")
            end
        else 
            find_mthd_def class_name f p in

        let local_env = Hashtbl.create 16 in (*environnement local pour exécuter la fonction*)

        (*ajouter l'objet courant à l'environnement local*)
        let () = Hashtbl.replace local_env "this" (VObj this) in

        (*ajoute les paramètres/arguments de la méthode à l'environnement local*)
        let rec param_to_env args params = match args, params with 
          | e1::l1, (name, _)::l2 ->  Hashtbl.replace local_env name (eval e1);
                                        param_to_env l1 l2;
          | [], [] -> ()
          | _,_ -> failwith "eval_call : cas non atteignable - the number of parameters is incorrect - voir typechecker"
      in 
      let () = param_to_env args mthd.params in
      (*ajouter les variables locales*)
      let rec locals_to_env lcls = match lcls with 
      | (name,typ)::l -> let () = Hashtbl.replace local_env name Null in locals_to_env l;
      | [] -> ()
    in 
    let () = locals_to_env mthd.locals in 
    let () = exec_seq mthd.code local_env in 
    !return_exp (*dereference le pointeur*)
    
    (*evalue un accès mémoire*)
    and evalvar (m : mem_access) = match m with
      | Var x -> if Hashtbl.mem lenv x then
                    Hashtbl.find lenv x
                 else 
                  if Hashtbl.mem env x then 
                    Hashtbl.find env x
                  else
                    error (Printf.sprintf "Variable %s is undefined" x)
                    
      | Field(e, attribute) -> 
              let obj_ = evalo e in 
              Hashtbl.find obj_.fields attribute
      
      (*permet la lecture d'un élément*)
      | ArrElem(t, index) -> let arr = evalarr t in
                             let ind = evali index in 
                             let length = Array.length arr in 
                             if length <= ind then error (Printf.sprintf "List index out of range")
                             else Array.get arr ind 
              
    (*creer un nouvel objet*)
    and evalnew(x : expr) = 
      
      (*renvoie la définition de la classe et un record composé de son nom et de son env d'attributs*)
      let init_object class_name = 
        (*chercher dans classes la definition qu'on veut*)
        let class_def = find_cls_def class_name p in 
        (*environnement pour les attributs*)
        let attr_env = Hashtbl.create 8 in 
        let update class_def = 
          List.iter (fun (attr_name, attr_type) -> Hashtbl.add attr_env attr_name Null) class_def.attributes in
        (* permet d'ajouter les attributs des classes parentes à l'environnement*) 
        let rec found_mother class_def =
          match class_def.parent with 
          | None -> ()
          | Some parent_class_name -> let mother_def = find_cls_def parent_class_name p in 
                                      update mother_def;
                                      found_mother mother_def
        in
        let () = update class_def in
        let () = found_mother class_def in
        class_def,{cls=class_name; fields=attr_env};
      in
      (*on effectue les déclarations avec valeur initiale*)
      let rec exec_init init_instr_list obj = 
        match init_instr_list with
        | [] -> ()
        | Set(Field(This, x), e)::suite -> 
              let () = Hashtbl.replace obj.fields x (eval e) 
              in exec_init suite obj
        | _ -> failwith "eval_new : cas non atteignable - que des sets dans init_instr_list"
      in 
      (*déclarations avec valeur initiale de la classe courante et des classes parentes*)
      let rec init_attributes_current_and_parents class_name obj = 
          let cls_def = find_cls_def class_name p in  
          let () = exec_init cls_def.init_instr obj in 
          match cls_def.parent with
          | None -> ()
          | Some parent_class_name -> init_attributes_current_and_parents parent_class_name obj
    in 
      match x with
        | New class_name -> let cls_def,obj = init_object class_name in
                            (*initialiser les attributs*)
                            let () =  init_attributes_current_and_parents class_name obj
                            in obj 
        | NewCstr (class_name, param) -> let cls_def,obj = init_object class_name in
                                         let () =  init_attributes_current_and_parents class_name obj in
                                         let _ = eval_call "constructor" obj param false
                                         in obj                         
        | _ -> assert false 

    (*evalue une expression*)
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
                  | VArr a1, VArr a2 -> VBool (a1 == a2)
                  | _, _ -> VBool true  (*même étiquette, donc égaux*)
                )

        | Neq -> let vb = evalb (Binop(Eq, e1, e2)) in 
        if vb = true then VBool false else VBool true
          
        | Eq_struct -> let ev1 = eval e1 in 
                       let ev2 = eval e2 in 
                       let b = (ev1 = ev2) in 
                       if b = false then 
                        VBool false 
                      else 
                        (match ev1, ev2 with
                        | VObj v1, VObj v2 -> VBool (v1.fields = v2.fields) (*comparer structurellement les tables de hachage*)
                        | VArr a1, VArr a2 -> VBool (a1 = a2)
                        | _, _ -> VBool true  (*même étiquette, donc égaux*)
                        )
        | Neq_struct -> let vb = evalb (Binop(Eq_struct, e1, e2)) in 
                        if vb = true then VBool false else VBool true
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
      | MethCall (e, name, param) -> let objet = (evalo e) in
                                  eval_call name objet param false  (*e : objet, param: parametre de la fonction*)
      | Super(method_name, params) -> eval_call method_name (evalo This) params true
      | This -> Hashtbl.find lenv "this"
      | Instance_of(e, t) -> (*verifier que le type dynamique de e est un sous-type de t*)
                            let value = eval e in 
                            (match value with
                            | VObj objet ->
                                  let target_class = (match t with
                                      | TClass x -> x 
                                      | _ -> failwith "ce cas n'est pas atteignable") 
                                  in
                                  let class_name = objet.cls in 
                                  let rec is_instance_of class_name = 
                                    if class_name = target_class then true (*on vérifie si le type dynamique de e = t*)
                                    else let class_def = find_cls_def class_name p in  (*on regarde s'il est sous-type*)
                                    match class_def.parent with
                                    | None -> false 
                                    | Some parent_class_name -> is_instance_of parent_class_name
                                  in VBool (is_instance_of class_name)
                            | VArr _ | VInt _ | VBool _ | Null -> failwith "instance_of : cas non atteignable")
      | Transtyp(e, t) -> (*verifier que le type réel de e est un sous-type de t*)
                    let vb = evalb (Instance_of(e, t)) in 
                    let objet = evalo e in 
                    let class_name = objet.cls in 
                    let target_class = (match t with
                                | TClass x -> x 
                                | _ -> failwith "ce cas n'est pas atteignable") 
                            in
                    if vb then (eval e) else error (Printf.sprintf "Cannot cast from type %s to type %s" class_name target_class)
                
      | Array (elements) -> VArr (Array.map (fun elem -> eval elem) elements)
      | ArrayNull(t, n) -> (*créer un array contenant n éléments Null*)
                          let rec aux n acc = 
                          if n = 0 then acc 
                          else aux (n-1) (Null::acc) 
                        in VArr (Array.of_list (aux (evali n) [])) 

                               
    
  in
    (*execute une instruction*)
    let rec exec (i: instr): unit = match i with
      | Print e -> 
          let v = eval e in 
          (*on peut faire un appel à print_value ??*)
            let rec print_proc v = 
            match v with 
            | VInt n -> Printf.printf "%d" n
            | VBool b -> Printf.printf "%b" b
            | VObj _ -> Printf.printf "objet"
            | Null -> Printf.printf "null"
            | VArr (values) -> 
                let () = Printf.printf "[ " in 
                let () = Array.iter (fun v -> let () = print_proc v in Printf.printf "; ") values in
                Printf.printf " ]"
         in print_proc v; Printf.printf "\n";
      | If(e, s1, s2) -> let v = evalb e in if v = true then exec_seq s1 else exec_seq s2
      | While(e, s) -> let v = evalb e in 
          if v = true then 
          begin 
            exec_seq s;
            exec (While(e, s));
          end 
      | Set(m, e) -> (match m with
          | Var x -> if Hashtbl.mem lenv x then (*on regarde si notre variable est dans notre env local*)
                        Hashtbl.replace lenv x (eval e) (*on modife sa valeur*)
                    else 
                      if Hashtbl.mem env x then (*on regarde si notre variable est dans notre env global*)
                        Hashtbl.replace env x (eval e)
                      else
                        error (Printf.sprintf "Variable %s is undefined" x)
          | Field (exp_obj, attribute) -> 
            let obj_ = evalo exp_obj in 
            (* on modifie le champ de l'objet concerné*)
            Hashtbl.replace obj_.fields attribute (eval e);
          | ArrElem (t, index) -> let arr = evalarr t in 
                                  let ind = evali index in  
                                  if Array.length arr <= ind then error (Printf.sprintf "List index out of range")
                                  (* on modifie la valeur de arr[ind] en y affectant l'évaluation de e *)
                                  else arr.(ind) <- (eval e)
        )
      | Expr(e) -> let _ = eval e in () (*faire un match : si c'est une méthode appeler eval sinon renvoyer une erreur*)
      | Return(e) -> return_exp := eval e (*renvoie la valeur de retour*)

    (*execute une séquence d'instructions*)
    and exec_seq s = 
      List.iter exec s
    in
    exec_seq s
  in
  exec_seq p.main (Hashtbl.create 5)
