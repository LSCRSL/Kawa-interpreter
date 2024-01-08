open Kawa
open Parsing

exception Error of string
exception Missing_semi


let error s = raise (Error s)


type visibility = Private | Protected | Public

let rec aux liste acc_decl acc_instr = 
  match liste with 
  | [] -> (acc_decl, acc_instr)
  | (l1, l2)::suite -> aux suite (l1 @ acc_decl) (acc_instr @ l2)


let rec map_ident_val_attributes (t : typ) (is_final : bool) (visib : visibility)
                      (names : string list) 
                      (values : expr list) 
                      (tuples_list : (string * typ * bool * visibility) list) 
                      (instr_list : instr list) = 
  match names, values with 
  | [], [] -> (tuples_list, instr_list)
  | _::_, [] -> failwith "not enough values to unpack"
  | [], _::_ -> failwith "too many values to unpack"
  | name::s1, value::s2 -> map_ident_val_attributes t is_final visib s1 s2 ((name, t, is_final, visib)::tuples_list) (Set(Field(This,name), value)::instr_list)


let rec map_ident_val_variables (t : typ)
                      (names : string list) 
                      (values : expr list) 
                      (tuples_list : (string * typ) list) 
                      (instr_list : instr list) = 
    match names, values with 
    | [], [] -> (tuples_list, instr_list)
    | _::_, [] -> failwith "not enough values to unpack"
    | [], _::_ -> failwith "too many values to unpack"
    | name::s1, value::s2 -> map_ident_val_variables t s1 s2 ((name, t)::tuples_list) (Set(Var name, value)::instr_list)


let rec find_cls_def class_name p =
  let get_first_element class_def_filter = 
    match class_def_filter with 
      | [] ->  failwith "the class is not implemented"
      | e::[] -> e
      | _ -> failwith "multiple classes have the same name"
  in
  let class_list = p.classes in 
  let class_def_filter = List.filter (fun cls_def -> (cls_def.class_name = class_name)) class_list in
  get_first_element class_def_filter 

and find_mthd_def (class_name : string) (mthd_name : string) (p : program) =
  let class_def = find_cls_def class_name p in
  let method_list = List.filter (fun method_def -> (method_def.method_name = mthd_name)) class_def.methods in
  let get_mthd (mthd_list : method_def list) : method_def = 
    match mthd_list with 
    | [] -> (match class_def.parent with 
            | None -> Printf.printf "%s" mthd_name; error "the method is not implemented"
            | Some parent_cls_name -> find_mthd_def parent_cls_name mthd_name p)
    | e::[] -> e
    | _ -> failwith "surcharge non implémentée"
  in 
  get_mthd method_list

let rec separate_attributes (decl_liste : (string * typ * bool * visibility) list) 
                            attr_type_list 
                            attr_final_names 
                            attr_private_names 
                            attr_protected_names = 
    let modify_final_attr is_final elem list = 
        if is_final then elem::list else list
    in 

    let modify_visib_attr visib elem private_list protected_list =
      match visib with
      | Private -> elem::private_list, protected_list
      | Protected -> private_list, elem::protected_list
      | Public -> private_list, protected_list

    in
  
    match decl_liste with 
    | [] -> attr_type_list, attr_final_names, attr_private_names, attr_protected_names
    | (name, t, is_final, v)::suite -> let attr_final_mod = modify_final_attr is_final name attr_final_names in 
                                       let attr_private_mod, attr_protected_mod = modify_visib_attr v name attr_private_names attr_protected_names in
                                       separate_attributes suite ((name,t)::attr_type_list) attr_final_mod attr_private_mod attr_protected_mod
                                       

let rec is_sub_class sub_class super_class p = 
  if sub_class = super_class then true
  else let class_def = find_cls_def sub_class p in 
        match class_def.parent with
        | None -> false 
        | Some parent_class_name -> is_sub_class parent_class_name super_class p

let rec is_implemented (class_name : string) (mthd_name : string) (p : program) =
  let class_def = find_cls_def class_name p in
  let method_list = List.filter (fun method_def -> (method_def.method_name = mthd_name)) class_def.methods in
  let get_mthd (mthd_list : method_def list) = 
    match mthd_list with 
    | [] -> (match class_def.parent with 
            | None -> false
            | Some parent_cls_name -> is_implemented parent_cls_name mthd_name p)
    | e::[] -> true
    | _ -> failwith "surcharge non implémentée"
  in 
  get_mthd method_list