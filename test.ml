let rec separate_attributes (decl_liste : (string * 'a * bool) list) attr_list attr_final_list = 
  match decl_liste with 
  | [] -> (attr_list, attr_final_list)
  | (name, t, is_final)::suite -> if is_final then separate_attributes suite attr_list ((name,t)::attr_final_list)
                                  else separate_attributes suite ((name,t)::attr_list) attr_final_list