let new_mthd_list = match method_list with 
  | [] -> let mthd_constr = {method_name="constructor";code=(snd acc_tuple); params=[]; locals=[]; return=TVoid} in 
          mthd_constr::mthd
  | e::[] -> let e.code = (snd acc_tuple)@ e.code in
            mthd
  | e::l -> failwith "too many constructor"
in