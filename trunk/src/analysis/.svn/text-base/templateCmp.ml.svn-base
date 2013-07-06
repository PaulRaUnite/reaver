(******************************************************************************)
(* templateCmp *)
(* template comparison statistics *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="TemplateCmp";
              Log.level=Log.Debug}

(* compute statistics about two results: locs1 <= locs2
   - number of boolean reachable states anres1, anres2
   - number of bounds anres1 < anres2
   - number of bounds anres1 = anres2
   - number of bounds anres1 > anres2
*)

type stats_t = 
{
  mutable b1: int;
  mutable b2: int;
  mutable lt: int;
  mutable eq: int;
  mutable eqinf: int;
  mutable gt: int;
  mutable gtinf1: int;
  mutable gtinf2: int;
  mutable gtdev : float;
}

let get_local_template local_templates loc =
 try List.assoc loc local_templates 
 with Not_found -> []

let template_comparison_stat env cfprog1 anres1 cfprog2 anres2 
  global_template local_templates =
  let cfg1 = cfprog1.Program.c_cfg in
  let cfg2 = cfprog2.Program.c_cfg in
  let apronman = Polka.manager_alloc_loose () in
  let initial = cfprog1.Program.c_init in
  let locmap = Cfg.get_locmap env cfg1 cfg2 in (* l1 -> [l2...] *)
(*  let expand anres locmap = List.flatten (List.map 
    (fun (v,s) -> 
      let vs = List.assoc v locmap in
      List.map (fun v -> (v,s)) vs)
    anres)
  in *)
  let reduce  (anres:Analysis.bddapron_res_t) locmap = 
    Util.list2mappe (List.map
    (fun (loc,locs) -> 
      let (b,n) = List.fold_left
        (fun sacc s  -> 
          List.fold_left (fun (bacc,nacc) (b,n)  ->
            (Cudd.Bdd.dor bacc b,
             Apron.Abstract1.join apronman nacc 
               (Apron.Abstract1.of_lincons_array apronman env.Env.apronenv n)))
            sacc s)
        (Cudd.Bdd.dfalse env.Env.cuddman,
         Apron.Abstract1.bottom apronman env.Env.apronenv)
        (List.map (fun v -> Mappe.find v anres) locs) 
      in
      (loc,[(b,Apron.Abstract1.to_lincons_array apronman n)]))
    locmap)
  in
  let get_bool_space_size (anres:Analysis.bddapron_res_t) =
    let reach = Mappe.fold
      (fun _ s res -> 
         if Util.list_is_empty s then res 
         else 
           List.fold_left (fun res (b,_) -> Cudd.Bdd.dor res b) res s)
      anres (Cudd.Bdd.dfalse env.Env.cuddman)  
    in
    BddapronUtil.bool_space_size env.Env.env env.Env.cond env.Env.bs_vars
      (Cudd.Bdd.dand reach (Cudd.Bdd.dnot initial))
  in
  let get_max n t = 
    if Apron.Abstract1.is_bottom apronman n then 
      Apron.Scalar.of_infty (-1)
    else
    if Apron.Abstract1.is_top apronman n then 
      Apron.Scalar.of_infty 1
    else
      let minmax = Apron.Abstract1.bound_linexpr apronman n t in
      minmax.Apron.Interval.sup 
  in
  let compare_template templates stats n1 n2 =
    List.fold_right (fun t stats ->
      let max1 = get_max n1 t in
      let max2 = get_max n2 t in
      let incinf max = if (Apron.Scalar.is_infty max)>0 then 1 else 0 in
      let scalar_to_float c = 
        match c with
	  |Apron.Scalar.Float c -> c
	  |Apron.Scalar.Mpqf c -> Mpqf.to_float c
	  |Apron.Scalar.Mpfrf c -> Mpfrf.to_float c
      in
      let incdev max1 max2 = 
        if (Apron.Scalar.is_infty max1)=0 && 
           (Apron.Scalar.is_infty max2)=0 then 
          let m1 = scalar_to_float max1 in
          let m2 = scalar_to_float max2 in
          if Apron.Scalar.equal_int max2 0 then 
            abs_float (m1 -. m2)
          else abs_float ((m1 -. m2) /. m2)
        else 0. 
      in
      let cmp = Apron.Scalar.cmp max1 max2 in
(*      Log.debug3_o logger (fun fmt (max1,max2,cmp) ->
        Apron.Scalar.print fmt max1; Format.pp_print_string fmt "   ";
        Apron.Scalar.print fmt max2; Format.pp_print_string fmt "   ";
        Format.pp_print_int fmt cmp) "" (max1,max2,cmp);*)
      if cmp<0 then {stats with lt = stats.lt+1}
      else if cmp>0 then 
        {stats with gt = stats.gt+1; 
                    gtinf1 = stats.gtinf1 + (incinf max1);
                    gtinf2 = stats.gtinf2 + (incinf max2);
                    gtdev = stats.gtdev +. (incdev max1 max2)}
      else {stats with eq = stats.eq+1;
                    eqinf = stats.eqinf + (incinf max1)})
    templates stats
  in
  let convexify_num s = 
    List.fold_left
        (fun nacc (_,ne)  -> 
          Apron.Abstract1.join apronman nacc 
            (Apron.Abstract1.of_lincons_array apronman env.Env.apronenv ne))
      (Apron.Abstract1.bottom apronman env.Env.apronenv) s
  in
  let anres2 = reduce anres2 locmap in
  let stats = 
  {b1 = get_bool_space_size anres1;
   b2 = get_bool_space_size anres2;
   lt=0;
   eq=0;eqinf=0;
   gt=0;gtinf1=0;gtinf2=0;gtdev=0.0}
  in
  let stats = Mappe.fold
    (fun v s1 stats ->
      let s2 = Mappe.find v anres2 in
      let n1 = convexify_num s1 in
      let n2 = convexify_num s2 in
      Log.debug3_o logger (Apron.Abstract1.print) "n1 = " n1;
      Log.debug3_o logger (Apron.Abstract1.print) "n2 = " n2;
      let templates = List.append global_template 
        (get_local_template local_templates v) in
      compare_template templates stats n1 n2)
    anres1 stats
  in
  stats

let print_template_comparison_stat fmt stats =
  Format.pp_print_newline fmt ();
  Format.pp_print_string fmt 
    ("reachable bool space size 1 = "^(string_of_int stats.b1));
  Format.pp_print_newline fmt ();
  Format.pp_print_string fmt 
     ("reachable bool space size 2 = "^(string_of_int stats.b2));
  Format.pp_print_newline fmt ();
  Format.pp_print_string fmt 
     ("better bounds = "^(string_of_int stats.lt));
  Format.pp_print_newline fmt ();
  Format.pp_print_string fmt 
     ("equal  bounds = "^(string_of_int stats.eq));
  Format.pp_print_newline fmt ();
  Format.pp_print_string fmt 
    ("infinite equal bounds = "^(string_of_int stats.eqinf));
  Format.pp_print_newline fmt ();
  Format.pp_print_string fmt 
     ("worse  bounds = "^(string_of_int stats.gt));
  Format.pp_print_newline fmt ();
  Format.pp_print_string fmt 
    ("infinite worse bounds 1 = "^(string_of_int stats.gtinf1));
  Format.pp_print_newline fmt ();
  Format.pp_print_string fmt 
     ("infinite worse bounds 2 = "^(string_of_int stats.gtinf2));
  Format.pp_print_newline fmt ();
  Format.pp_print_string fmt 
     ("deviation of worse bounds = "^(string_of_float stats.gtdev));
  Format.pp_print_newline fmt ();
  Format.pp_print_string fmt "***** summary *****";
  let total = stats.lt+stats.eq+stats.gt in
  let totfinite = total-stats.eqinf-stats.gtinf2 in
  let kept = stats.eq-stats.eqinf in
  let worse = stats.gt-stats.gtinf1 in
  let lost = stats.gtinf1-stats.gtinf2 in
  let base = totfinite in
  Format.pp_print_newline fmt ();
  Format.pp_print_string fmt 
    ("         total bounds = "^(string_of_int total));
  Format.pp_print_newline fmt ();
  Format.pp_print_string fmt 
    ("total finite bounds 2 = "^
       (string_of_int totfinite));
  Format.pp_print_newline fmt ();
  Format.pp_print_string fmt 
     ("        better bounds = "^(string_of_int stats.lt)^
    " ("^(string_of_float ((float_of_int stats.lt)/.(float_of_int base)))^")");
  Format.pp_print_newline fmt ();
  Format.pp_print_string fmt 
     ("          kept bounds = "^(string_of_int kept)^
    " ("^(string_of_float ((float_of_int kept)/.(float_of_int base)))^")");
  Format.pp_print_newline fmt ();
  Format.pp_print_string fmt 
     ("         worse bounds = "^(string_of_int worse)^
    " ("^(string_of_float ((float_of_int worse)/.(float_of_int base)))^")"^
    " dev = "^(string_of_float (stats.gtdev /. (float_of_int worse))));
  Format.pp_print_newline fmt ();
  Format.pp_print_string fmt 
     ("          lost bounds = "^(string_of_int lost)^
    " ("^(string_of_float ((float_of_int lost)/.(float_of_int base)))^")")
