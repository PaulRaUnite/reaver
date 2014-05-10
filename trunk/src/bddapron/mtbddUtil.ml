(******************************************************************************)
(* MtbddUtil *)
(* manipulating products of Mtbdds *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="MtbddUtil";
              Log.level=Log.Debug3}

(******************************************************************************)
(* generic types *)
(******************************************************************************)

type 'b base_t = 'b (** leaf element of the base MTBDDs *)
type 'b array_t = 'b base_t array (** array of leaf elements *)
type 'b arrset_t = 'b array_t PSette.t (** set of arrays of leaf elements *)

type 'b base_mtbdd_t = 'b base_t Cudd.Mtbdd.t (** base MTBDD *)
type 'b array_mtbdd_t = 'b array_t Cudd.Mtbdd.t (** array MTBDD *)
type 'b arrset_mtbdd_t = 'b arrset_t Cudd.Mtbdd.t (** array set MTBDD *)

type 'b array_table_t = 'b array_t Cudd.Mtbdd.table  
                                             (** hash table for array MTBDDs *)
type 'b arrset_table_t = 'b arrset_t Cudd.Mtbdd.table 
                                       (** hash table for array set MTBDDs *)

type 'a compare_t = 'a -> 'a -> int

(******************************************************************************)
(* generic operations *)
(******************************************************************************)
(** converts an MTBDD into an array MTBDD  *)
let basemtbdd_to_arraymtbdd array_table basemtbdd = 
  Cudd.User.map_op1
    (fun e1 -> (Cudd.Mtbdd.unique array_table 
       (Array.make 1 (Cudd.Mtbdd.get e1))))
    basemtbdd

(******************************************************************************)
(** computes the product of two MTBDDs 
    by concatenating the leaves in an array *)
let arraymtbdd_product array_table arraymtbdd1 arraymtbdd2 =
  Cudd.User.map_op2
    (fun array1 array2 ->
      (Cudd.Mtbdd.unique array_table
	(Array.append (Cudd.Mtbdd.get array1) (Cudd.Mtbdd.get array2))))
    arraymtbdd1 arraymtbdd2

(******************************************************************************)
(** converts an array MTBDD to an arrset MTBDD *)
let arraymtbdd_to_arrsetmtbdd 
  ~(compare_array:'a array_t compare_t)
  (arrset_table:'a arrset_table_t)
  (arraymtbdd:'a array_mtbdd_t)
  : 'a arrset_mtbdd_t =
  Cudd.User.map_op1
    (fun e1 -> 
      Cudd.Mtbdd.unique arrset_table (PSette.add (Cudd.Mtbdd.get e1)
                                     (PSette.empty compare_array)))
    arraymtbdd

(******************************************************************************)
(** computes the product of two arrset MTBDDs,
    assuming that the leaf sets contain both exactly one element *)
let arrsetmtbdd_product arrset_table mtbdd1 mtbdd2 =
  Cudd.User.map_op2
    (fun set1 set2 ->
      let sset1 = Cudd.Mtbdd.get set1 in
      let compare_arrset = sset1.PSette.compare in
      (Cudd.Mtbdd.unique arrset_table (PSette.singleton (compare_arrset)
	(Array.append (PSette.choose sset1) 
                      (PSette.choose (Cudd.Mtbdd.get set2))))))
    mtbdd1 mtbdd2

(******************************************************************************)
(** quantifies supp in the arrayset MTBDD *)
let arrsetmtbdd_exists 
  (arrset_table:'a arrset_table_t)
  (arrsetmtbdd:'a arrset_mtbdd_t)
  (supp:Cudd.Bdd.vt)
  : 'a arrset_mtbdd_t =
  let merge_leaves a1 a2 = Cudd.Mtbdd.unique arrset_table
    (PSette.union (Cudd.Mtbdd.get a1) (Cudd.Mtbdd.get a2)) in
  let op2 = Cudd.User.make_op2 ~commutative:true (merge_leaves) in 
  let exist = Cudd.User.make_exist op2 in
  Cudd.User.apply_exist exist ~supp arrsetmtbdd

(******************************************************************************)
(** returns the list of guards of the given MTBDD *)
let get_guards_mtbdd mtbdd =
  let guardleaves = Cudd.Mtbdd.guardleafs mtbdd in
  Array.to_list (Array.map (fun (g,_) -> g) guardleaves)

(******************************************************************************)
(** returns the empty array set leaf *)
let arrsetmtbdd_empty ~compare_array arrset_table cuddman =
  Cudd.Mtbdd.cst_u cuddman
    (Cudd.Mtbdd.unique arrset_table (PSette.empty compare_array))

(******************************************************************************)
(** returns the list of conjunctions (paths) of the given mtbdd *)
let mtbdd_to_dnf cuddman mtbdd  =
  let cubelist = ref [] in
  let add_to_list cubarr leaf = 
    cubelist := (Cudd.Bdd.cube_of_minterm cuddman cubarr,
                 leaf)::!cubelist 
  in
  Cudd.Mtbdd.iter_cube add_to_list mtbdd;
  !cubelist

(******************************************************************************)
(* printing *)
(******************************************************************************)

let print_base_mtbdd env cond print_baseleaf fmt mtbdd =
  Cudd.Mtbdd.print
    (Bddapron.Expr0.O.print_bdd env cond)
    (print_baseleaf)
    fmt mtbdd

let print_array print_baseleaf fmt arr = 
  Format.pp_print_string fmt "[";
  Array.iter 
    (fun e -> print_baseleaf fmt e; 
       Format.pp_print_string fmt ";")
    arr;
  Format.pp_print_string fmt "]"

let print_array_mtbdd env cond print_baseleaf fmt arraymtbdd =
  Cudd.Mtbdd.print
    (Bddapron.Expr0.O.print_bdd env cond)
    (print_array print_baseleaf)
    fmt arraymtbdd

let print_arrset print_baseleaf fmt arrset = 
  PSette.print (print_array print_baseleaf) fmt arrset

let print_arrset_mtbdd env cond print_baseleaf fmt arrsetmtbdd =
  Cudd.Mtbdd.print
    (Bddapron.Expr0.O.print_bdd env cond)
    (print_arrset print_baseleaf)
    fmt arrsetmtbdd

(******************************************************************************)
(* instantiation: numerical actions *)
(******************************************************************************)
let hash_action env e = Bddapron.Apronexpr.hash env.Bdd.Env.symbol e

(* array hashing (as in the java 2 library) *)
let hash_action_array env arr = 
  Array.fold_left (fun accu el -> 31*accu+(hash_action env el)) 1 arr

let equal_action_array env a1 a2 = 
  let n = Array.length a1 in
  if n!=(Array.length a2) then false 
  else
    let rec check i = 
      if i>=n then true
      else
        if (Bddapron.Apronexpr.equal env.Bdd.Env.symbol a1.(i) a2.(i))  then 
          check (i+1)
        else false
    in
    check 0

let hash_action_arrset env arrset = 
  PSette.fold 
    (fun arr accu -> 31*accu+(hash_action_array env arr)) 
    arrset 1

(* PSette.equal relies on a compare function that guarantees 
   a (total) order of set elements, 
   otherwise PSette.equal will not work correctly! *)
let equal_action_arrset a1 a2 = PSette.equal a1 a2

let compare_action_arrset = Pervasives.compare

(******************************************************************************)
let make_table_action_array env = 
  Cudd.Mtbdd.make_table ~hash:(hash_action_array env)
                        ~equal:(equal_action_array env)
let make_table_action_arrset env = 
  Cudd.Mtbdd.make_table ~hash:(hash_action_arrset env) 
                        ~equal:equal_action_arrset

(******************************************************************************)
let print_action env fmt action = 
  Bddapron.Apronexpr.print env.Bdd.Env.symbol fmt action

(******************************************************************************)
(* instantiation: polymorphic actions *)
(******************************************************************************)

(** polymorphic leaf of numerical actions/labels/truth values  *)
type 'a poly_t = 
    | Apron of 'a Bddapron.Apronexpr.t
    | Bool of 'a Bddapron.Expr0.Bool.t
    | Benum of 'a
    | Bint of int * bool * int (* value * signed * length *)

let hash_poly env e = 
  match e with
    |Apron(e) -> Bddapron.Apronexpr.hash env.Bdd.Env.symbol e
    |Bool(e) -> Hashtbl.hash e
    |Benum(e) -> Hashtbl.hash e
    |Bint(e,_,_) -> Hashtbl.hash e

let hash_poly_array env arr = 
  Array.fold_left (fun accu el -> 31*accu+(hash_poly env el)) 1 arr

let hash_poly_arrset env set = 
  PSette.fold 
    (fun arr accu -> 31*accu+(hash_poly_array env arr)) 
    set 1

let equal_poly_array = Pervasives.(=)
let equal_poly_arrset a1 a2 = PSette.equal a1 a2
let compare_poly_arrset = Pervasives.compare

(******************************************************************************)
let make_table_poly_array env = 
  Cudd.Mtbdd.make_table ~hash:(hash_poly_array env) 
                        ~equal:equal_poly_array
let make_table_poly_arrset env = 
  Cudd.Mtbdd.make_table ~hash:(hash_poly_arrset env)
                        ~equal:equal_poly_arrset

(******************************************************************************)
let print_poly env cond fmt e = 
  match e with
   |Apron(e) -> Bddapron.Apronexpr.print env.Bdd.Env.symbol fmt e
   |Bool(e) -> Bddapron.Expr0.O.print_bdd env cond fmt e
   |Benum(e) -> env.Bdd.Env.symbol.Bdd.Env.print fmt e
   |Bint(e,_,_) -> Format.pp_print_int fmt e

(******************************************************************************)
(* converts a (BDD,leaf) list to an MTBDD *)
let bddleafarr_to_mtbdd make_leaf env bddleafarr =
  let cuddman = env.Bdd.Env.cudd in
  let n = Array.length bddleafarr in
  assert(n>=1);
  if (Array.length bddleafarr)=1 then 
    Cudd.Mtbdd.cst_u cuddman (make_leaf (snd bddleafarr.(0)))
  else
    let rec fold i =
      if i<n-2 then 
        Cudd.Mtbdd.ite (fst bddleafarr.(i)) 
          (Cudd.Mtbdd.cst_u cuddman (make_leaf (snd bddleafarr.(i))))
          (fold (i+1)) 
      else
        Cudd.Mtbdd.ite (fst bddleafarr.(i)) 
          (Cudd.Mtbdd.cst_u cuddman (make_leaf (snd bddleafarr.(i))))
          (Cudd.Mtbdd.cst_u cuddman 
            (make_leaf (snd bddleafarr.(i+1)))) 
    in
    fold 0


(******************************************************************************)
(* converts a Bddapron Expr0 into an MTBDD *)
let bddapron_to_mtbdd make_leaf env expr = 
  let cuddman = env.Bdd.Env.cudd in
  match expr with
    |`Apron(e) -> 
       Cudd.User.map_op1 (fun e -> make_leaf (Apron(Cudd.Mtbdd.get e))) e
    |`Benum(e) -> 
       let bddleafarr = Array.map (fun (bdd,l) -> (bdd,Benum l)) 
         (Array.of_list (Bdd.Enum.guardlabels env e)) in
       bddleafarr_to_mtbdd make_leaf env bddleafarr
    |`Bint(e) -> 
       let sign = e.Bdd.Int.signed in
       let len = Array.length e.Bdd.Int.reg in
       let bddleafarr = Array.map (fun (bdd,i) -> (bdd,Bint (i,sign,len))) 
         (Array.of_list (Bdd.Int.guardints cuddman e)) in
       bddleafarr_to_mtbdd make_leaf env bddleafarr
    |`Bool(e) -> 
       let bddleafarr = [|(e,Bool (Cudd.Bdd.dtrue cuddman));
              (Cudd.Bdd.dnot e,Bool (Cudd.Bdd.dfalse cuddman))|] in
       bddleafarr_to_mtbdd make_leaf env bddleafarr

(******************************************************************************)
(** converts a Bddapron Expr0 into an array MTBDD *)
let bddapronexpr_to_arraymtbdd env table_poly_array expr = 
  let make_leaf e = Cudd.Mtbdd.unique table_poly_array (Array.make 1 e) in
  bddapron_to_mtbdd (make_leaf) env expr

(******************************************************************************)
(** converts a Bddapron Expr0 into an arrset MTBDD *)
let bddapronexpr_to_arrsetmtbdd env table_poly_arrset expr = 
  let make_leaf e = Cudd.Mtbdd.unique table_poly_arrset 
    (PSette.singleton (Pervasives.compare) (Array.make 1 e))
  in
  bddapron_to_mtbdd (make_leaf) env expr

(*
(* TODO: try out: why not use Pervasives.compare??? *)
let compare_set a1 a2 = 
  (* use the difference of hash values as order relation *)
  let diff = (hash a1)-(hash a2) in
  (* if hash values are equal then compare for actual equality 
     Remark: This may still lead to false results 
             in PSette.equal in some cases*)
  if diff==0 then 
    if equal a1 a2 then 0 else (-1)
  else diff*)
