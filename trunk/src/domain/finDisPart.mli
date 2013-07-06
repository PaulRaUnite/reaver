(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** domain implementation: finitely disjunctive partitioned domain *)

(** abstract value type of the finitely disjunctive partitioned domain *)
type 'apronman aprontuple_t = 
  'apronman Apron.Abstract1.t PSette.t * 'apronman Apron.Abstract1.t

(** abstract value type of the finitely disjunctive partitioned
    logico-numerical product domain *)
type 'apronman bddaprontuple_t = Env.boolexpr_t * 'apronman aprontuple_t 

(** finitely disjunctive partitioned logico-numerical product domain *)
module FdpProd(Man : Domain.BDDAPRON_MAN_T) :
  (Domain.NOPARAM_T with type t = Man.apronman_t bddaprontuple_t
     with type numdomain_t = Man.apronman_t Apron.Abstract0.t
     with type doman_t = (Env.var_t, Env.var_t Bddapron.Domain0.t) Man.man_t
  ) 

(** finitely disjunctive partitioned logico-numerical product domain 
    with intervals *)
module FdpProdInt : Domain.NOPARAM_T
