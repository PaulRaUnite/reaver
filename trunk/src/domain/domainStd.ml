(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

module LoosePol : Domain.BDDAPRON_MAN_T =
  struct
    type apronman_t = Polka.loose Polka.t
    type 'a bddapronman_t = ('a, apronman_t) Bddapron.Domain0.bdd
    type ('a, 'b) man_t = ('a, apronman_t, 'a bddapronman_t, 'b) 
                   Bddapron.Domain0.man

    let make_man () = 
      let bman: 'a bddapronman_t = 
        Bddapron.Domain0.make_bdd (Polka.manager_alloc_loose ()) in
      Bddapron.Domain0.man_of_bdd bman
  end

module StrictPol : Domain.BDDAPRON_MAN_T =
  struct
    type apronman_t = Polka.strict Polka.t
    type 'a bddapronman_t = ('a, apronman_t) Bddapron.Domain0.bdd
    type ('a, 'b) man_t = ('a, apronman_t, 'a bddapronman_t, 'b) 
                   Bddapron.Domain0.man

    let make_man () = 
      let bman: 'a bddapronman_t = 
        Bddapron.Domain0.make_bdd (Polka.manager_alloc_strict ()) in
      Bddapron.Domain0.man_of_bdd bman
  end

module LoosePolPow = Domain.Pow(LoosePol)
module LoosePolProd = Domain.Prod(LoosePol)

module StrictPolPow = Domain.Pow(StrictPol)
module StrictPolProd = Domain.Prod(StrictPol)

module Oct : Domain.BDDAPRON_MAN_T =
  struct
    type apronman_t = Oct.t
    type 'a bddapronman_t = ('a, apronman_t) Bddapron.Domain0.bdd
    type ('a, 'b) man_t = ('a, apronman_t, 'a bddapronman_t, 'b) 
                   Bddapron.Domain0.man

    let make_man () = 
      let bman: 'a bddapronman_t = 
        Bddapron.Domain0.make_bdd (Oct.manager_alloc ()) in
      Bddapron.Domain0.man_of_bdd bman
  end

module OctPow = Domain.Pow(Oct)
module OctProd = Domain.Prod(Oct)

module Box : Domain.BDDAPRON_MAN_T =
  struct
    type apronman_t = Box.t
    type 'a bddapronman_t = ('a, apronman_t) Bddapron.Domain0.bdd
    type ('a, 'b) man_t = ('a, apronman_t, 'a bddapronman_t, 'b) 
                   Bddapron.Domain0.man

    let make_man () = 
      let bman: 'a bddapronman_t = 
        Bddapron.Domain0.make_bdd (Box.manager_alloc ()) in
      Bddapron.Domain0.man_of_bdd bman
  end

module BoxPow = Domain.Pow(Box)
module BoxProd = Domain.Prod(Box)
