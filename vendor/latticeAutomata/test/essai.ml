(** Test file *)

open Format

module Alphabet = PLattice.MakeAlphabet(struct
  type t = char
  let compare (x:t) (y:t) = Pervasives.compare x y
  let sep = '#'
end)

module Finite = struct
  module PL = PLattice.Make(Alphabet)
  module A = LAutomaton.Make(PL)
  let lattice = A.lunit

  let print_letter fmt (letter:unit PL.t) =
    PL.print
      ~firstbind:""
      ~sepbind:""
      ~lastbind:""
      pp_print_char
      (fun fmt _ -> ())
      fmt letter

 let print_regexp (title:string) fmt (auto: unit A.t) : unit =
    fprintf fmt "%s: @[%a@]@."
      title
      (Regexp.print print_letter) (Regexp.simplify (PL.is_eq lattice) (A.to_regexp auto));

    fprintf fmt "automaton:@.@[%a@]@."
      (A.print print_letter) auto

  let print_dot (title:string) fmt (auto:unit A.t) : unit =
    let info = LAutomaton.Graph.info auto in
    let title = sprintf "%s\\ndet=%b,min=%b" title info.LAutomaton.det info.LAutomaton.min in
    A.print_dot ~title print_letter fmt auto

 

  let letters (l:char list) : unit PL.t =
    List.fold_left
      (begin fun res c -> Alphabet.Map.add c () res end)
      Alphabet.Map.empty l

  let letter (c:char) : unit PL.t = letters [c]

  let (partition : unit Alphabet.Map.t) =
    letters ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j']

  let complementary fmt (auto:unit A.t) : unit
    =
    Format.fprintf fmt "auto @[%a@] @. compl @[%a@] @."
      (A.print print_letter) auto
      (A.print print_letter) (A.complementary auto)


  let itself fmt (auto:unit A.t) : unit
    =
    print_dot "itself" fmt auto;
    print_dot "itself union" fmt (A.union auto auto);
    print_dot "itself union normalized" fmt (A.minimise (A.union auto auto));
    print_dot "itself intersection" fmt (A.inter auto auto);
    print_dot "itself intersection normalized" fmt (A.minimise (A.inter auto auto));
    print_dot "itself concatenation" fmt (A.Language.concat auto auto);
    print_dot "itself concatenation normalized" fmt (A.minimise (A.Language.concat auto auto));
    print_dot "itself plus" fmt (A.Language.plus auto);
    print_dot "itself plus normalized" fmt (A.minimise (A.Language.plus auto));
    print_dot "itself star" fmt (A.Language.star auto);
    print_dot "itself star normalized" fmt (A.minimise (A.Language.star auto));
    (* complementary std_formatter auto; *)
    ()


  let main () =
    let file = open_out "essai.dot" in
    let fmt = formatter_of_out_channel file in

    (* Build the language (a+b+c)abbcc *)
    let word0 = A.Language.empty ~lattice ~partition in
    print_dot "empty" fmt word0;
    let word1 = A.Language.cons_right word0 (letters ['a';'b';'c']) in
    print_dot "a+b+c" fmt word1;
    let word1 = A.Language.cons_right word1 (letter 'a') in
    print_dot "(a+b+c)a" fmt word1;
    let word1 = A.Language.cons_right word1 (letter 'b') in
    let word1 = A.Language.cons_right word1 (letter 'b') in
    let word1 = A.Language.cons_right word1 (letter 'c') in
    let word1 = A.Language.cons_right word1 (letter 'c') in
    print_dot "(a+b+c)abbcc" fmt word1;
    (* itself fmt word1; *)

    (* Build the language add(d+e)e(e+f)gg differently *)
    let word2 = A.Language.empty ~lattice ~partition in
    print_dot "empty" fmt word2;
    let word2 = A.Language.cons_left word2 (letter 'g') in
    print_dot "g" fmt word2;
    let word2 = A.Language.cons_left word2 (letter 'g') in
    print_dot "gg" fmt word2;
    let word2 = A.Language.cons_left word2 (letters ['e';'f']) in
    let word2 = A.Language.cons_left word2 (letters ['d';'e']) in
    let word2 = A.Language.cons_left word2 (letter 'd') in
    let word2 = A.Language.cons_left word2 (letter 'd') in
    let word2 = A.Language.cons_left word2 (letter 'a') in
    print_dot "add(d+e)(e+f)gg" fmt word2;
    (* itself fmt word2; *)

    let word3b = A.inter word1 word2 in
    print_dot "(a+b+c)abbcc inter add(d+e)e(e+f)gg" fmt word3b;
    let word3b = A.minimise word3b in
    print_dot "(a+b+c)abbcc inter add(d+e)e(e+f)gg normalized" fmt word3b;
    (* itself fmt word3; *)

    let word3 = A.union word1 word2 in
    print_dot "(a+b+c)abbcc+add(d+e)e(e+f)gg" fmt word3;
    let word3 = A.minimise word3 in
    print_dot "(a+b+c)abbcc+add(d+e)e(e+f)gg normalized" fmt word3;
    (* itself fmt word3; *)

    let word4 = A.union (A.Language.star word1) (A.Language.star word2) in
    print_dot "((a+b+c)abbcc)^*+(add(d+e)e(e+f)gg)^*" fmt word4;
    let word4 = A.minimise word4 in
    print_dot "((a+b+c)abbcc)^*+(add(d+e)e(e+f)gg)^* normalized" fmt word4;
    (* itself fmt word4; *)

    close_out file;
    ()

  let main2 () =
    let file = open_out "essai.dot" in
    let fmt = formatter_of_out_channel file in

    let worda = A.Language.of_regexp ~lattice ~partition
      (Regexp.Exp (Regexp.Concat
	[Regexp.Letter(letters ['a';'b';'c']);
	(Regexp.Plus (
	  Regexp.Concat [
	    Regexp.Letter(letter 'a');
	    Regexp.Letter(letter 'b');
	  ]))]))
    in
    print_dot "(a+b+c)(ab)^+" fmt worda;
    print_regexp "(a+b+c)(ab)^+  " std_formatter worda;

    let wordb = A.Language.of_regexp ~lattice ~partition	
      (Regexp.Exp (Regexp.Star (
	Regexp.Concat [
	  Regexp.Letter(letter 'b');
	  Regexp.Letter(letter 'c');
	  Regexp.Letter(letter 'c');
	])))
    in
    print_dot "(bcc)^*" fmt wordb;
    print_regexp "(bcc)^*  " std_formatter wordb;

    
    let wordc = A.Language.concat worda wordb in
    print_dot "(a+b+c)(ab)^+(bcc)^*" fmt wordc;
    print_regexp "(a+b+c)(ab)^+(bcc)^*" std_formatter wordc;


    let word1 = A.Language.of_regexp ~lattice ~partition	
      (Regexp.Exp (Regexp.Concat
	[Regexp.Letter(letters ['a';'b';'c']);
	(Regexp.Plus (
	  Regexp.Concat [
	    Regexp.Letter(letter 'a');
	    Regexp.Letter(letter 'b');
	  ]));
	(Regexp.Star (
	  Regexp.Concat [
	    Regexp.Letter(letter 'b');
	    Regexp.Letter(letter 'c');
	    Regexp.Letter(letter 'c');
	  ]));
	]))
    in
    print_dot "(a+b+c)(ab)^+(bcc)^*" fmt word1;
    print_regexp "(a+b+c)(ab)^+(bcc)^*  " std_formatter word1;
    let word1 = A.minimise word1 in
    print_dot "(a+b+c)(ab)^+(bcc)^*" fmt word1;
    print_regexp "(a+b+c)(ab)^+(bcc)^*  " std_formatter word1;

    (* Build the language (a+b+c)(ab)*#(bcc)* *)
    let word1 = A.of_regexp ~lattice ~partition 2 
      (Regexp.Exp (Regexp.Concat
	[Regexp.Letter(letters ['a';'b';'c']);
	(Regexp.Plus (
	  Regexp.Concat [
	    Regexp.Letter(letter 'a');
	    Regexp.Letter(letter 'b');
	  ]));
	Regexp.Letter (PL.sep lattice);
	(Regexp.Star (
	  Regexp.Concat [
	    Regexp.Letter(letter 'b');
	    Regexp.Letter(letter 'c');
	    Regexp.Letter(letter 'c');
	  ]));
	]))
    in
   (*  complementary std_formatter word1; *)
    print_dot "(a+b+c)(ab)^+#(bcc)^*" fmt word1;



      let word1 = A.minimise word1 in
    print_dot "(a+b+c)(ab)^+#(bcc)^*" fmt word1;

     (* itself fmt word1; *)

    (* Build the language a(dd)^*(d+e)e#((e+f)g)^+g^+ differently *)
    let word2 = A.of_regexp ~lattice ~partition 2 
      (Regexp.Exp (Regexp.Concat
	[Regexp.Letter(letter 'a');
	(Regexp.Star (
	  Regexp.Concat [
	    Regexp.Letter(letter 'd');
	    Regexp.Letter(letter 'd')
	  ]));
	Regexp.Letter(letters ['d';'e']);
	Regexp.Letter(letter 'e');
	Regexp.Letter (PL.sep lattice);
	(Regexp.Plus (
	  Regexp.Concat [
	    Regexp.Letter(letters ['e';'f']);
	    Regexp.Letter(letter 'g')
	  ]));
	Regexp.Plus (Regexp.Letter(letter 'g'));
	]))
    in
    print_dot "a(dd)^*(d+e)e#((e+f)g)^+g^+" fmt word2;
    let word2 = A.minimise word2 in
    print_dot "a(dd)^*(d+e)e#((e+f)g)^+g^+" fmt word2;

    let word3 = A.inter word1 word2 in
    print_dot "(a+b+c)abbcc inter add(d+e)e(e+f)gg" fmt word3;
    let word3 = A.minimise word3 in
    print_dot "(a+b+c)abbcc inter add(d+e)e(e+f)gg normalized" fmt word3;
 (*   itself fmt word3; *)

    let word3 = A.union word1 word2 in
    print_dot "word3=(a+b+c)abbcc+add(d+e)e(e+f)gg" fmt word3;
    let word3 = A.minimise word3 in
    print_dot "word3=(a+b+c)abbcc+add(d+e)e(e+f)gg normalized" fmt word3;
 (*   itself fmt word3; *)

    (* try cons and deriv *)
    let word4 = A.cons_left word3 0 (letter 'h') in
    print_dot "cons_left 0 h" fmt word4;
    print_dot "normalized" fmt (A.minimise word4);
    let word4 = A.cons_left word3 1 (letter 'h') in
    print_dot "cons_left 1 h" fmt word4;
    print_dot "normalized" fmt (A.minimise word4);
    
    let word4 = A.cons_right word3 0 (letter 'g') in
    print_dot "cons_right 0 g" fmt word4;
    print_dot "normalized" fmt (A.minimise word4);
    let word4 = A.cons_right word3 1 (letter 'g') in
    print_dot "cons_right 1 g" fmt word4;
    print_dot "normalized" fmt (A.minimise word4);

    let word4 = A.deriv_left ~cond:(letters ['b';'e']) word3 0 in
    print_dot "deriv_left 0 be" fmt word4;
    print_dot "normalized" fmt (A.minimise word4);
    let word4 = A.deriv_left ~cond:(letters ['b';'e']) word3 1 in
    print_dot "deriv_left 1 be" fmt word4;
    print_dot "normalized" fmt (A.minimise word4);
    
    let word4 = A.deriv_right ~cond:(letters ['b';'e']) word3 0 in
    print_dot "deriv_right 0 be" fmt word4;
    print_dot "normalized" fmt (A.minimise word4);
    let word4 = A.deriv_right ~cond:(letters ['c';'g']) word3 1 in
    print_dot "deriv_right 1 cg" fmt word4;
    print_dot "normalized" fmt (A.minimise word4);


    close_out file;
    ()

end

module Int = struct

  module L = struct
    type t = int*int
    let sep = (0,0)
    let is_bottom (a,b) = a>b
    let is_leq (a1,b1) (a2,b2) =
      if is_bottom (a1,b1) then true
      else if is_bottom (a2,b2) then false
      else
	a1>=a2 && b1<=b2
    let is_eq (a1,b1) (a2,b2) =
      if is_bottom (a1,b1) then is_bottom (a2,b2)
      else if is_bottom (a2,b2) then false
      else a1==a2 && b1==b2
    let join (a1,b1) (a2,b2) =
      if is_bottom (a1,b1) then (a2,b2)
      else if is_bottom (a2,b2) then (a1,b1)
      else (Pervasives.min a1 a2, Pervasives.max b1 b2)
    let meet (a1,b1) (a2,b2) =
      (Pervasives.max a1 a2, Pervasives.min b1 b2)
    let widening (a1,b1) (a2,b2) =
      if not (is_leq (a1,b1) (a2,b2)) then failwith "";
      if is_bottom (a1,b1) then (a2,b2)
      else (
	(if a2<a1 then min_int else a1),
	if b2>b1 then max_int else b1
      )
  end
  let (lattice : (char,int*int) PLattice.glattice) = {
    PLattice.sep = L.sep;
    PLattice.is_bottom = (begin fun letter -> L.is_bottom end);
    PLattice.is_leq = (begin fun letter -> L.is_leq end);
    PLattice.is_eq = (begin fun letter -> L.is_eq end);
    PLattice.join = (begin fun letter -> L.join end);
    PLattice.meet = (begin fun letter -> L.meet end);
    PLattice.widening = (begin fun letter -> L.widening end);
  }
    
  module PL = PLattice.Make(Alphabet)
  module A = LAutomaton.Make(PL)
  
 


  let print_interval fmt ((a,b) as elt) =
    if L.is_bottom elt then
      pp_print_string fmt "bot"
    else
      fprintf fmt "[%i,%i]" a b

  let print_letter fmt (letter:(int*int) PL.t) =
    PL.print
      pp_print_char
      print_interval
      fmt letter

  let print_dot (title:string) fmt (auto:(int*int) A.t) : unit =
    let info = LAutomaton.Graph.info auto in
    let title = sprintf "%s\\ndet=%b,min=%b" title info.LAutomaton.det info.LAutomaton.min in
    A.print_dot
      ~title
      print_letter
      fmt auto

  let letters (l:(char*L.t) list) : L.t Alphabet.Map.t =
    List.fold_left
      (begin fun res (c,elt) -> Alphabet.Map.add c elt res end)
      Alphabet.Map.empty l

  let letter (c:char*L.t) = letters [c]

  let (partition : L.t Alphabet.Map.t) =
    letters [
      ('a',(-30,-21));
      ('b',(-20,-11));
      ('c',(-10,-1));
      ('d',(0,9));
      ('e',(10,19));
      ('f',(20,29));
      ('g',(30,39));
      ]

  let itself fmt (auto:(int*int) A.t) : unit
    =
    print_dot "itself" fmt auto;
    print_dot "itself union" fmt (A.union auto auto);
    print_dot "itself union normalized" fmt (A.minimise (A.union auto auto));
    print_dot "itself intersection" fmt (A.inter auto auto);
    print_dot "itself intersection normalized" fmt (A.minimise (A.inter auto auto));
    print_dot "itself concatenation" fmt (A.Language.concat auto auto);
    print_dot "itself concatenation normalized" fmt (A.minimise (A.Language.concat auto auto));
    print_dot "itself plus" fmt (A.Language.plus auto);
    print_dot "itself plus normalized" fmt (A.minimise (A.Language.plus auto));
    print_dot "itself star" fmt (A.Language.star auto);
    print_dot "itself star normalized" fmt (A.minimise (A.Language.star auto));

    let pletter = letters [('c',(-4,-4));('e',(12,12));('a',(-22,22))] in
    let nauto = A.Language.cons_right auto pletter in
    print_dot "itself cons_right ('c',(-4,-4));('e',(12,12));('a',(-22,22))" fmt nauto;
    print_dot "itself cons_right normalized" fmt (A.minimise nauto);
    let nauto = A.Language.cons_left auto pletter in
    print_dot "itself cons_left ('c',(-4,-4));('e',(12,12));('a',(-22,22))" fmt nauto;
    print_dot "itself cons_left normalized" fmt (A.minimise nauto);

    let nauto = A.Language.deriv_right auto in
    print_dot "itself deriv_right" fmt nauto;
    print_dot "itself deriv_right normalized" fmt (A.minimise nauto);
    let nauto = A.Language.deriv_left auto in
    print_dot "itself deriv_left" fmt nauto;
    print_dot "itself deriv_left normalized" fmt (A.minimise nauto);
    ()

  (* auto1 is supposed strictly included in auto2 *)
  let binary fmt (auto1:(int*int) A.t) (auto2:(int*int) A.t) : unit
    =
    assert((LAutomaton.Graph.info auto1).LAutomaton.det);
    assert((LAutomaton.Graph.info auto2).LAutomaton.det);
    print_dot "binary: auto1" fmt auto1;
    print_dot "binary: auto2" fmt auto2;
    assert(A.is_leq auto1 auto2);
    assert(not (A.is_leq auto2 auto1));
    assert(not (A.is_eq auto1 auto2));

    let auto = A.inter auto1 auto2 in
    print_dot "binary inter" fmt auto;
    assert(A.is_leq auto auto1);
    assert(A.is_leq auto auto2);
    assert(A.is_eq auto1 auto);
    assert(not (A.is_eq auto2 auto));
    assert(not (A.is_leq auto2 auto));

    let auto = A.minimise auto in
    print_dot "binary inter normalized" fmt auto;
    assert(A.is_leq auto auto1);
    assert(A.is_leq auto auto2);
    assert(A.is_eq auto1 auto);
    assert(not (A.is_eq auto2 auto));
    assert(not (A.is_leq auto2 auto));

    let auto = A.union auto1 auto2 in
    print_dot "binary union" fmt auto;
    let auto = A.minimise auto in
    print_dot "binary union normalized" fmt auto;
    assert(A.is_leq auto1 auto);
    assert(A.is_leq auto2 auto);
    assert(A.is_eq auto2 auto);
    assert(not (A.is_eq auto1 auto));
    assert(not (A.is_leq auto auto1));


    let nauto1 = A.minimise (A.Language.plus auto1) in
    let nauto2 = A.minimise (A.Language.plus auto2) in
    assert(A.is_leq nauto1 nauto2);

    let nauto1 = A.minimise (A.Language.star auto1) in
    let nauto2 = A.minimise (A.Language.star auto2) in
    assert(A.is_leq nauto1 nauto2);
    ()

  let main () =
    let file = open_out "essai.dot" in
    let fmt = formatter_of_out_channel file in

    (* Build the language d0^*d5d9 *)
    let word = A.Language.of_regexp ~lattice ~partition
      (Regexp.Exp (Regexp.Concat
	[Regexp.Plus (Regexp.Letter (letter ('d',(5,5))));
	Regexp.Letter (letter ('d',(0,0)));
	Regexp.Letter (letter ('d',(9,9)))]));
    in
    print_dot "word = d0^+d5d9" fmt word;
    let word1 = A.minimise word in
    print_dot "word1 = d0^+d5d9 normalized" fmt word1;

    let word2 = A.inter word word1 in
    print_dot "word2 = word intrer word1" fmt word2;
    let word2 = A.minimise word2 in
    print_dot "word2 normalized" fmt word2;

    (* Build the language (a+b+c)abbcc *)
    let word1 = A.Language.empty ~lattice ~partition in
    let word1 = A.Language.cons_right word1 (letters [('a',(-30,-30));('b',(-20,-20));('c',(-10,-10))]) in
    let word1 = A.Language.cons_right word1 (letter ('a',(-25,-25))) in
    let word1 = A.Language.cons_right word1 (letter ('b',(-15,-15))) in
    let word1 = A.Language.cons_right word1 (letter ('b',(-11,-11))) in
    let word1 = A.Language.cons_right word1 (letter ('c',(-5,-5))) in
    let word1 = A.Language.cons_right word1 (letter ('c',(-1,-1))) in
    print_dot "(a+b+c)abbcc" fmt word1;
    let word1 = A.minimise word1 in
    print_dot "(a+b+c)abbcc" fmt word1;
(*    itself fmt word1; *)

    (* Build the language add(d+e)e(e+f)ggbcc differently *)
    let word2 = A.Language.empty ~lattice ~partition in
    let word2 = A.Language.cons_left word2 (letter ('c',(-8,-8))) in
    let word2 = A.Language.cons_left word2 (letter ('c',(-2,-2))) in
    let word2 = A.Language.cons_left word2 (letter ('b',(-13,-13))) in
    let word2 = A.Language.cons_left word2 (letter ('g',(35,35))) in
    let word2 = A.Language.cons_left word2 (letter ('g',(30,30))) in
    let word2 = A.Language.cons_left word2 (letters [('e',(15,15));('f',(20,20))]) in
    let word2 = A.Language.cons_left word2 (letters [('d',(8,8));('e',(10,10))]) in
    let word2 = A.Language.cons_left word2 (letter ('d',(5,5))) in
    let word2 = A.Language.cons_left word2 (letter ('d',(0,0))) in
    let word2 = A.Language.cons_left word2 (letter ('a',(-29,-29))) in
    print_dot "add(d+e)e(e+f)ggbcc" fmt word2;
(*    itself fmt word2; *)

    let word3 = A.union word1 word2 in
    print_dot "(a+b+c)abbcc+add(d+e)e(e+f)ggbcc" fmt word3;
    let word3 = A.minimise word3 in
    print_dot "(a+b+c)abbcc+add(d+e)e(e+f)ggbcc normalized" fmt word3;
    itself fmt word3;
    binary fmt word2 word3;
    binary fmt word1 word3;

    let word4 = A.union (A.Language.plus word1) (A.Language.plus word2) in
    print_dot "((a+b+c)abbcc)^++(add(d+e)e(e+f)ggbcc)^+" fmt word4;
    let word4 = A.minimise word4 in
    print_dot "((a+b+c)abbcc)^++(add(d+e)e(e+f)ggbcc)^+ normalized" fmt word4;
    itself fmt word4;

    close_out file;
    ()

end



let _ = Finite.main2()
