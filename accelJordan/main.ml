
open Format


let inputfilename = ref ""

let apron_interval_compare tab itv1 itv2 =
  Apron.(
    Interval.(
      if is_bottom itv1 && not (is_bottom itv2) then
	tab.(2) <- tab.(2) - 0
      else if is_bottom itv2 && not (is_bottom itv1) then
	tab.(2) <- tab.(2) + 0
      else begin
	if Scalar.is_infty itv1.inf <>0 then begin
	  if not (Scalar.is_infty itv2.inf <> 0) then
	    tab.(0) <- tab.(0)+1;
	end else begin
	  if Scalar.is_infty itv2.inf <> 0 then
	    tab.(1) <- tab.(1)-1
	  else begin
	    let cmp = Scalar.cmp itv1.inf itv2.inf in
	    if cmp<0 then tab.(2) <- tab.(2) + 1
	    else if cmp>0 then tab.(3) <- tab.(3) - 1
	  end
	end;
	if Scalar.is_infty itv1.sup <> 0 then begin
	  if not (Scalar.is_infty itv2.sup <> 0) then
	    tab.(0) <- tab.(0)+1;
	end else begin
	  if Scalar.is_infty itv2.sup <> 0 then
	    tab.(1) <- tab.(1)-1
	  else begin
	    let cmp = Scalar.cmp itv1.sup itv2.sup in
	    if cmp>0 then tab.(2) <- tab.(2) + 1
	    else if cmp<0 then tab.(3) <- tab.(3) - 1
	  end
	end
      end
    )
  )

let apron_box_compare tab box1 box2 =
  assert( (Array.length box1)=(Array.length box2) );
  for i=0 to (Array.length box1) - 1 do
    apron_interval_compare tab box1.(i) box2.(i)
  done;
  ()

let apron_box_count tab box =
  Apron.(
    Interval.(
      Array.iter
	(begin fun itv ->
	  if Scalar.is_infty itv.inf <> 0 then tab.(0) <- tab.(0)+1;
	  if Scalar.is_infty itv.sup <> 0 then tab.(0) <- tab.(0)+1;
	  tab.(1) <- tab.(1)+2
	 end)
	box
    )
  )

let parse () =
  (* Parsing the command line *)
  Arg2.parse Option.t;
  (* Parsing the program *)
  let input = open_in !Option.inputfilename in
  let lexbuf = Lexing.from_channel input in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with
      Lexing.pos_fname = !inputfilename;
    };

  let prog = begin
    try
      Parser.auto Lexer.token lexbuf
    with
    | Parsing.Parse_error ->
	let startp = Lexing.lexeme_start_p lexbuf
	and endp = Lexing.lexeme_end_p lexbuf
	in
	Format.fprintf err_formatter
	  "Syntaxical error: %s, line %i, characters %i-%i: '%s'.@."
	  startp.Lexing.pos_fname
	  startp.Lexing.pos_lnum
	  (startp.Lexing.pos_cnum - startp.Lexing.pos_bol)
	  (endp.Lexing.pos_cnum - endp.Lexing.pos_bol)
	  (Lexing.lexeme lexbuf);
	raise Exit;
    | Syntax.Error str ->
	let startp = Lexing.lexeme_start_p lexbuf
	and endp = Lexing.lexeme_end_p lexbuf
	in
	Format.fprintf err_formatter "Lexical error: %s, line %i, character %i-%i: '%s': %s@."
	  startp.Lexing.pos_fname
	  startp.Lexing.pos_lnum
	  (startp.Lexing.pos_cnum - startp.Lexing.pos_bol)
	  (endp.Lexing.pos_cnum - endp.Lexing.pos_bol)
	  (Lexing.lexeme lexbuf)
	  str;
	raise Exit
    | _ as exn ->
	let startp = Lexing.lexeme_start_p lexbuf
	and endp = Lexing.lexeme_end_p lexbuf
	in
	Format.fprintf err_formatter "Problem here: %s, line %i, character %i-%i: '%s'@."
	  startp.Lexing.pos_fname
	  startp.Lexing.pos_lnum
	  (startp.Lexing.pos_cnum - startp.Lexing.pos_bol)
	  (endp.Lexing.pos_cnum - endp.Lexing.pos_bol)
	  (Lexing.lexeme lexbuf);
	raise exn
  end
  in
  close_in input;
  (* Computing solution *)
  prog

let _ =
  let prog = parse () in
  let apron = Polka.manager_alloc_loose () in
  let auto = Auto.of_syntax ~logtemplate:!Option.logtemplate ~apron prog in
  let jauto = Auto.jgraph_of_graph auto in
  let joutput = Auto.analysis ~apron jauto in
  if !Option.comparison then begin
    let sauto = Auto.sgraph_of_graph auto in
    let soutput = Auto.analysis ~apron sauto in
    let dauto = Auto.dgraph_of_graph ~apron auto in
    let doutput = Auto.analysis ~apron dauto in
    if !Option.debug>=1 then begin
      let stab = Array.make 4 0 and dtab = Array.make 4 0 in
      let scount = Array.make 2 0 and dcount = Array.make 2 0 in
      PSHGraph.iter_vertex auto
	(begin fun vertex _ ~pred ~succ ->
	  if vertex<>"linit" then begin
	    let jabs = PSHGraph.attrvertex joutput vertex in
	    let sabs = PSHGraph.attrvertex soutput vertex in
	    let dabs = PSHGraph.attrvertex doutput vertex in
	    let jbox = Apron.Abstract0.to_box apron jabs in
	    let sbox = Apron.Abstract0.to_box apron sabs in
	    let dbox = Apron.Abstract0.to_box apron dabs in
	    apron_box_count scount sbox;
	    apron_box_count dcount dbox;
	    apron_box_compare stab sbox jbox;
	    apron_box_compare dtab dbox jbox;
	    ()
	  end
	 end)
      ;
      printf "scount=%a@." (Print.array pp_print_int) scount;
      printf "stab=%a@." (Print.array pp_print_int) stab;
      printf "dcount=%a@." (Print.array pp_print_int) dcount;
      printf "dtab=%a@." (Print.array pp_print_int) dtab;
      ()
    end
  end;
  ()
