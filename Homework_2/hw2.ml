(* Type Declarations *)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* Problem 1 *)

let rec alt_list rules_list nonterm = match rules_list with
  | [] -> []
  | head::tail -> 
      if (fst head) = nonterm then (snd head)::(alt_list tail nonterm)
      else alt_list tail nonterm

let convert_grammar gram1 = 
  ((fst gram1), alt_list (snd gram1))


(* Problem 2 *)

let rec ptl_helper tree_list = match tree_list with
  | [] -> []
  | head::tail -> match head with
    | Node (_, sub_tree) -> (ptl_helper sub_tree)@(ptl_helper tail)
    | Leaf term -> [term]@(ptl_helper tail)
  
let parse_tree_leaves tree = 
  ptl_helper [tree]


(* Problem 3 *)

(* This function is used to compute how many nonterminal symbols
  a given rule has *)
let rec nonterm_counter rule =  
  match rule with
  | [] -> 0
  | head::tail ->
      match head with
      | N _ ->
          1000 + nonterm_counter tail
      | T _ ->
          1 + nonterm_counter tail
	     (* We prioritize nonterminal symbols in a given rule,
               but we use terminal symbols as tiebreakers *)
            
(* This function serves as the comparison function necessary for
  List.sort and uses nonterm_counter to generate values*)
let compar_fun x y =
  if (nonterm_counter x) < (nonterm_counter y) then 1
  else -1

(* This function was added to ensure more complex rules are
  searched first *)
let sort_rules rules =
  List.sort compar_fun rules 
    
(* This function is crucial in that it allows us to give a number
  in the return tuple that keeps track of the fragment *)
let rec where_in_frag num frag = 
  match frag with
  | [] -> []
  | _ ->
      match num with
      | 0 -> frag
      | _ -> where_in_frag (num - 1) (List.tl frag)
            
(* These two mutually recursive functions are where all the work
  gets done to create the matcher: the first (search_list) iterates 
  through a list of rules for a given nonterminal symbol, and the 
  second (search_rule) iterates through the symbols within a given 
  rule *)
(* prod_fun, accept, and frag will never change throughout *)
(* The 'num' argument in both functions keeps track of how many 
  of the symbols in frag have been successfully matched*)
let rec search_list prod_fun sym_rules num accept frag =
  match sym_rules with
  | [] -> (num, None)
     (* If sym_rules is empty we have run out of rules, so we return with None*)
  | head::tail ->
      let first_rule = search_rule prod_fun head num accept frag in
	(* Try to find an acceptable frag for the first rule *)
      match first_rule with
      | (n, x) ->
          if n = num then
	    (* If n=num, that means the first rule did not fully match frag *)
            match x with
            | None -> search_list prod_fun tail num accept frag
	      (* Try the next rule in the list of rules recursively *)
            | Some _ -> first_rule 
          else first_rule
            (* If n is not num, we have found a rule to match the frag*)
and search_rule prod_fun sym_list num accept frag =
  match sym_list with
  | [] -> (num, (accept (where_in_frag num frag))) 
     (* An empty sym_list indicates we have searched through the rule, so we test frag*)
  | head::tail ->
      let new_frag = (where_in_frag num frag) in
	(* new_frag contains all the elements of frag we have yet to match *)
      match new_frag with
      | [] -> (num, None)
	(* If we have matched all elements of frag but still have rules, we return None*)
      | _ -> 
          match head with
          | N x ->
              let firstn_res = search_list prod_fun (sort_rules (prod_fun x)) num accept frag in
		 (* If we have a nonterminal symbol, we iterate through its rules *)
              (
                match firstn_res with
                | (n, y) ->
                    if n = num then
                      (n, None)
			(* If n=num, nothing new was matched in frag so we leave this rule *)
                    else
                      match tail with
                      | [] -> firstn_res
			(* If tail is empty, we matched part of frag for the whole rule *)
                      | _ -> 
                          let rest_res = search_rule prod_fun tail n accept frag in
			    (* Search rest of the symbols in list recursively*)
                          match rest_res with
                          | (k, _) ->
                              if k = n then (num, None)
				(* If k=n, we found no new matching symbols in frag *)
                              else rest_res 
              )
          | T x ->
              if (List.hd new_frag) = x then
                search_rule prod_fun tail (num + 1) accept frag
		  (* If terminal symbol matches, search rest of symbols in list recursively
                     and add 1 to the overall count of frag elements matched*)
              else (num, None) 
		  (* Else, this rule doesn't match so we return with None*)

let matcher_helper prod_fun sym_rules accept frag =
  let res = search_list prod_fun (sort_rules sym_rules) 0 accept frag in
    (* We call search_list function with 0 because we have matched 0 elements of frag *)
  match res with
  | (_, x) -> x
    (* The 'num' is extremely useful throughout, but no so much for the solution *)
            
let make_matcher gram =
  matcher_helper (snd gram) ((snd gram) (fst gram))
  

(* Problem 4 *)


(* Similarly, the bulk of the work will be done in these two mutually
  recursive functions.  Its structure is based on my solution for
  make_matcher *)
(* current_node is the name of the nonterminal symbol whose rules we
  are investigating *)
(* parse_rule does not need current_node because it is simply searching
  for the list to accompany current_node *)
let rec parse_list prod_fun sym_rules num current_node frag =
  match sym_rules with
  | [] -> (num, None)
  | head::tail ->
      let first_rule = parse_rule prod_fun head num frag in
      match first_rule with
      | (n, x) -> 
          if n = num then
            parse_list prod_fun tail num current_node frag 
          else 
            match x with
            | Some y -> (n, (Some (Node (current_node, y)))) 
	      (* parse_rule will return a value of (n, Some List) the list
		will be the list the Node takes, so we add Some Node
		current_node in front of the List *)
            | None -> parse_list prod_fun tail num current_node frag
	      (* This should never happen, but OCaml was yelling at me *)
                        
and parse_rule prod_fun sym_list num frag =
  match sym_list with
  | [] -> (num, None)
  | head::tail ->
      let new_frag = (where_in_frag num frag) in
      match new_frag with
      | [] -> (num, None)
      | _ ->
          match head with
          | N x ->
              (
                let firstn_res = parse_list prod_fun (sort_rules (prod_fun x)) num x frag in
                match firstn_res with
                | (n, y) ->
                    if n = num then (num, None)
                    else
                      match tail with
                      | [] -> 
                          (
                            match y with
                            | Some z -> (n, Some [z])
			      (* Necessary because we need to put 'z' in a list *)
                            | None -> (n, None)
                          )
                      | _ ->
                          let rest_res = parse_rule prod_fun tail n frag in
			    (* Resursively search the rest of the symbols in the rule *)
                          match rest_res with
                          | (k, j) ->
                              if k = n then (num, None)
                              else 
                                match j with
                                | Some l ->
                                    (                      
                                      match y with
                                      | Some z -> (k, Some (z::l))
					(* Add res of first symbol to front of list *)
                                      | _ -> (num, None)
					(* Should never happen *)
                                    )
                                | _ -> (num, None)
				   (* Again, should never happen *)
              )              
          | T x -> 
              if (List.hd new_frag) = x then
                let new_count = num + 1 in
                match tail with
                | [] -> (new_count, Some [Leaf x])
		  (* If tail is empty, we matched all the symbols in the given rule *)
                | _ -> 
                    let rem_res = parse_rule prod_fun tail new_count frag in
		      (* If not, recursively search through the rest of the list *)
                    match rem_res with
                    | (n, y) ->
                        if n = new_count then (num, None)
                        else
                          match y with
                          | Some z -> (n, Some ((Leaf x)::z))
			    (* Add symbol x to the front of the matched list z *)
                          | _ -> (num, None)
              else (num, None) 

(* parser_helper will always correctly determine whether a parse can be found because
  we always sort our sym_rules in order of decreasing complexity *)
let rec parser_helper prod_fun sym_rules starting_sym frag = 
  match sym_rules with
  | [] -> None
  | _ -> 
    let res = parse_list prod_fun (sort_rules sym_rules) 0 starting_sym frag in
    match res with
    | (n, y) ->
        let test_empty = where_in_frag n frag in
        match test_empty with
        | [] -> y
           (* If the empty list is matched, then we have successfully found a parse
             for the entirety of the frag *)
        | _ -> parser_helper prod_fun (List.tl sym_rules) starting_sym frag
           (* A nonempty frag means at best we only matched part of the frag, so we
             iterate through the list without the head recursively to make sure we
             matched the longest frag if it exists *)
            
let make_parser gram = 
  parser_helper (snd gram) ((snd gram) (fst gram)) (fst gram)
    

