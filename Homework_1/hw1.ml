(* Problem 1 *)
let rec subset a b = match a with
  	| [] -> true
  	| _ -> if List.mem (List.hd a) b then subset (List.tl a) b
      		else false;; 

(* Problem 2 *)
let equal_sets a b =
	if (subset a b) && (subset b a) then true
	else false;;

(* Problem 3 *)
let rec set_union a b = match a with
	| [] -> b
  	| _ -> if List.mem (List.hd a) b then set_union (List.tl a) b
      		else set_union (List.tl a) b@[List.hd a];;

(* Problem 4 *)
let rec set_intersection a b = match a with
  	| [] -> []
  	| _ -> if not (List.mem (List.hd a) b) then set_intersection (List.tl a) b
      		else (List.hd a)::(set_intersection (List.tl a) b);; 

(* Problem 5 *)
let rec set_diff a b = match a with
  	| [] -> []
  	| _ -> if List.mem (List.hd a) b then set_diff (List.tl a) b
      		else (List.hd a)::(set_diff (List.tl a) b);;

(* Problem 6 *)
let rec computed_fixed_point eq f x = 
  	if eq (f x) x then x
  	else computed_fixed_point eq f (f x);;

(* Problem 7 *)
type ('nonterminal, 'terminal) symbol =
  	| N of 'nonterminal
  	| T of 'terminal;;

let rec rules_for a b = match b with
  	| [] -> []
	(* if b is [], we have looked through all of our remaining rules already *)
  	| _ -> if (fst (List.hd b)) = a then (List.hd b)::(rules_for a (List.tl b))
      		else rules_for a (List.tl b);;
	(* if the left hand side of a rule matches our nonterminal symbol a, we add
	the rule to the list we will return*)

let rec filter_term a = match a with 
  	| [] -> []
	(* if a is [], we have iterated through all of our found symbols already *)
  	| N y::x -> (y)::(filter_term x)
	(* if the symbol is nonterminal, we add it to the list we will return *)
  	| T _::x -> filter_term x;;
	(* if the symbol is terminal, we skip it and move on to the next one *)

let rec find_nonterms a b c = match a with
  	| [] -> b
	(* if a is [], then we have iterated through all of the nonterminal symbols
	that are accessible from the starting symbol; we return the rules we didn't use *)
  	| _ -> let a_rules = rules_for (List.hd a) b in 
		(* a_rules = list of rules with a as its left hand side *)
      		let symbols_from_a = snd (List.split a_rules) in
		(* symbols_from_a = list of right hand side of a_rules *)
      		let nonterms = set_diff (filter_term (List.flatten symbols_from_a)) c in
		(* nonterms = list of nonterminal symbols not already stored in c *) 
      		find_nonterms ((List.tl a)@nonterms) (set_diff b a_rules) ((List.hd a)::c);;
		(* We use (List.tl a)@nonterms for the first argument in order to keep track
		of the nonterminal symbols we have not yet found rules for
		   We use set_diff b a_rules for the second argument in order to keep track
		of which rules we have not yet used 
		   We use (List.hd a)::c as the third argument in order to keep track of
		which nonterminal symbols we have already found the rules for *)

let filter_reachable g =
  	let useless_rules = find_nonterms [(fst g)] (snd g) [] in
	(* We call the starting symbol as a list for future ease
	   We use [] for the third argument for the same reason *)
  	(fst g, (set_diff (snd g) useless_rules));;
	(* We take set_diff here because we found all the unused
	rules in useless_rules and we want to return useful rules *)
