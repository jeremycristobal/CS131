type melee_nonterminals =
  | DashDance | Wavedash | Shine | Jump | Run | Grab
  
let melee_grammar = 
  (DashDance,
   function
   | DashDance ->
       [ [N Shine];
         [N Wavedash; T "Shield"];
         [N Wavedash];
         [N Jump; N Run]; 
         [T "Dash Attack"];
         [N Run];
         [T "Stop"; T "Up Smash"; N DashDance]]
   | Wavedash -> 
       [[T "Up Smash"; T "Shield"];
        [T "Shield"; N Grab]]
   | Shine -> 
       [[T "Shield"; N Grab];
        [T "Up Smash"; N Run];
        [N Wavedash]]
   | Grab ->
       [[T "Up Throw"; N Jump];
        [T "Down Throw"];
        [T "Back Throw"; N Run; T "Up Smash"]]
   | Jump ->
       [[T "Arial"; T "Land"];
        [T "Land"]]
   | Run ->
       [[T "Stop"];
        [T "Shield"];
        [N Jump]]
  )
  
let accept_all string = Some string

let ex_melee_frag = ["Shield"; "Back Throw"; "Arial"; "Land"; "Up Smash"; "Shield"]          

let make_matcher_test = 
  ((make_matcher melee_grammar accept_all ex_melee_frag
   ) = Some[]) 

let make_parser_test1 = 
  ((make_parser melee_grammar ex_melee_frag) = 
   Some (Node (DashDance, [Node (Wavedash, [Leaf "Shield"; Node (Grab, [Leaf "Back Throw"; Node (Run, [Node (Jump, [Leaf "Arial"; Leaf "Land"])]); Leaf "Up Smash"])]); Leaf "Shield"])))
      
let make_parser_test2 = 
  ((parse_tree_leaves (Node (DashDance, [Node (Wavedash, [Leaf "Shield"; Node (Grab, [Leaf "Back Throw"; Node (Run, [Node (Jump, [Leaf "Arial"; Leaf "Land"])]); Leaf "Up Smash"])]); Leaf "Shield"])))
   = ex_melee_frag)
