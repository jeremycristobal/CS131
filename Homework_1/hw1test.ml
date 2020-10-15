let my_subset_test0 = subset [] [1; 2; 3]
let my_subset_test1 = subset [1; 1; 2; 3] [1; 2; 3]
let my_subset_test2 = not (subset [1; 2; 3; 4] [1; 2; 3])

let my_equal_sets_test0 = equal_sets [1; 2; 3] [3; 2; 1]
let my_equal_sets_test1 = not (equal_sets [1; 2; 3] [1; 2 ;3; 4])

let my_set_union_test0 = equal_sets (set_union [1; 2; 3] [2; 3; 4]) [1; 2; 3; 4]
let my_set_union_test1 = equal_sets (set_union [1; 2; 3] [2; 2; 2]) [1; 2; 3]

let my_set_intersection_test0 = equal_sets (set_intersection [1; 2; 3] [2; 3; 4]) [2; 3]
let my_set_intersection_test1 = equal_sets (set_intersection [1; 2; 3] [4; 5; 6]) []

let my_set_diff_test0 = equal_sets (set_diff [1; 2; 3] [1; 2; 3]) []
let my_set_diff_test1 = equal_sets (set_diff [1; 2; 3; 4] [1;3]) [2; 4]
let my_set_diff_test2 = equal_sets (set_diff [3; 1; 2] []) [1; 2; 3]
    
let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x / 3) 3333 = 0 
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x *. 1.01) 1. = infinity
  
type basketball_nonterminals =
  | Dribble | Shoot | Dunk | Pass | Cut
    
let basketball_grammar = 
  Dribble, 
  [ Dribble, [N Shoot];
    Dribble, [N Dunk];
    Dribble, [N Pass];
    Dribble, [T "Steal"];
    Shoot, [T "Miss"];
    Shoot, [T "Make"];
    Dunk, [T "Miss"];
    Dunk, [T "Make"];
    Pass, [T "Screen"; N Cut];
    Pass, [N Cut];
    Pass, [T "Call"; T "Catch"; N Shoot];
    Pass, [T "Call"; T "Catch"; N Dunk];
    Cut, [T "Catch"; N Shoot]]
  
let my_filter_reachable_test0 = 
  filter_reachable basketball_grammar = basketball_grammar
let my_filter_reachable_test1 = 
  filter_reachable (Pass, snd basketball_grammar) = 
  (Pass, [Shoot, [T "Miss"];
          Shoot, [T "Make"];
          Dunk, [T "Miss"];
          Dunk, [T "Make"];
          Pass, [T "Screen"; N Cut];
          Pass, [N Cut];
          Pass, [T "Call"; T "Catch"; N Shoot];
          Pass, [T "Call"; T "Catch"; N Dunk];
          Cut, [T "Catch"; N Shoot]])
let my_filter_reachable_test2 = 
  filter_reachable (Dunk, snd basketball_grammar) =
  (Dunk, [Dunk, [T "Miss"];
          Dunk, [T "Make"]])
