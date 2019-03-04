let my_subset_test0 = subset [1] [5; 4; 3; 1; 2]
let my_subset_test1 = not (subset [1] [5; 4; 3; 2])
let my_subset_test2 = subset [1; 3; 2] [5; 4; 3; 1; 2]
let my_subset_test3 = not (subset [1; 6; 3] [5; 4; 3; 1; 2])
let my_subset_test4 = subset [] [5; 4]

let my_equal_sets_test0 = equal_sets [13; 14; 5; 5; 6; 6] [13; 5; 6; 14]
let my_equal_sets_test1 = not (equal_sets [1;2;2;3] [3;2;4])
let my_equal_sets_test2 = equal_sets [] []
					  
let my_set_union_test0 = equal_sets (set_union [1;3;5;5;7] [2;4;1;1;4;5;6]) [1;2;3;4;5;6;7]
let my_set_union_test1 = equal_sets (set_union [1] [4;4;4;4;4;4;4;4;4;4;4]) [1;4]

let my_set_intersection_test0 = equal_sets (set_intersection [1;2;3;4] [5;6]) [] 
let my_set_intersection_test1 = equal_sets (set_intersection [1;4;5;5;5;3] [4;5;5;2]) [4;5]

let my_set_diff_test0 = equal_sets (set_diff [3;6] [3;3;3;6;6]) []
let my_set_diff_test1 = equal_sets (set_diff [1;2;3] [1;2]) [3]
let my_set_diff_test2 = equal_sets (set_diff [] [2]) []
let my_set_diff_test3 = equal_sets (set_diff [1;1;2;3;4;4;4;4] []) [1;2;3;4]
					     
let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun y -> y) 10 = 10
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> 15) 30 = 15
let my_computed_fixed_point_test2 = computed_fixed_point (=) (fun z -> z *. 10.) 2. = infinity

let my_computed_periodic_point_test0 = computed_periodic_point (=) (fun y -> y) 10 10 = 10
let my_computed_periodic_point_test1 = computed_periodic_point (=) (fun x -> x / 5) 0 4 = 4

let my_while_away_test0 = while_away (fun z -> z / 2) ((<) 2) 1 = []
let my_while_away_test1 = while_away (fun x -> x + 10) ((>) 30) 5 = [5; 15; 25]

let my_rle_decode_test0 = rle_decode [5,1; 1,7; 2,8; 2,2] = [1; 1; 1; 1; 1; 7; 8; 8; 2; 2]
let my_rle_decode_test2 = rle_decode [1, "g"; 1, "r"; 4, "o"; 1, "t"] = ["g"; "r"; "o"; "o"; "o"; "o"; "t"]

type nonterminals = | ST | This | Is | Great

let good_rules =
[ST, [N Great]; Great, [N Is]; Is, [N This]; This, [T "Reversed"]]

let good_grammar = ST, good_rules

let my_filter_blind_alleys_test0 = filter_blind_alleys good_grammar = good_grammar

let bad_rules = 
[ST, [T "Normal"; T "Original"; N Great]; Great, [N ST]; This, [T "Used"]]

let bad_grammar = ST, bad_rules

let my_filter_blind_alleys_test1 = filter_blind_alleys bad_grammar = (ST, [This, [T "Used"]])
