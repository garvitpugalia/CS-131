open List;;

(* Check if a is a subset of b *)
let rec subset a b =
match a with
| [] -> true
| h::t -> let equal_to_head x = ((compare x h) = 0) in
List.exists equal_to_head b && subset t b;;

(* Check if two sets are equal *)
let equal_sets a b = subset a b && subset b a;;

(* Return the union of two lists *)
let set_union a b =
let union_with_duplicate = append a b in
List.sort_uniq compare union_with_duplicate;;

(* Return the intersection of two lists *)
let rec set_intersection a b =
match a with
| [] -> []
| h::t -> let equal_to_head x = ((compare x h) = 0) in
if List.exists equal_to_head b && not (List.exists equal_to_head t)
then [h]@(set_intersection t b)
else set_intersection t b;;

(* Return the difference of two lists a - b *)

let rec set_diff a b =
match a with
| [] -> []
| h::t -> let equal_to_head x = ((compare x h) = 0) in
if List.exists equal_to_head b
then set_diff t b
else [h]@(set_diff t b);;

(* --------------------------------------------------------------- *)

(* Returns the computed fixed point for f with respect to x *)

let rec computed_fixed_point eq f x =
if eq (f x) x
then x
else computed_fixed_point eq f (f x);;

(* Returns the computed periodic point for f with respect to x and period p *)

let rec h_computed_periodic_point eq f p x xs =
match p with
| 0 -> if eq x xs then x
else let next_f = f xs in let next_x = f x in
h_computed_periodic_point eq f p next_x next_f
| _ -> let next_p = p - 1 in let next_f = f xs in
h_computed_periodic_point eq f next_p x next_f;;

let computed_periodic_point eq f p x = h_computed_periodic_point eq f p x x;;

(* --------------------------------------------------------------- *)

(* Return longest list till predicate is true *)

let rec while_away s p x =
let next_s = s x in
if p x
then [x]@(while_away s p next_s)
else [];;

(* Decodes a list of pairs in run-length encoding *)

let rec rle_decode lp =
match lp with
| [] -> []
| h::t ->
let num, value = h in
if num = 0
then rle_decode t
else value::rle_decode ((num-1,value)::t);;

(* --------------------------------------------------------------- *)

type ('nonterminal, 'terminal) symbol =
      | N of 'nonterminal
      | T of 'terminal

(* Filter blind alley rules from grammar g *)

let rec filter_blind_alleys g =
let first, list_of_rules = g in

(* Helper function to check and remove terminals from a list *)
let rec helper_T temp is_over =
let check_T_value node =
let _, list_of_dp = node in
List.for_all (fun s -> match s with
| T x -> true
| N y -> subset [y] is_over) list_of_dp in

(* Match with terminal or non-terminal *)

(* If list empty, return empty list else keep repeating till terminals *)

match temp with
| [] -> []
| h::t -> if check_T_value h
then helper_T t is_over
else [h]@(helper_T t is_over) in

(* Function defined to remove terminals from list of rules *)

let rec check_T visited =
let temp, is_over = visited in
let empty_rules = helper_T temp is_over in
let visited_new, _ = List.split (set_diff temp empty_rules) in
empty_rules, (set_union is_over visited_new) in
let ba_rules, _ = computed_fixed_point (fun tuple1 tuple2 ->
let x, _ = tuple1 in let y, _ = tuple2 in List.length y == List.length x) check_T (list_of_rules,[]) in
let non_ba_rules node = List.for_all (fun curr -> curr != node) ba_rules in
let temp = List.filter non_ba_rules list_of_rules in first, temp;;
