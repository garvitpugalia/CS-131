open List;;
open Pervasives;;

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* ------------------------------------------------------------------------------------------------------------------------------------------- *)

(* Write a function convert_grammar gram1 that returns a Homework 2-style grammar, which is converted from the Homework 1-style grammar gram1. *)

let rec convert_grammar gram1 =
((fst gram1), fun index -> (List.map (fun (i,j) -> j)
(List.filter (fun (i, j) -> index = i) (snd gram1))));;

(* ------------------------------------------------------------------------------------------------------------------------------------------- *)

(* Write a function parse_prefix gram that returns a matcher for the grammar gram. When applied to an acceptor accept and a fragment frag, the matcher must return the first acceptable match of a prefix of frag, by trying the grammar rules in order *)

let rec parse_prefix gram accept frag =
let list_alts = (snd gram) (fst gram) in

(* Helper function to avoid retyping of same matching code *)
let empty_list lt = match lt with
| _::_ -> false
| [] -> true in

(* Helper function to find the best matching out of all possibilities *)
  let rec alt_find atl = match atl with
    | [] -> None
    | h::t ->

(* Helper function to traverse through possible tokens in the given grammar *)
      let rec trav_token matching temp token_lt = match token_lt with
        | [] -> accept matching temp
        | element::remaining_list -> match element with 
            | T x -> if (not (empty_list temp) && ((List.hd temp) = x)) then trav_token matching (List.tl temp) remaining_list else None
            | N y ->

(* Helper function to check remaining possible matchings *)
              let rec matching_left derive_rmn frag_rmn = trav_token (List.append matching derive_rmn) frag_rmn remaining_list in
parse_prefix (y, (snd gram)) matching_left temp in

(* Create final Some constructor to return *) 
      let final_matching = trav_token [((fst gram), h)] frag h in
match final_matching with
| Some(derive_rmn,s) -> final_matching
| _ -> alt_find t in alt_find list_alts;;
