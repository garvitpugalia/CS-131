(* Creating acceptor for the grammar matching *)

let accept_all derivation string =
match string with
| [] -> Some (derivation, string)
| _ -> None;;

(* Declaring the nonterminal symbols of the grammar *)

type boolean_logic =
| Equation | Variable | Expression | Operator

let boolean_grammar =
(Equation,
 function
 | Variable -> [[T "x"];[T "y"];[T "z"]]
 | Equation -> [[N Variable; T "="; N Expression]]
 | Expression -> [[T "("; N Expression; T ")"]; [T "("; N Expression; T ")"; N Operator; N Expression]; [N Variable]; [N Variable; N Operator; N Expression]]
 | Operator -> [[T "="]; [T "|"]; [T "^"]; [T "&"]]
  )

(* Creating test cases based on boolean_grammar *)

let test_1 = ((parse_prefix boolean_grammar accept_all ["x"; "="; "z"; "^"; "("; "y"; "|"; "z"; ")"]) =
Some ([(Equation, [N Variable; T "="; N Expression]);
(Variable, [T "x"]); (Expression, [N Variable; N Operator; N Expression]);
(Variable, [T "z"]); (Operator, [T "^"]);
(Expression, [T "("; N Expression; T ")"]);
(Expression, [N Variable; N Operator; N Expression]); (Variable, [T "y"]); (Operator, [T "|"]);
(Expression, [N Variable]); (Variable, [T "z"])], []));;

let test_2 = ((parse_prefix boolean_grammar accept_all ["z"; "="; "("; "("; "x"; "|"; "y"; ")"; "&"; "("; "x"; "^"; "y"; ")"; "&"; "y"; ")"; "|"; "x"]) =
Some ([(Equation, [N Variable; T "="; N Expression]);
(Variable, [T "z"]); (Expression, [T "("; N Expression; T ")"; N Operator; N Expression]);
(Expression, [T "("; N Expression; T ")"; N Operator; N Expression]);
(Expression, [N Variable; N Operator; N Expression]); (Variable, [T "x"]); (Operator, [T "|"]);
(Expression, [N Variable]); (Variable, [T "y"]); (Operator, [T "&"]);
(Expression, [T "("; N Expression; T ")"; N Operator; N Expression]);
(Expression, [N Variable; N Operator; N Expression]); (Variable, [T "x"]); (Operator, [T "^"]);
(Expression, [N Variable]); (Variable, [T "y"]);
(Operator, [T "&"]); (Expression, [N Variable]); (Variable, [T "y"]);
(Operator, [T "|"]); (Expression, [N Variable]); (Variable, [T "x"])], []));;

let test_3 = ((parse_prefix boolean_grammar accept_all ["y"; "="; "("; "x"; "^"; "z"; ")"; "&"; "("; "p"; "$"; "z"; ")"]) = None)
