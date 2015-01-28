(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (string, string_list) =
    let fun helper(string, string_list) = 
	    case string_list of
		[]  => []
		   | xs :: ys => if same_string(string, xs)
				 then helper(string, ys)
				 else xs :: helper(string, ys)
    in
	if string_list = helper(string, string_list)
	then NONE
	else SOME (helper(string, string_list))
    end

fun get_substitutions1 (string_list_list, string) =
    case string_list_list of
	[] => []
      | x :: xs => (case all_except_option(string, x) of
		      NONE => get_substitutions1(xs, string)
		    | SOME lst => lst @ get_substitutions1(xs, string))

fun get_substitutions2 (string_list_list, string) = 
    let fun helper(string_list_list, string) = 
	    case string_list_list of
		[] => []
	      | x :: xs => (case all_except_option(string, x) of
			       NONE => helper(xs, string)
			     | SOME lst => lst @ helper(xs, string))
    in
	helper(string_list_list, string)
    end


fun similar_names (string_list_list, full_name) =
    case full_name of {first=f,middle=m,last=l} =>
		      let
			  fun helper z = 
			      case z of 
				  x :: [] => {first=x,middle=m,last=l} :: []
				| x :: xs' => {first=x,middle=m,last=l} :: helper(xs')
		      in
			  (case get_substitutions1(string_list_list,f) of
			      [] => []
			    | y :: ys' => helper([y]) @ helper(ys')) @ [full_name]
		      end
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)


datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color card = 
    case card of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red

fun card_value card = 
    case card of
	(_, Jack) => 10
      | (_, Queen) => 10
      | (_, King) => 10
      | (_, Ace) => 11
      | (_, Num i) => i

fun remove_card (cs : card list, c : card, e) = 
    case cs of 
	[] => raise e
      | x :: xs' => (if x = c then [] @ xs'
		     else x :: remove_card(xs',c,e))
				 
fun all_same_color card_list = 
    case card_list of
	[] => true
	   | x :: [] => true
	   | head :: (neck :: rest) => (card_color head = card_color neck andalso all_same_color (neck :: rest))

fun sum_cards card_list = 
    case card_list of
	[] => 0
      | x :: [] => card_value x
      | x :: xs' => card_value x + sum_cards xs'

fun score (card_list, goal) = 
    let fun score_calc (card_list, goal) =
	    if sum_cards card_list > goal
	    then 3 * (sum_cards card_list - goal)
	    else goal - sum_cards card_list
    in
	case all_same_color card_list of false  => score_calc (card_list, goal)
			     | true => score_calc (card_list, goal) div 2
    end

fun officiate (card_list, move_list, goal) = 
    let fun helper (cards, moves, held_list) =
	    case moves of
		[] => score(held_list, goal)
	      | x :: xs' => case x of
				Discard d => helper(cards, xs', remove_card(held_list, d, IllegalMove))
			      | Draw => case cards of
					    [] => score(held_list, goal)
					  | y :: ys' => if sum_cards ( y :: held_list) > goal
							then score(y :: held_list, goal)
							else helper(ys', xs', y :: held_list)
    in
	helper(card_list, move_list, [])
    end
	
