(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* assignment 1a  *)
fun all_except_option (str : string, strlst : string list) =
    case strlst of
	[] =>  NONE
      | x::xs' =>
	if same_string(str, x)
	then SOME xs'
	else
	    case all_except_option(str, xs') of
		NONE => NONE
	      | SOME elmnt => SOME(x::elmnt)

(* assignment 1b *)
fun get_substitutions1 (twod_lst : string list list, str : string) =
    case twod_lst of
	[] => []
      | x::xs' =>
	case all_except_option(str, x) of
	    NONE => get_substitutions1(xs', str)
	  | SOME elmnt => elmnt @ get_substitutions1(xs', str)
						    
(* assignment 1c *)
fun get_substitutions2 (twod_lst : string list list, str : string) =
    let
	fun helper (twod_lst : string list list, str : string, acc : string list) =
	    case twod_lst of
		[] => acc
	      | x::xs' =>
		case all_except_option(str, x) of
		    NONE => helper(xs', str, acc)
		 | SOME elmnt => helper(xs', str, acc @ elmnt)
    in
	helper(twod_lst, str, [])
    end

(* assignment 1d  *)
fun similar_names (twod_lst : string list list, name : {first : string, middle : string, last : string}) =
    let
	fun helper(first_lst, middle, last, acc) =
	    case first_lst of
		[] => acc
		   | x::xs' => helper(xs', middle, last, {first=x, middle=middle, last=last} :: acc)
    in
	case name of
	    {first, middle, last} => helper( get_substitutions2(twod_lst, first) @ [first], middle, last, [])
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

(* assignment 2a *)
fun card_color (c : card) =
    case c of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | _ => Red

(* assignment 2b *)
fun card_value (v : card) =
    case v of
	(_, Ace) => 11
      | (_, Num i) => i
      | _ => 10

(* assignment 2c *)		 
fun remove_card (cs : card list, c : card, e : exn) =
    case cs of
	[] => raise e
      | x::xs' => if x = c
		  then xs'
		  else x :: remove_card(xs', c, e)
				       
(* assignment 2d *)		      
fun all_same_color (cs : card list) =
    case cs of
	card1::card2::card3andso => if card_color(card1) = card_color(card2)
					 then all_same_color(card2 :: card3andso)
					 else false
     | _ => true
					     
					     
(* assignment 2e *)
fun sum_cards (cs : card list) =
    let fun helper (cs, acc) =
	    case cs of
		[] => acc
	      | x::xs' => helper(xs', card_value(x) + acc)
    in
	helper(cs, 0)
    end
	

(* assignment 2f *)
fun score (cs : card list, goal : int) =
    let val sum = sum_cards(cs)

	val is_same = all_same_color(cs)
    in
	case (sum > goal, is_same) of
	    (true, true) => (3 * (sum - goal)) div 2
	  | (true, false) => 3 * (sum - goal)
	  | (false, true) => (goal - sum) div 2
	  | (false, false) => goal - sum
    end

(* assignment 2g *)
fun officiate (cs : card list, moves : move list, goal : int) =
    let
	fun helper (card_left : card list, move_left : move list, held : card list) =
	    case move_left of
		[] => held
	      | move::moves_left => case move of
					Discard card => helper(card_left, moves_left, remove_card(held, card, IllegalMove))
				      | Draw => case card_left of
						    [] => held
						  | card::_ => case sum_cards(card::held) > goal of
								   true => card::held
								|  false => helper(remove_card(card_left, card, IllegalMove), moves_left, card::held)
								   
					
    in
	score(helper (cs, moves ,[]), goal)
    end
	
	      
	  
	      
