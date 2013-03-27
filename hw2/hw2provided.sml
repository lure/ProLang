(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2
	
fun revers xs = (* List.rev was shown in a video but I am not sure I am allowed to use it in homework*)
    let fun aux([], acc) = acc
	  | aux (x::xs', acc) = aux(xs', x::acc)
    in
	aux (xs, [])
    end
	
	
(* put your solutions for problem 1 here *)
(* a *)
fun all_except_option (line, strlist) = 
    let fun aux ([], acc) = NONE
	  | aux(x :: xs', acc) = if line = x
				  then SOME (revers acc @ xs')
				  else aux(xs', x::acc)
    in
	aux(strlist, [])
    end

(* b *)
fun get_substitutions1(subs, s) =
    case subs of
	[] => []
      | x::xs' => case all_except_option(s, x) of
		     SOME lst => lst @ get_substitutions1(xs', s)
		   | NONE =>  get_substitutions1(xs', s)
(* c *)
fun get_substitutions2(subs, s) = 
    let fun aux([], acc) = acc
	  | aux (x::xs', acc) = case all_except_option(s, x) of
				   SOME lst => aux(xs', acc @ lst)
				 | NONE =>  aux(xs', acc)
    in
	aux(subs, [])
    end

(* d *)	
fun similar_names(subs, {first=f, middle=m, last=s}) =
    let fun aux([], acc) = acc
	  | aux (x::xs', acc) = aux(xs', {first=x, middle=m, last=s} :: acc)
    in
	revers(aux(get_substitutions2(subs, f), [{first=f, middle=m, last=s}]))
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

(* a *)
fun card_color (Clubs,_) = Black 
  | card_color (Spades,_) = Black
  | card_color (_, _) = Red

(* b *)
fun card_value (_, Num x) = x
  | card_value (_, Ace) = 11
  | card_value (_, _) = 10

(* c *)
fun remove_card (cxs, card, exc)=
    case all_except_option(card, cxs) of
	NONE => raise exc
     | SOME xs' => xs'

(* d 
fun all_same_color [] = true
  | all_same_color (x::[]) = true
  | all_same_color (head::(neck::rest)) = let 
      val (s, r) = head;
      val (s1, r1) = neck;
  in
      s = s1 andalso all_same_color(neck::rest)
  end
*)

fun all_same_color [] = true
  | all_same_color (x::[]) = true
  | all_same_color ((s,r)::((s1,r1)::rest)) = 
    (case (s, s1) of 
	 (Spades, Spades) => true
       | (Clubs, Clubs) => true
       | (Clubs, Spades) => true
       | (Spades, Clubs) => true
       | (Hearts, Diamonds) => true
       | (Diamonds, Hearts) => true
       | (Hearts, Hearts) => true
       | (Diamonds, Diamonds) => true
	   | (_,_) => false	   
    ) andalso all_same_color((s1, r1)::rest)

fun sum_cards xs =
    let fun aux([], acc) = acc
	  | aux (x::xs', acc) = aux(xs', acc + card_value(x))
    in
	aux(xs, 0)
    end

fun score(xs, goal) =
    let 
	fun prem x = x * let in if x > 0 then 3 else ~1 end
	fun prem_color x = if all_same_color xs then x div 2 else x
	val sum = (sum_cards xs) - goal
    in
	prem_color (prem sum)
    end


fun officiate (cxs, mxs, goal) = 
    let fun aux ([], moves, hand) = score(hand, goal)
	  | aux (_, [], hand) =  score(hand, goal)
	  | aux (s::stack, m::moves, hand) =  if (sum_cards(hand) > goal)
				       then score(hand, goal)
				       else case m of
						Draw => aux(stack, moves, s::hand)
					     |  Discard card => aux(s::stack, moves, remove_card(hand, card, IllegalMove))
    in
	aux(cxs, mxs, [])
    end

(* complexity roughly equals 3n + m while it is possible to make n+m. Sadly I have no time for that. RL prevails*)
fun score_challenge (xs, goal)=
    let
	val color = all_same_color xs;
	val score_sum = (sum_cards xs) - goal;
	fun count ([], acc) = acc
	  | count ((s, r)::xs, acc) = count(xs, acc + (if (r = Ace) then 1 else 0))	
	fun prem x = x * let in if x > 0 then  3 else ~1 end
	fun prem_color x = if color then x div 2 else x;
	fun cheat (0) = prem_color (prem score_sum)
	  | cheat (count) = let 
	      val tmp1 = prem_color (prem (score_sum-count*10))
	      val tmp2 = cheat(count-1)
	  in if (tmp1 < tmp2)
	     then tmp1 
	     else tmp2
	  end
    in
	cheat (count(xs, 0))
    end


fun officiate_challenge (cxs, mxs, goal) = 
    let fun aux ([], moves, hand) = score_challenge(hand, goal)
	  | aux (_, [], hand) =  score_challenge(hand, goal)
	  | aux (s::stack, m::moves, hand) =  if (sum_cards(hand) > goal)
				       then score_challenge(hand, goal)
				       else case m of
						Draw => aux(stack, moves, s::hand)
					     |  Discard card => aux(s::stack, moves, remove_card(hand, card, IllegalMove))
    in
	aux(cxs, mxs, [])
    end

(* card list and goal *)
fun careful_player (cxs, goal) =    
    let 
	(* hand @ card*)
	fun cheat (cards) = 
	    let fun aux [] = NONE
		  | aux (c::[]) = NONE
		  | aux (c::xs') = let
		      val SOME x = all_except_option (c, cards)
		  in
		      if score(x, goal) = 0
		      then SOME(c)
		      else aux(xs')
		  end
	    in
		aux(cards)
	    end

	fun aux ([],_,mxs ) = mxs
	  | aux (s::stack, [], mxs) = aux(stack, [s], Draw::mxs)
	  | aux (s::stack, hand, mxs) = let 
	      val scr = score(hand, goal);
	      val scr_cheat = cheat(hand @ [s]);
	      val diff = goal - sum_cards(hand);
	  in if scr = 0
	     then mxs
	     else case scr_cheat of
		      SOME card =>  Draw::(Discard card)::mxs
		   | NONE => 
		     if (diff > 10)
		     then aux(stack, s::hand, Draw::mxs)
		     else mxs
	  end
    in
	revers(aux(cxs, [], []))
    end
