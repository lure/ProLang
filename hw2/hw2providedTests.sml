(* Dan Grossman, Coursera PL, HW2 Provided Tests *)

(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)
fun provided_test1 () = (* correct behavior: raise IllegalMove *)
    let val cards = [(Clubs,Jack),(Spades,Num(8))]
	val moves = [Draw,Discard(Hearts,Jack)]
    in
	officiate(cards,moves,42)
    end;

fun provided_test2 () = (* correct behavior: return 3 *)
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val moves = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(cards,moves,42)
    end;
	
 
score([], 20) = 10;
score([(Spades, Jack), (Spades, Ace)], 20) = 1;
score([(Spades, Ace), (Spades, Ace)], 20) = 3;
score([(Spades, Ace), (Clubs, Ace)], 20) = 3;
score([(Spades, Ace), (Hearts, Ace)], 20)= 6;

(officiate([(Clubs,Jack),(Spades,Num(8))], [Draw,Discard(Hearts,Jack)], 42) handle IllegalMove => 0) = 0;
officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw], 42) = 3;
officiate([(Clubs,Ace),(Clubs,Ace),(Clubs,Ace),(Clubs,Ace)], [Draw,Draw,Draw,Draw,Draw], 42) = 3;
officiate([(Clubs,Ace),(Clubs,Ace),(Clubs,Ace),(Clubs,Ace)], [Draw,Draw,Draw], 42) = 4;
officiate([(Clubs,Ace),(Clubs,Ace),(Clubs,Ace),(Clubs,Ace)], [], 42) = 21;
officiate([(Clubs,Ace),(Clubs,Ace),(Spades,Jack),(Clubs,Ace)], [Draw,Draw,Discard(Clubs,Ace) ], 42) = 15;

officiate( [(Clubs,Num 1),(Spades, Num 2),(Clubs, Num 3),(Spades, Num 4)], [Draw,Draw,Draw,Draw, Discard (Clubs, Num 2)], 40) = 15;
officiate( [(Hearts,Num 1),(Spades, Num 2),(Clubs, Num 3),(Spades, Num 4)], [Draw,Draw,Draw,Draw, Draw], 40)=30;
