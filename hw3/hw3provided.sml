(* Coursera Programming Languages, Homework 3, Provided Code *)

fun only_capitals xs = List.filter (fn x => Char.isUpper(String.sub(x,0))) xs

fun longest_string1 xs = foldl (fn (str, acc) => if String.size(acc) < String.size(str) 
						      then str 
						      else acc) "" xs

fun longest_string2 xs = foldl (fn (str, acc) => if String.size(acc) <= String.size(str) 
						      then str 
						      else acc) "" xs

fun longest_string_helper f strlist = foldl (fn (s1, s2) => if f(String.size(s1), String.size(s2)) 
				   then s1 
				   else s2) "" strlist


val longest_string3 = longest_string_helper (fn (size1, size2) =>  size1 > size2)

val longest_string4 = longest_string_helper (fn (size1, size2) =>  size1 >= size2)

val longest_capitalized  =  longest_string1 o only_capitals 

fun rev_string str = (String.implode o List.rev o String.explode) str

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

fun count_wildcards pattern = g (fn ()=> 1) (fn x => 0) pattern

fun count_wild_and_variable_lengths pattern = g (fn ()=> 1) (fn x => String.size(x)) pattern

fun count_some_var (str, pattern) = g (fn ()=> 0) (fn x => if x = str then 1 else 0) pattern

fun first_answer f [] = raise NoAnswer
  | first_answer f (x::xs) = case f(x) of
			    NONE => first_answer f xs 
			  | SOME v => v
    
fun all_answers f xs = 
    let 
	fun aux acc [] = SOME acc
	  | aux acc (x::xs') = 
	    case f(x)of
		SOME v => aux (v @ acc) xs'
	     |  NONE => NONE
    in
	aux [] xs
    end

fun check_pat pattern =
    let
	fun var_list pattern = 
	    case pattern of
		Variable v => [v]
	      | TupleP ps => List.foldl (fn (pttrn, acc) => acc @ var_list(pttrn)) [] ps
	      | ConstructorP (_, p) => var_list p
	      | _ => []

	fun uniq_check [] = true
	  | uniq_check (s::strlist) = if List.exists (fn x => x = s) strlist 
				 then false
				 else uniq_check(strlist)
    in
	uniq_check ( var_list pattern)
    end
(* 
Wildcard - anything is good
Variable (s) - Variable(v) из выражения и получается (s, v)
UnitP - Unit => [] 
Const(s) - Const(v) => ()
TupleP(lst) - Tuple(lstv) if size(lst) == size(lstv) and lst[0...i] == lstv[0..i]
ConstructorP(s1, p) - COnstructor(s2, v) => if s1 == s2 and p == v
Sorry for some russian here, I need it to pick up my mind :(
This 'match' task is a one great mystery. 
*)
fun match (vl, ptr) = 
    case (vl, ptr) of 
	(_, Wildcard) => SOME []
      | (_, Variable s) => SOME [(s, vl)]
      | (Unit, UnitP) => SOME []
      | (Const c, ConstP p) => if c = p 
			      then SOME []
			      else NONE
      | (Tuple t, TupleP tp) => if List.length(t) = List.length(tp)
			       then all_answers match (ListPair.zip(t, tp))
			       else NONE
      | (Constructor (c, cv), ConstructorP(p, pv)) => if c = p 
						      then match(cv, pv)
						      else NONE
      | _ => NONE
		    
fun first_match vl [] = NONE
  | first_match vl patternS = SOME (first_answer (fn x => match(vl, x)) patternS) 
				 handle NoAnswer => NONE


(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* like List.foldl but the accumulator is an Option that will short-circuit on
NONE
COMMENTED OUT
fun optfoldl f acc lst =
            case (lst, acc) of
                (x :: xs, SOME y) => optfoldl f (f (x, y)) xs
              | _ => acc
 *)
(* like List.mapPartial but will short-circuit with NONE when f x
returns NONE 
fun optmap f =
    Option.map rev o
    optfoldl (fn (x, acc) => Option.map (fn a => a :: acc) (f x)) (SOME [])

fun typecheck_patterns (datatypes, patterns) =
    let fun join_types pair =
            case pair of
                (Anything, y) => SOME y
              | (x, Anything) => SOME x
              | (TupleT xs, TupleT ys) =>
                Option.map
                    TupleT
                    (optmap join_types (ListPair.zipEq (xs, ys))
                     handle ListPair.UnequalLengths => NONE)
              | (x, y) => if x = y then SOME x else NONE
        fun pat_to_typ p =
            case p of
                Wildcard => SOME Anything
              | Variable _ => SOME Anything
              | UnitP => SOME UnitT
              | ConstP _ => SOME IntT
              | TupleP ps => Option.map TupleT (optmap pat_to_typ ps)
              | ConstructorP (s, cp) =>
                case (pat_to_typ cp,
                      List.find (fn (cs, _, _) => cs = s) datatypes) of
                    (SOME pt, SOME (cs, ds, dt)) =>
                    if isSome (join_types (pt, dt))
                    then SOME (Datatype ds)
                    else NONE
                  | _ => NONE
    in
        Option.mapPartial
            (optfoldl join_types (SOME Anything))
            (optmap pat_to_typ patterns)
    end
*)
