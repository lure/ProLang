(* val t00 = count_wildcards (TupleP([ConstructorP("5", ConstP(77)), ConstP 67, UnitP, Wildcard, Wildcard]))
count_wild_and_variable_lengths (TupleP([ConstructorP("5", ConstP(77)), ConstP 67, UnitP, Wildcard, Wildcard]))
*)
val t1 = only_capitals [] = []
val t2 = only_capitals ["hello", "bye"] = []
val t3 = only_capitals ["more", "Monro", "ala"] = ["Monro"]
val t4 = only_capitals ["hello", "monro", "Bye"] = ["Bye"]
val t5 = only_capitals ["Ramma", "bye", "more"] = ["Ramma"]
 
val t6 = longest_string1 [] = ""
val t7 = longest_string1 ["monro"] = "monro"
val t8 = longest_string1 ["monro", "hello"] = "monro"
val t9 = longest_string1 ["mena", "hello"] = "hello"
val t10 = longest_string1 ["hello", "mena"] = "hello"
 
val t11 = longest_string2 [] = ""
val t12 = longest_string2 ["monro"] = "monro"
val t13 = longest_string2 ["monro", "hello"] = "hello"
val t14 = longest_string2 ["mena", "hello"] = "hello"
val t15 = longest_string2 ["hello", "mena"] = "hello"
 
val t16 = longest_string3 [] = ""
val t17 = longest_string3 ["monro"] = "monro"
val t18 = longest_string3 ["monro", "hello"] = "monro"
val t19 = longest_string3 ["mena", "hello"] = "hello"
val t20 = longest_string3 ["hello", "mena"] = "hello"
 
val t21 = longest_string4 [] = ""
val t22 = longest_string4 ["monro"] = "monro"
val t23 = longest_string4 ["monro", "hello"] = "hello"
val t24 = longest_string4 ["mena", "hello"] = "hello"
val t25 = longest_string4 ["hello", "mena"] = "hello"
 
val t26 = longest_capitalized [] = ""
val t27 = longest_capitalized ["hello", "bye"] = ""
val t28 = longest_capitalized ["Mara", "hello", "bye"] = "Mara"
val t29 = longest_capitalized ["Mara", "hello", "Kenia", "bye"] = "Kenia"
 
val t30 = rev_string "" = ""
val t31 = rev_string "M" = "M"
val t32 = rev_string "hello" = "olleh"
val t33 = rev_string "reNo" = "oNer"
 
val t34 = first_answer (fn x => if x = 3 then SOME "hi" else NONE) [1,2] = "M" handle NoAnswer => true
val t35 = first_answer (fn x => if x = 3 then SOME "hi" else NONE) [1,2,3] = "hi"
 
val t36 = all_answers (fn x => NONE) [] = SOME []
val t37 = all_answers (fn x => NONE) [1] = NONE
val t38 = all_answers (fn x => if x < 3 then SOME ["hi","hello"] else NONE) [1,2] =
SOME ["hi", "hello", "hi", "hello"]
 
val t39 = count_wildcards UnitP = 0
val t40 = count_wildcards Wildcard = 1
val t41 = count_wildcards (ConstructorP ("M", Wildcard)) = 1
val t42 = count_wildcards (TupleP [Wildcard, UnitP, Wildcard]) = 2
 
val t43 = count_wild_and_variable_lengths UnitP = 0
val t44 = count_wild_and_variable_lengths Wildcard = 1
val t45 = count_wild_and_variable_lengths (Variable "monro") = 5
val t46 = count_wild_and_variable_lengths (ConstructorP ("M", Wildcard)) = 1
val t47 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "monro", UnitP, Wildcard]) = 7
 
val t48 = count_some_var ("monro", UnitP) = 0
val t49 = count_some_var ("monro", Wildcard) = 0
val t50 = count_some_var
("monro", TupleP [Variable "monro", UnitP, Variable "monro", Variable "mena"]) = 2
 
val t51 = check_pat UnitP = true
val t52 = check_pat (TupleP [Variable "monro", Variable "monro", Variable "mena"]) = false
val t53 = check_pat (TupleP [Variable "monro", Variable "mena"]) = true

val t54 = match( Const 7, Wildcard ) = SOME []
val t55 = match( Unit, Wildcard ) = SOME []
val t56 = match( Tuple[Const 7], Wildcard ) = SOME []
val t57 = match( Constructor("ABC", Const 7), Wildcard ) = SOME []

val t58 = match( Const 7, Variable "A" ) = SOME [("A", Const 7)]
val t59 = match( Unit, Variable "sName" ) = SOME [("sName", Unit)]

val t60 = match( Unit, UnitP ) = SOME []
val t61 = match( Const 7, UnitP ) = NONE

val t62 = match( Const 7, ConstP 7 ) = SOME []
val t63 = match( Const 7, ConstP 8 ) = NONE

val t64 = match( Constructor("ABC", Const 7), ConstructorP( "ABC", Wildcard ) ) = SOME[]
val t65 = match( Constructor("AB", Const 7), ConstructorP( "ABC", Wildcard ) ) = NONE
val t66 = match( Constructor("ABC", Const 7), ConstructorP( "ABC", UnitP ) ) = NONE
val t67 = match( Constructor("ABC", Const 7), ConstructorP( "ABC", Variable "bbba" ) ) = SOME[("bbba", Const 7)]

val t68 = match( Tuple[Const 7], TupleP[ConstP 7] ) = SOME []
val t69 = match( Tuple[Const 7], TupleP[ConstP 7,ConstP 7] ) = NONE
val t70 = match( Tuple[Const 7, Const 6, Unit, Const 7], TupleP[ConstP 7, Variable "ba", Wildcard, ConstP 8] ) = NONE
val t72 = match( Tuple[Const 7, Const 6, Unit, Const 7], TupleP[Variable "a", Variable "ba", Variable "bba", Variable "bbba"] ) = SOME [("bbba",Const 7),("bba",Unit),("ba",Const 6),("a",Const 7)]

val first_match_test1 = first_match (Const 7) [ConstP 1, UnitP, ConstP 7, Variable "aa"] = SOME []
(* Match with no bindings *)
val first_match_test2 = first_match (Const 7) [ConstP 1, UnitP, Variable "aa", ConstP 7] = SOME [("aa", Const 7)]
(* Match with one binding *)
val first_match_test3 = first_match (Const 7) [ConstP 1, UnitP, TupleP [UnitP, UnitP]] = NONE