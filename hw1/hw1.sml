val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

(*1*)
fun is_older(d1:int*int*int, d2:int*int*int) =
    (#1 d1 <  #1 d2) orelse
    ((#1 d1 = #1 d2) andalso (#2 d1 < #2 d2)) orelse
    ((#1 d1 = #1 d2) andalso (#2 d1 = #2 d2) andalso (#3 d1 < #3 d2))

(* 2 *)
fun number_in_month(xs:(int*int*int)list, month: int) =
    if null xs 
    then 0
    else if (#2 (hd xs)) = month 
    then 1 + number_in_month(tl xs, month)
    else number_in_month(tl xs, month)

(* 3 *)
fun number_in_months(dxs:(int*int*int)list, mxs: int list) =
    if null mxs orelse null dxs
    then 0
    else number_in_month(dxs, hd mxs) + number_in_months(dxs, tl mxs)

(* 4 *)
fun dates_in_month(xs:(int*int*int)list, month: int) =
    if null xs
    then []
    else if (#2 (hd xs)) = month
    then hd xs :: dates_in_month(tl xs, month)
    else dates_in_month(tl xs, month)

(* 5 *)
fun dates_in_months(dxs:(int*int*int)list, mxs: int list) =
    if null dxs orelse null mxs
    then []
    else dates_in_month(dxs, hd mxs) @ dates_in_months(dxs, tl mxs)

(* 6 *)
fun get_nth(xs: string list, pos: int)=
    if pos = 1
    then hd xs
    else get_nth(tl xs, pos-1)

(* 7 *)
fun date_to_string (date: (int*int*int)) =
    get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)

(* 8 *)
fun number_before_reaching_sum(sum: int, xs: int list)=
    if (sum <= hd xs)
    then 0
    else 1 + number_before_reaching_sum(sum - hd xs, tl xs)

(* 9 *)
fun what_month(day_of_the_year: int) =
    if day_of_the_year < 1 orelse day_of_the_year > 365
    then 0
    else number_before_reaching_sum(day_of_the_year, days_in_month)+1
				     
(* 10 
What Dan wants here is to use what_month with recursion. Something like 
    fun month_range (day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month day1 :: month_range(day1 + 1, day2)
What I am doing here is to exclude some internal recursion in exchange of somewhat unreadable code. =)
Don't try it at home.
*)
fun month_range(day1: int, day2: int) =
    if day1 > day2 orelse day1 < 1 orelse day2 > 365
    then []
    else let
		fun snowball(count: int, month: int, days_left: int, xs: int list) =
			if count = day2 (* reached the end of period*)
			then [month]
			else if count < day1 (* here we have to start counting*)
				then if days_left > 0
					then snowball(count+1, month, days_left-1, xs)
					else snowball(count+1, month+1, (hd xs)-1, tl xs)
				else if count >= day1  (* looks like this 'count >= day1 is redundunt*)
					then if days_left > 0
						then month::snowball(count+1, month, days_left-1, xs)
						else month::snowball(count+1, month+1, (hd xs)-1, tl xs)
					else []
    in
	snowball(1, 1, hd days_in_month-1, tl days_in_month)
    end
	
(*11*)
fun oldest (xs: (int*int*int) list)=
    if null xs
    then NONE
    else let val tl_old = oldest(tl xs)
	in if isSome tl_old andalso is_older(valOf tl_old, hd xs)
	    then tl_old
	    else SOME(hd xs)
	end

(* 12 *)
fun is_uniq(x: int, xs: int list) =
    (null xs) orelse (x <> hd xs andalso is_uniq(x, tl xs))

fun uniq(xs: int list) =
    if null xs
    then []
    else if is_uniq(hd xs, tl xs)
    then hd xs :: uniq(tl xs)
    else uniq(tl xs)
		
fun dates_in_months_challenge(dxs:(int*int*int)list, mxs: int list) =
    let	val uniq_mxs = uniq(mxs)
    in
	if null dxs orelse null uniq_mxs
	then []
	else dates_in_month(dxs, hd uniq_mxs) @ dates_in_months(dxs, tl uniq_mxs)
end

fun number_in_months_challenge(dxs:(int*int*int)list, mxs: int list) =
    let	val uniq_mxs = uniq(mxs)
    in	
	if null uniq_mxs orelse null dxs
	then 0
	else number_in_month(dxs, hd uniq_mxs) + number_in_months(dxs, tl uniq_mxs)
end

fun reasonable_date(d: (int*int*int))=
    let
	fun get_days(m: int, y: int)=	
	    let
		fun leap(y:int)=
		    (y mod 400 = 0) orelse (y mod 100 <> 0 andalso y mod 4 = 0)
		
		fun from_list(m: int, xs: int list)=
		    if m = 1 
		    then hd xs
		    else from_list(m-1, tl xs)
	    in
		if m = 2 
		then if leap(y)
		     then 29 
		     else 28
		else from_list(m, days_in_month)
	    end

	val y = #1 d
	val m = #2 d
	val d = #3 d
    in
	(y > 0) andalso (m > 0 andalso m < 13) andalso (d > 0 andalso d <= get_days(m , y))
    end
