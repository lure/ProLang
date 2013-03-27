is_older((~5,5,5), (1,2,3));
not (is_older((1,1,1),(1,1,1)));
not (is_older((1995,1,1),(1,1,1)));

number_in_month([(1,2,3),(4,5,6),(7,8,9),(10,11,12),(13,14,15)], 5) = 1;
number_in_month([(1,2,3),(4,5,6),(7,8,9),(10,11,12),(13,14,15)], 12313) = 0;
number_in_month([(1027,1,1),(733,56,13),(1991,5,4),(8874,5,04),(6,4,4)], 5) = 2;

number_in_months([(1027,1,1),(733,56,13),(1991,5,4),(8874,5,04),(6,4,4)], [5,1,4]) = 4;
number_in_months([(1027,1,1),(733,56,13),(1991,5,4),(8874,5,04),(6,4,4)], [0]) = 0;
number_in_months([(1027,1,1),(733,56,13),(1991,5,4),(8874,5,04),(6,4,4)], []) = 0;
number_in_months([], []) = 0;
number_in_months([], [5]) = 0;

dates_in_month([(1001,5,1),(1002,5,2),(1003,5,3),(1001,5,1),(1991,5,5)], 5) = [(1001,5,1),(1002,5,2),(1003,5,3),(1001,5,1),(1991,5,5)];
dates_in_month([(1027,1,1),(733,56,13),(1991,5,4),(8874,5,04),(6,4,4)], 0) = [];
dates_in_month([(1027,1,1),(733,56,13),(1991,5,4),(8874,5,04),(6,4,4)], 56) =  [(733,56,13)];
dates_in_month([], 5)=[];

dates_in_months([], [])=[];
dates_in_months([(1001,5,1)], [])=[];
dates_in_months([], [1])=[];
dates_in_months([(1001,5,1),(1002,5,2),(1003,5,3),(1001,5,1),(1991,5,5)], [5,1,4]) =  [(1001,5,1),(1002,5,2),(1003,5,3),(1001,5,1),(1991,5,5)];
dates_in_months([(1027,1,1),(733,56,13),(1991,5,4),(8874,5,04),(6,4,4)], [5,1,4]) =  [(1991,5,4),(8874,5,4),(1027,1,1),(6,4,4)];
dates_in_months([(1027,1,1),(733,56,13),(1991,5,4),(8874,5,04),(6,4,4)], [0]) = [];

(* exceptions
get_nth([], 1);
get_nth([], 0); 
get_nth(["rabc"], 0); 
get_nth(["ab", "cd", "ef"], 4); *)
get_nth(["rabc"], 1) = "rabc";
get_nth(["ab", "cd", "ef"], 1) = "ab";
get_nth(["ab", "cd", "ef"], 2) = "cd";
get_nth(["ab", "cd", "ef"], 3) = "ef";

date_to_string(2013, 1, 20) = "January 20, 2013";
(* date_to_string(2013, 0, 20); *)
date_to_string(1854, 6, 1)= "June 1, 1854";

number_before_reaching_sum(0, [1,2,3]) = 0;
(* number_before_reaching_sum(5, []); 
number_before_reaching_sum(0, []) =0;*)
number_before_reaching_sum(15, [0, 1, 123]) = 2;
number_before_reaching_sum(15, [0, 1, 10, 4]) = 3;

what_month(0)=0;
what_month(366)=0;
what_month(365)=12;
what_month(1)=1;
what_month(100)=4;

month_range(1,10)= [1,1,1,1,1,1,1,1,1,1];
month_range(25,35)=[1,1,1,1,1,1,1,2,2,2,2];
month_range(10,9)=[];
month_range(10,10)=[1];
month_range(1, 365)=[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12];
month_range(335, 365) =  [12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12];

oldest ([])=NONE;
oldest ([(1991,1,14)])=SOME (1991,1,14);
oldest ([(1991,1,14), (2020,5,17), (19,1,14)])=SOME (19,1,14);

dates_in_months_challenge([], []) =  [];
dates_in_months_challenge([(1001,5,1)], [])=[];
dates_in_months_challenge([], [1])=[];
dates_in_months_challenge([(1001,5,1),(1002,5,2),(1003,5,3),(1004,5,1),(1991,5,5)], [5,1,4,5,4,6]) =  [(1001,5,1),(1002,5,2),(1003,5,3),(1004,5,1),(1991,5,5)];
dates_in_months_challenge([(1001,5,1),(1002,5,2),(1003,5,3),(1004,5,1),(1991,5,5)], [5,5,5,5,5]) =  [(1001,5,1),(1002,5,2),(1003,5,3),(1004,5,1),(1991,5,5)];
dates_in_months_challenge([(1001,5,1),(1002,5,2),(1003,5,3),(1004,5,1),(1991,5,5)], [1,1,1,1,1]) =  [];


reasonable_date(1940,2,29);
not (reasonable_date(1939,2,29));
reasonable_date(2000,1,1);
reasonable_date(2000,1,31);
not(reasonable_date(2000,6,31));
reasonable_date(2000,12,31);
not(reasonable_date(000,12,31));
not(reasonable_date(1000,0,31));
not(reasonable_date(1000,12,0));



