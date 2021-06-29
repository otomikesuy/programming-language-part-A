(* Hello!! *)
(* English is not my first language, please ALSO correct me if my english is wrong! *)
(* thanks in advence :) *)

(* assignment1 *)
fun is_older (date1 : int * int * int, date2 : int * int * int) =
    if (#1 date1) < (#1 date2)
    then true
    else if (#1 date1) = (#1 date2) andalso (#2 date1) < (#2 date2)
    then true
    else if (#1 date1) = (#1 date2) andalso (#2 date1) = (#2 date2) andalso (#3 date1) < (#3 date2)
    then true
    else false
		    
(* assignment2 *)
fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else
	if (#2 (hd dates)) = month
	then 1 + number_in_month(tl dates, month)
	else number_in_month(tl dates, month)

(* assignment3 *)
fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null dates orelse null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* assignment4 *)
fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else
	if (#2 (hd dates)) = month
	then (hd dates) :: dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month)

(* assignment5 *)
fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null dates orelse null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* assignment6 *)
fun get_nth (elms : string list, nth : int) =
    if nth = 1
    then hd elms
    else get_nth(tl elms, nth - 1)

(* assignment7 *)
fun date_to_string (date : (int * int * int)) =
    let val month_list = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth(month_list, (#2 date)) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
    end
	
(* assignment8 *)
fun number_before_reaching_sum (sum : int, numbers : int list) =
    if sum <= hd numbers
    then 0
    else 1+number_before_reaching_sum(sum - hd numbers, tl numbers)
						
(* assignment9 *)
fun what_month (day : int) =
    let val days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
    in 1+number_before_reaching_sum(day,days_in_month)
    end

(* assignment10 *)				    
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else
	what_month(day1) :: month_range(day1+1, day2)
	    

(* assignment11 *)
fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else
	let val cur_oldest = oldest(tl dates)
	in
	    if (not (isSome cur_oldest)) orelse (is_older(hd dates, valOf cur_oldest))
	    then SOME (hd dates)
	    else cur_oldest
	end
	    
