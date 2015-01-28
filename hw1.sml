fun is_older (date1 : int * int * int, date2 : int * int * int) =
    if #1 date1 <> #1 date2
    then #1 date1 < #1 date2 
    else 
	if #2 date1 <> #2 date2
	then #2 date1 < #2 date2
	else 
	    if #3 date1 <> #3 date2
	    then #3 date1 < #3 date2
	    else false

fun number_in_month (dates_list : (int * int * int) list, month : int) = 
    if dates_list = []
    then 0
    else
	let val counter = 0
	in
	    if month = #2 (hd dates_list)
	    then
		if null (tl dates_list)
		then counter + 1
		else counter + 1 + number_in_month(tl dates_list, month)
	    else 
		if null (tl dates_list)
		then counter
		else counter + number_in_month(tl dates_list, month)
	end

fun number_in_months (dates_list : (int * int * int) list, months_list : int list) = 
    if months_list = []
    then 0
    else
	let val new_counter = 0
	in
	    if number_in_month(dates_list, hd months_list) > 0 andalso null (tl months_list)
	    then new_counter + number_in_month(dates_list, hd months_list)
	    else 
		if number_in_month(dates_list, hd months_list) > 0
		then new_counter + number_in_month(dates_list, hd months_list) + number_in_months(dates_list, tl months_list)
		else new_counter
	end

fun dates_in_month (dates_list : (int * int * int) list, month : int) = 
    if number_in_month(dates_list, month) = 0
    then []
    else
	if month = #2 (hd dates_list)
	then (hd dates_list) :: dates_in_month(tl dates_list, month)
	else dates_in_month(tl dates_list, month)

fun dates_in_months (dates_list : (int * int * int) list, months_list : int list) =
    if number_in_months(dates_list, months_list) = 0
    then []
    else
	if dates_in_month(dates_list, hd months_list) <> []
	then dates_in_month(dates_list, hd months_list) @ dates_in_months(dates_list, tl months_list)
	else dates_in_months(dates_list, tl months_list)

fun get_nth (string_list : string list, index : int) =
    if index = 1
    then hd string_list
    else get_nth(tl string_list, index - 1)

fun date_to_string (date : int * int * int) =
    let val months_as_strings = ["January ", "February ", "March ", "April ", "May ", "June ", "July ", "August ", "September ", "October ", "November ", "December "]
    in
	get_nth(months_as_strings, #2 date) ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum : int, num_list : int list) = 
    if sum - hd num_list <= 0
    then 0
    else 1 + number_before_reaching_sum(sum - hd num_list, tl num_list)

fun what_month (day_of_year : int) = 
    let val day_of_year_list = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in number_before_reaching_sum(day_of_year, day_of_year_list) + 1
    end

fun month_range (date1 : int, date2 : int) = 
    if date1 <= date2
    then what_month(date1) :: month_range(date1 + 1, date2)
    else []

fun oldest (dates : (int * int * int) list) = 
    if null dates
    then NONE
    else
	let fun max_date (dates : (int * int * int) list) =
	    if null (tl dates)
	    then hd dates
	    else 
		let val oldest_val = max_date(tl dates)
		in
		    if is_older(hd dates, oldest_val)
		    then hd dates
		    else oldest_val
		end
	in SOME (max_date dates)
	end
