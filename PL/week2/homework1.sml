fun is_older(x:int *int * int,y :int * int * int)=
    if #1 x < #1 y
    then true
    else
	if #1x > #1 y
	then false
	else
	    (* #1 x = #1 y *)
	    if #2 x < #2 y
	    then true
	    else
		if #2 x > #2 y
	        then false
		else
		    (* #1 x = #1 y and #2 x = #2 y *)
		    if #3 x < #3 y
		    then true
		    else
			false
	     
	 
fun number_in_month(x:(int*int *int) list,y:int)=
    if null x (* x is empty list *)
    then 0
    else
	if #2 (hd x) = y
	then 1 + number_in_month((tl x),y)
	else
	    0 + number_in_month((tl x),y)
fun number_in_months(x:(int * int * int ) list,y:int list)=
    if null y
    then 0
    else 
    number_in_month(x,hd y) + number_in_months(x,tl y)
					      
fun dates_in_month(x:(int * int *int) list,y:int)=
    if null x
    then []
    else
	if #2 (hd x) = y
	then hd x::dates_in_month(tl x,y)
	else dates_in_month(tl x,y)
			   
(* fun5 dates_in_months *)
fun dates_in_months(x:(int * int * int) list, y:int list) =
    if null y
    then
	[]
    else
	dates_in_month(x,hd y)@dates_in_months(x,tl y)
(*let fun temp(x:(int * int * int) list,y : int list) =
	    if null y
	    then []
	    else dates_in_month(x,hd y)::temp(x,tl y)
    
    in 
    end*)
    
    (*if null  y
    then []
    else
	if null datas_in_month(x,hd y)
	then dates_in_months(x,tl y)
	else
	    (hd dates_in_month(x,hd y))::dates_in_months(x,tl y)*)
 

(* fun6 get_nth *)
fun get_nth(x:string list,y:int) =
	if y = 1
	then hd x
	else get_nth(tl x,y-1)
		    
fun date_to_string(x:int * int * int)=
    let val y = ["January ","February ","March ","April ","May ","June ","July ","August ","September ","October ","November ","December "]
    in get_nth(y,#2 x) ^ Int.toString(#3 x) ^", " ^Int.toString(#1 x)
    end 


(*fun 8 *)
fun number_before_reaching_sum(x:int,y:int list)=
    if hd y >= x
    then 0
    else 1 + number_before_reaching_sum(x - (hd y),tl y)


(* fun 9 *)
fun what_month(day:int)=
    let val temp = [31,28,31,30,31,30,31,31,30,31,30,31]
    in number_before_reaching_sum(day,temp) + 1
    end
	
fun month_range(day:int * int)=
    if #1 day > #2 day
    then
	[]
    else
	let
	    fun result(x:int,y:int)=
	    if x = y
	    then
		what_month(x)::[]
	    else
		what_month(x)::result(x+1,y)
	in
	    result(#1 day,#2 day)
        end				       

	(* 11 oldest*)
fun oldest(x:(int * int *int) list) =
    if null x
    then
	NONE
    else
	let (*fie to assume it's non empty *)
	    fun find_older(y:(int * int * int ) list)=
		if null (tl y)
		then
		    hd y
		else
		    let val temp = find_older(tl y)
		    in
			if is_older((hd y),temp)
			then
			    hd y
			else
			    temp
		    end
	in
	    SOME (find_older x)
	end
	    
		    
