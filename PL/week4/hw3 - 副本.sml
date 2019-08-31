(* Coursera Programming Languages, Homework 3, Provided Code *)

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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
			       (*q1*)
fun only_capitals xs =
    List.filter (fn x=> Char.isUpper(String.sub(x,0))) xs

(*Q2 *)

fun longest_string1 xs =
    List.foldl (fn (x,y)=> if String.size(x) > String.size(y)
			   then
			       x
			   else y)
	       "" xs
fun longest_string2 xs =
    case xs of
	[] => ""
      | x::xs' => longest_string1 (List.rev(x::xs'))

fun longest_string_helper f xs  =
    List.foldl (fn(x,y) => let val lx = String.size(x)
			       val ly = String.size(y)
			   in
			        if f(lx,ly)
				then x
				else y
			   end
	(*Q4-b *)		       ) "" xs 
fun longest_string3 xs =
    longest_string_helper (fn(x,y) => x > y) xs
(*Q5 *)

fun longest_string4 xs =
    case xs of
	[] =>""
	  | xs::xs' => longest_string3 (List.rev(xs::xs')) 
val  longest_capitalized  = longest_string1 o only_capitals 

(* Q6 *)
 val rev_string = String.implode o List.rev o String.explode
(*Q7 *)	       
 fun first_answer f xs =
     case xs  of
	 [] => 0
       | x::xs' => case f x of
		       NONE  => first_answer f xs'
		    | SOME y =>  y
(*Q8 *)
 fun all_answers f xs =
     let fun local_all_answer(f,y,acc)=
	     case y of
		 [] =>acc
	       | x::xs' =>  case (f x) of
				NONE =>local_all_answer(f,xs',acc)
			      | SOME z => local_all_answer(f,xs',acc @ z)
     in  
	 case xs of
	     [] =>SOME []
	   | x::xs' => case local_all_answer(f,x::xs',[]) of
			   [] => NONE
			 | y::ys' => SOME (y :: ys')
     end
	 
	 (*Q9-a*)
 fun count_wildcards pa =
     g (fn ()  =>1)  (fn x => 0)  pa

       (* Q9-b *)
 fun count_wild_and_variable_lengths pa =
     count_wildcards pa + (g (fn x => 0) (fn x => String.size(x)) pa)
			      
(* Q9-c *)
 fun count_some_var (str, pa) =
	 g (fn x => 0) (fn y => if str = y then 1 else 0) pa
     (*g (fn x => 0) (fn y => if str = y then 1 else 0) pa *)     

	   (* Q10 *)
 fun get_vat pa =
     case pa of
	 Variable x => x::[]
       | TupleP PS => List.foldl(fn(m,n)=> (get_vat m) @ n) []  PS 
       | _=> []
		 
    (*et fun g f pa =
	     let val r = g f
	     in
		 case pa of
		     Variable x => x::[]
		   | TupleP PS => List.foldl(fn(m,n)=> (r  n) @ m) [] PS
		   | _=>[]
	     end*)
     
	 
		 
	     
    (* case pa of
	 Variable z => z::[]
			      
      |  TupleP Ps => let f(m,n) =
			  case n of
			      Variable z => (z::[])@m
					 |  => 
      (*List.foldl(fn(m,n) => case n of
						Variable z => (z::[]) @ m
					      | TupleP PaList  => (Palist) @ m
					      | _ =>[]) [] Ps*)
       | _ => [] *)
       
	   (*Q10 *)
 (*fun check_pat pa =
     let
	 val str = (case ps of
			Variable z => z :: []
		      | Tuplep x =>List.foldl (fn x y => case x of
							     Variable z => x @ y
							   | _ => y) [] x
		      | _ => [] )
     in
	 (*str is a string list *)
	 
	*)		      

	 
