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
(*fun longest_string2 xs =*)
    (*List.foldl(fn (x,y) => if String.size(x) > String.size(y)
			   then  x  
			   elsec
			       y)
	      "" xs *)
	       (*Q5 *)
val  longest_capitalized  = longest_string1 o only_capitals 

(* Q6 *)
 val rev_string = String.implode o List.rev o String.explode
(*Q7 *)	       
 fun first_answer f xs =
     case xs of
	 [] => 0
       | x::xs' => case f x of
		       NONE  => first_answer f xs'
		    | SOME y =>  y
(*Q8 *)
 fun all_answers f xs =
     let fun local_all_answer(f,xs,acc)=
	     case xs of
		 [] =>acc
	       | x::xs' =>  case f x of
				NONE =>local_all_answer(f,xs',acc)
			      | SOME [y] => local_all_answer(f,xs',acc @ [y])
     in
	 case xs of
	     [] =>SOME []
	   | x::xs' => case local_all_answer(f,x::xs',[]) of
			   [] => NONE
			 | y::ys' => SOME (y :: ys')
     end
	 
	 (*Q9-a*)
(* fun count_wildcards pa =
*)     
