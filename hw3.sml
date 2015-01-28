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

    
fun count_wildcards p = 
    g (fn f1 => 1) (fn f2 => 0) p

fun count_wild_and_variable_lengths p = 
    g (fn f1 => 1) size p


fun count_some_var (some_string, p) =
    g (fn f1 => 0) (fn f2 => if Variable(some_string) = p 
			     then 1 
			     else 
				 case p of
				     TupleP ps =>
				     (case ps of
					  x :: xs' => List.foldl (fn (y, acc) => if y = Variable(some_string) 
										 then 1
										 else acc
								 ) 0 ps
				     )
				   | _ => 0) p

(*
fun check_pat pat = 
    let fun helper1 patches = 
    (case patches of
	 ConstructorP (_,p) => helper1 p
       | TupleP ps => (case ps of
			   [] => []
			 | x :: xs' => List.foldl (fn (y, acc) => case y of 
								      Variable x => acc @ [x] @ (helper1 (TupleP xs'))
								    | _ => acc
						  ) [] ps
		      )
       | Variable x => [x]
       | _ => []
    )
    in
	case helper1 pat of
	    [] => true
	  | [z] => true
	  | z :: zs' => if hd (helper1 pat) = hd (tl (helper1 pat))
			then false
			else true
    end
*)

fun check_pat pat =
    let fun helper1 patches =
	    (case patches of
		 ConstructorP (_,p) => helper1 p
	       | Variable x => [Variable x]
	       | TupleP ps => List.foldl (fn (y, acc) => acc @ helper1 y) [] ps
	       | _ => [])
	     in
		 helper1 pat
	     end
					
fun match (v, p) = 
    case p of
	Wildcard => SOME []
      | Variable x => if x = "Const(v)"
		      then SOME [("x", v)]
		      else NONE
      | UnitP => if v = Unit
		 then SOME []
		 else NONE
      | ConstP y => if v = Const y
		    then SOME []
		    else NONE
      | ConstructorP (a, b) => (case v of
					  Constructor (c, d) => 
					  if a = c
					  then SOME []
					  else NONE)
      | _ => NONE

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

val only_capitals = 
    fn string_list => List.filter (fn string => Char.isUpper(String.sub(string,0))) string_list

val longest_string1 = 
    fn string_list => List.foldl ((fn (a,b) => if String.size a > String.size b then a else b)) "" string_list

fun longest_string2 string_list = 
    List.foldl ((fn (a,b) => if String.size a > String.size b then a else if String.size a = String.size b then a else b)) "" string_list


fun longest_string_helper f = fn string_list =>
				 (List.foldl (fn (x, y) => if f (String.size x, String.size y) then x else y) "" string_list)

val longest_string3 =
    longest_string_helper (op >)

val longest_string4 = 
    longest_string_helper (op >=)

val longest_capitalized = 
    longest_string1 o only_capitals

val rev_string = 
    String.implode o rev o String.explode

fun first_answer f = fn f_list =>
			case f_list of
			    [] => raise NoAnswer
			  | x :: xs' => 
			    case f x of
				NONE => first_answer f xs'
			      | SOME x => x

fun first_match v pl = 
    SOME (first_answer (fn x => match (v, x)) pl) handle _ => NONE


fun all_answers f = fn f_list =>
		       let fun helper some_list =
			       case some_list of
				   [] => []
				 | x :: xs' => 
				   case f x of
				       SOME lst => lst @ helper xs'
		       in
			   SOME(helper f_list) handle _ => NONE
		       end
