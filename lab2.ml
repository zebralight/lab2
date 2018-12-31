(* 
                              CS51 Lab 2
                    Polymorphism and record types
                             Spring 2018
 *)


(*
Objective:

In this lab, you'll exercise your understanding of polymorphism and
record types. Some of the problems extend those from Lab 1, but we'll
provide the necessary background code from that lab.

During the lab session, we recommend that you work on the following
exercises in this order:

1-5; 7; 9-11; 16-17

and complete the remaining ones at your leisure.
 *)

(*======================================================================
Part 1: Currying and uncurrying

........................................................................
Exercise 1: In this exercise, you'll define higher-order functions
curry and uncurry for currying and uncurrying binary functions
(functions of two arguments). The functions are named after
mathematician Haskell Curry '1920. (By way of reminder, a curried
function takes its arguments one at a time. An uncurried function
takes them all at once in a tuple.)

To think about before you start coding:

  * What should the types of curry and uncurry be?

  * What is an example of a function that curry could apply to?
    Uncurry?

  * What are some tests that you could try to verify that your
    implementations of curry and uncurry work properly?

Now implement the two functions curry and uncurry.
......................................................................*)

let curry (f : ('a * 'a) -> 'a) : 'a -> 'a -> 'a = 
  fun a -> fun b -> f(a, b)

(*
js version I needed one more "generation"
 var cur = f => a => b => f(a,b) *)
     
let uncurry (f : 'a -> 'a -> 'a): ('a * 'a) -> 'a = fun (a, b) -> f a b

(*......................................................................
Exercise 2: OCaml's built in binary operators, like ( + ) and ( * ) are
curried:

# ( + ) ;;
- : int -> int -> int = <fun>
# ( * ) ;;
- : int -> int -> int = <fun>

Using your uncurry function, define uncurried plus and times
functions.
......................................................................*)

let plus =
  uncurry ( + )
     
let times =
  uncurry ( * )
  
(*......................................................................
Exercise 3: Recall the prods function from Lab 1:

let rec prods (lst : (int * int) list) : int list =
  match lst with
  | [] -> []
  | (x, y) :: tail -> (x * y) :: (prods tail) ;;

Now reimplement prods using map and your uncurried times function. Why
do you need the uncurried times function?
......................................................................*)

let prods =
  map (fun (x,y) -> times (x, y))

(*======================================================================
Part 2: Option types

In Lab 1, you implemented a function max_list that returns the maximum
element in a non-empty integer list. Here's a possible implementation
for max_list:

let rec max_list (lst : int list) : int =
  match lst with
  | [elt] -> elt
  | head :: tail -> max head (max_list tail) ;;

(This implementation makes use of the polymorphic max function from
the Pervasives module.)

As written, this function generates a warning that the match is not
exhaustive. Why? What's an example of the missing case? Try entering
the function in ocaml and see what information you can glean from the
warning message.

The problem is that there is no reasonable value for the maximum
element in an empty list. This is an ideal application for option
types.

........................................................................
Exercise 4: 

Reimplement max_list, but this time, it should return an int option
instead of an int.
......................................................................*)

let rec max_list (lst : int list) : int option =
  match lst with
    | [] -> None
    | [x] -> Some x
    | x :: rest -> (max (Some x) (max_list rest)) ;;
  
(*......................................................................
Exercise 5: Write a function to return the smaller of two int options,
or None if both are None. If exactly one argument is None, return the
other.  The built-in function min from the Pervasives module may be
useful.
......................................................................*)

let min_option (x : int option) (y : int option) : int option =
  match x, y with
    | None, None -> None
    | _, None -> x
    | None, _ -> y
    | _, _ -> if x < y then x else y
     
(*......................................................................
Exercise 6: Write a function to return the larger of two int options, or
None if both are None. If exactly one argument is None, return the
other.
......................................................................*)

let max_option (x : int option) (y : int option) : int option =
  match x, y with
    | None, None -> None
    | _, None -> x
    | None, _ -> y
    | _, _ -> if x > y then x else y

(*======================================================================
Part 3: Polymorphism practice

........................................................................
Exercise 7: Do you see a pattern in your implementations of min_option
and max_option? How can we factor out similar code?  

Write a higher-order function for binary operations on options taking
three arguments in order: the binary operation (a curried function)
and its first and second argument. If both arguments are None, return
None.  If one argument is (Some x) and the other argument is None,
function should return (Some x). If neither argument is none, the
binary operation should be applied to the argument values and the
result appropriately returned. 

What is calc_option's function signature? Implement calc_option.
......................................................................*)

let calc_option =
  fun _ -> failwith "calc_option not implemented" ;;
     
(*......................................................................
Exercise 8: Now rewrite min_option and max_option using the higher-order
function calc_option. Call them min_option_2 and max_option_2.
......................................................................*)
  
let min_option_2 =
  fun _ -> failwith "min_option_2 not implemented" ;;
     
let max_option_2 =
  fun _ -> failwith "max_option_2 not implemented" ;;

(*......................................................................
Exercise 9: Now that we have calc_option, we can use it in other
ways. Because calc_option is polymorphic, it can work on things other
than int options. Define a function and_option to return the boolean
AND of two bool options, or None if both are None. If exactly one is
None, return the other.
......................................................................*)
  
let and_option =
  fun _ -> failwith "and_option not implemented" ;;
  
(*......................................................................
Exercise 10: In Lab 1, you implemented a function zip that takes two
lists and "zips" them together into a list of pairs. Here's a possible
implementation of zip (here renamed zip_exn to distinguish it
from the zip you'll implement below, which has a different signature):

let rec zip_exn (x : int list) (y : int list) : (int * int) list =
  match x, y with
  | [], [] -> []
  | xhd :: xtl, yhd :: ytl -> (xhd, yhd) :: (zip_exn xtl ytl) ;;

As implemented, this function works only on integer lists. Revise your
solution to operate polymorphically on lists of any type. What is the
type of the result? Did you provide full typing information in the
first line of the definition?
......................................................................*)

let zip_exn =
  fun _ -> failwith "zip_exn not implemented" ;;

(*......................................................................
Exercise 11: Another problem with the implementation of zip_exn is that,
once again, its match is not exhaustive and it raises an exception
when given lists of unequal length. How can you use option types to
generate an alternate solution without this property? 

Do so below in a new definition of zip.
......................................................................*)

let zip =
  fun _ -> failwith "zip not implemented" ;;

(*====================================================================
Part 4: Factoring out None-handling

Recall the definition of dot_prod from Lab 1. Here it is adjusted to
an option type:

let dotprod (a : int list) (b : int list) : int option =
  let pairsopt = zip a b in
  match pairsopt with
  | None -> None
  | Some pairs -> Some (sum (prods pairs)) ;;

Also recall zip from Exercise 8 above.

Notice how in these functions we annoyingly have to test if a value of
option type is None, requiring a separate match, and passing on the
None value in the "bad" branch or introducing the Some in the "good"
branch. This is something we're likely to be doing a lot of. Let's
factor that out to simplify the implementation.

........................................................................
Exercise 12: Define a function called maybe that takes a function of 
type 'a -> 'b and an argument of type 'a option, and "maybe" (depending
on whether its argument is a None or a Some) applies the function to
the argument. The maybe function either passes on the None if its
first argument is None, or if its first argument is Some v, it applies
its second argument to that v and returns the result, appropriately
adjusted for the result type. Implement the maybe function.
......................................................................*)
  
let maybe (f : 'a -> 'b) (x : 'a option) : 'b option =
  failwith "maybe not implemented" ;; 

(*......................................................................
Exercise 13: Now reimplement dotprod to use the maybe function. (The
previous implementation makes use of functions sum and prods. You've
already (re)implemented prods above. We've provided sum for you
below.)  Your new solution for dotprod should be much simpler than in
Lab 1.
......................................................................*)

let sum : int list -> int =
  List.fold_left (+) 0 ;;

let dotprod (a : int list) (b : int list) : int option =
  failwith "dot_prod not implemented" ;; 

(*......................................................................
Exercise 14: Reimplement zip along the same lines, in zip_2 below. 
......................................................................*)

let rec zip_2 (x : int list) (y : int list) : ((int * int) list) option =
  failwith "zip_2 not implemented" ;;

(*......................................................................
Exercise 15: For the energetic, reimplement max_list along the same
lines. There's likely to be a subtle issue here, since the maybe
function always passes along the None.
......................................................................*)

let rec max_list_2 (lst : int list) : int option =
  failwith "max_list not implemented" ;; 

(*======================================================================
Part 5: Record types

A college wants to store student records in a simple database,
implemented as a list of individual course enrollments. The
enrollments themselves are implemented as a record type, called
"enrollment", with string fields labeled "name" and "course" and an
integer student id number labeled "id". An appropriate type might be:
*)

type enrollment = { name : string;
                    id : int;
                    course : string } ;;

(* Here's an example of a list of enrollments. *)

let college = 
  [ { name = "Pat";   id = 1; course = "cs51" };
    { name = "Pat";   id = 1; course = "emr11" };
    { name = "Kim";   id = 2; course = "emr11" };
    { name = "Kim";   id = 2; course = "cs20" };
    { name = "Sandy"; id = 5; course = "ls1b" };
    { name = "Pat";   id = 1; course = "ec10b" };
    { name = "Sandy"; id = 5; course = "cs51" };
    { name = "Sandy"; id = 2; course = "ec10b" }
  ] ;;

(* In the following exercises, you'll want to avail yourself of the
List module functions, writing the requested functions in higher-order
style rather than handling the recursion yourself.

........................................................................
Exercise 16: Define a function called transcript that takes an
enrollment list and returns a list of all the enrollments for a given
student as specified with his or her id.

For example: 
# transcript college 5 ;;
- : enrollment list =
[{name = "Sandy"; id = 5; course = "ls1b"};
 {name = "Sandy"; id = 5; course = "cs51"}]
......................................................................*)

let transcript (enrollments : enrollment list)
               (student : int)
             : enrollment list =
  failwith "transcript not implemented" ;;
  
(*......................................................................
Exercise 17: Define a function called ids that takes an enrollment
list and returns a list of all the id numbers in that enrollment list,
eliminating any duplicates. The sort_uniq function from the List
module may be useful here.

For example:
# ids college ;;
- : int list = [1; 2; 5]
......................................................................*)

let ids (enrollments: enrollment list) : int list =
  failwith "ids not implemented" ;;
  
(*......................................................................
Exercise 18: Define a function called verify that determines whether all
the entries in an enrollment list for each of the ids appearing in the
list have the same name associated.

For example: 
# verify college ;;
- : bool = false
......................................................................*)

let verify (enrollments : enrollment list) : bool =
  failwith "verify not implemented" ;;
