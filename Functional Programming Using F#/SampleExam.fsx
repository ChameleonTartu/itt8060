// Question 1: True or False

// a. -> T ( because it will be -1 + 1 + 2 + 3 + 4 = 9

List.fold (+) -1 [1; 2; 3; 4]

// b. -> T ( because int list, in some sence, we convert into int list

([1;2] :> System.Object) :?> int list

// c. -> T? (It is not clear enough from question what it means)

// d. -> T ( lazy tuple does not have type tuple.)

fst (lazy (1, 2))

// e. -> F (because it is not a Run Time Error)

// f. -> T (straightforward question)

let x = printfn "hello"; 2

// g. -> T [(1,2,3)] = (int*int*int) list, be accurate with it, because int * int * int list means different thing.

// h. -> F (because it is int option option, not just int option)
Some (Some 42)

// i. -> F ( it will return an error )
Option.bind Some None

// j. -> F will generate infinite sequence of ones.

let rec t = seq{ yield 1; yield! t }

// k. -> F (actually, false, because input not a list, be accurate commas are used only in tuples, semicolons are used in lists, sequences, arrays).
// Even a result was [3,5,7,9] as expected, it is still false because of list notation
List.map (fun n -> n + 2) [1, 3, 5, 7]

// l. -> F? (I think that it is false, because for every node do not correspond unique label, but I need to read about this more.)
type Tree =
    | Leaf of int
    | Node of Tree * Tree

// Question 2: Trees

type STree = 
    | Val of string
    | Concat of STree*STree 

// a.
//Concat( Concat( Val "a", Val "b"), Concat(Val "c", Val "d"))

// b.
let rec f x =
    match x with
        | Val s -> Val (s.ToUpper())
        | Concat (a,b) -> Concat (a,f b)

// i.
f (Concat (Concat (Concat (Val "a", Val "b"), Val "c"), Val "d"))

// Result is:
//(Concat (Concat (Concat (Val "a", Val "b"), Val "c"), Val "D"))

// ii.
// STree -> STree

// iii.
//Make Val in Concat in Right subtree Upperbounded.

// c.
let rec addBang (x: STree) : STree =
    match x with
    | Val s -> Val (s + "!")
    | Concat (a, b) -> Concat (addBang a, addBang b)

addBang (Concat (Concat (Concat (Val "a", Val "b"), Val "c"), Val "d"))

// d.
let rec flatten (x: STree) : string =
    match x with
    | Val p -> p
    | Concat(a, b) -> flatten a + flatten b

flatten (Concat (Concat (Concat (Val "a", Val "b"), Val "c"), Val "d"))

// e.
// f: string -> string
// (string -> string) -> STree -> STree



// Question 3:

// a.
let first (list : 'a list) : 'a =
    if( list = [] ) then
        failwith "oops"
    else
        list.[0]

first ['a'; 'b'; 'n']
first []

// b.

let rec g x y =
    match y with
    | a :: b :: cs -> a :: x b :: g x cs
    | d -> d

// i
g (fun x -> x + 1) [1..4]

// ii

//('a -> 'a) -> 'a list -> 'a list 
// The trick is to predict what type g has without previous example.

// iii
// Changing every odd indexed elements, if indexing starting from 0.

// c.

// Bugs that I have found
// 1. _, [] - such pattern does not present

// 2. xs', ys' should be in zip call. 
// 4th line of code 
// now: | x :: xs', y :: ys' -> (x,  y) :: zip(xs, ys)
// should be: | x :: xs', y :: ys' -> (x,  y) :: zip(xs', ys')

// 3. "rec" word should be in function declaration

let rec zip (xs, ys) =
    match xs, ys with
        | [], _ | _, [] -> []
        | x :: xs', y :: ys' -> (x, y) :: zip(xs', ys')

// d.

let min (e1 : int) (e2 : int) : int =
    if( e1 > 100 || e2 > 100 || e1 < -100 || e2 < -100 ) then
        failwith "Incorrect parameter"
    else
        if( e1 <= e2 ) then
            e1
        else
            e2 

let rec minList (l : int list) : int =
    match l with
    | [] -> failwith "List is empty"
    | x1 :: x2 :: xs -> minList ((min x1 x2) :: xs)
    | x :: xs -> x

minList [-100; 10; 4; 5; 6; 100; 99; 78]

// Question 4: Option
// a.
let convert (v: bool) : bool option = 
    Some v

convert true
convert false

// b.
let convertToBool (v: bool option) : bool =
    match v with
    | Some x -> x
    | None -> failwith "oops"

convertToBool (Some true)
convertToBool (None)
convertToBool (Some false)

// c.
let rec kleeneNeg (v: bool option) : bool option =
    match v with
    | Some x -> Some (not x)
    | None -> None

kleeneNeg (Some true)
kleeneNeg (Some false)
kleeneNeg None

// d.
let kleeneOr (a: bool option) (b: bool option) : bool option =
    if( a = Some true || b = Some true ) then
        Some true
    elif( a = Some false && b = Some false ) then
        Some false
    else
        None

kleeneOr (Some true) None

// e.
let kleeneImpl (a: bool option) (b: bool option) : bool option =
    kleeneOr (kleeneNeg a) b

kleeneImpl (Some true) (Some false)

// f.

// i.
let rec kleeneToInt (v: bool option) : int =
    match v with
    | None -> 1
    | Some true -> 2
    | Some false -> 0

kleeneToInt (Some true)
kleeneToInt (Some false)
kleeneToInt None

// ii.
let kleeneAnd (a: bool option) (b: bool option) : int =
    min (kleeneToInt a) (kleeneToInt b)

kleeneAnd (Some true) None

// g.

let g x = Option.map not x

g None
// i. bool option -> bool option

// ii. Result is None.

// Was non-trivial question for myself, for some reason.