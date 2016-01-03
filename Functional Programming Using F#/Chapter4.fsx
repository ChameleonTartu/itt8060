// 4.1
let upto (n : int) : int list =
    if( n > 0 ) then
        [1..1..n]
    else
        failwith "Incorrect parameters"

upto 3

//upto 0

// 4.2
let downto1 (n : int) : int list =
    if( n > 0 ) then
        List.rev [1..n]
    else
        failwith "Incorrect parameters"
 
downto1 5

// 4.3
let evenN (n : int) : int list =
    if( n >= 0 ) then
        [0..2..n]
    else
        failwith "Incorrect parameters"

evenN 10
evenN 0

// 4.4
let rec altsum = function
    | [] -> 0
    | x :: xs -> if (xs = []) then
                    x
                 else
                    let x'::xs' = xs
                    (x - x') + altsum xs'
altsum [2; -1; 3]

// 4.5
let rec rmodd list =
    match list with
    | [] -> []
    | [x] -> [x]
    | x1 :: x2 :: xs -> x1 :: (rmodd xs)

rmodd [1..10]
rmodd [1..9]
rmodd [1..11]

// 4.6
let rec removeEvenNumbers (list : int list) : int list =
    match list with
    | [] -> []
    | x :: xs when x % 2 = 1 -> x :: (removeEvenNumbers xs)
    | x :: xs -> removeEvenNumbers xs  

removeEvenNumbers [1..10]
removeEvenNumbers [-2; 3; 4; 8;  -8; 3; 1; 0]

// 4.7
let rec multiplicity x xs = 
    match xs with
    | [] -> 0
    | x1 :: xs1 when x1 = x -> 1 + multiplicity x xs1
    | x1 :: xs1 -> multiplicity x xs1

multiplicity 'a' ['a'; 'b'; 'n'; 'd'; 'a'; 'f'; 'g'; 'h'; 'j'; 'a']

// 4.8
let split list =
    let rec getEvenIndex list = 
        match list with
        | [] -> []
        | x1 :: x2 :: xs -> x2 :: getEvenIndex xs
        | x1 :: xs -> []

    let rec getOddIndex list =
        match list with
        | [] -> []
        | x1 :: x2 :: xs -> x1 :: getOddIndex xs
        | x1 :: xs -> [x1] 

    (getOddIndex list, getEvenIndex list)

split [1..10]

// 4.9
let rec zip (l1, l2) =
    match l1, l2 with
    | [], [] -> []
    | l1h :: l1t, l2h :: l2t -> (l1h, l2h) :: zip (l1t, l2t)
    | _ -> failwith "Incorrect parameters"

zip ([1..10], [1..10])

// 4.10
let prefix (x : 'a list) (y : 'a list): bool =
    if( List.length x <= List.length y ) then
        let rec prefixEqual x y =
            match x, y with
            | [], ys -> true
            | x1 :: xs, y1 :: ys when x1 = y1 -> true && (prefixEqual xs ys)
            | x1 :: xs, y1 :: ys -> false
        (prefixEqual x y)
    else
        (false)

prefix [1..10] [1..11]
prefix [1..2] [1..8]
prefix [1..9] [1..8]

// 4.11

// 1.
let rec count ((xs, x): int list*int) : int =
    match xs with
    | [] -> 0
    | xh :: xt when xh = x -> 1 + count (xt, x)
    | xh :: xt -> count (xt, x)

count ([1;1;1;2;3;4;5;7;8;9;9;10], 9)

// 2.
let rec insert ((xs, x): int list*int) : int list =
    match xs with
    | [] -> [x]
    | x1 :: (x2 :: _ as xs ) when x1 <= x && x <= x2 -> x1 :: x :: xs
    | x1 :: (x2 :: _ as xs ) -> x1 :: insert (xs, x)
    | x1 :: xs -> [x1; x]

insert ([1; 1; 3; 4; 5; 6; 7; 7; 8; 9], 9)

// 3.
let rec intersect ((x, y): int list*int list) : int list =
    match x, y with
    | ([], _) | (_, []) -> []
    | x :: xs, y :: ys when x < y -> intersect (xs, y :: ys)
    | x :: xs, y :: ys when y < x -> intersect (x :: xs, ys)
    | x :: xs, y :: ys -> x :: intersect (xs, ys)

intersect ([1; 1; 1; 2; 2], [1; 1; 2; 4])

// 4.
let rec plus ((x, y) : int list*int list) : int list =
    match x, y with
    | [], y -> y
    | x, [] -> x
    | x1 :: xs, y1 :: ys when x1 < y1 -> x1 :: plus(xs, y1 :: ys)
    | x1 :: xs, y1 :: ys when y1 < x1 -> y1 :: plus(x1 :: xs, ys)
    | x1 :: xs, y1 :: ys -> x1 :: y1 :: plus(xs, ys)

plus ([1; 1; 2], [1; 2; 4])

// 5.
let rec minus ((x, y) : int list*int list) : int list =
    match x, y with
    | [], y -> []
    | x, [] -> x
    | x1 :: xs, y1 :: ys when y1 > x1 -> x1 :: minus (xs, y1::ys)
    | x1 :: xs, y1 :: ys when y1 < x1 -> minus (x1::xs, ys)
    | x1 :: xs, y1 :: ys -> minus (xs, ys)

minus ([1;1;1;2;2], [1;1;2;3])
minus ([1;1;2;3], [1;1;1;2;2])

// 4.12
let rec sum ((p, xs) : (int -> bool)*int list) : int =
    match xs with
    | [] -> 0
    | xh :: xt when (p xh) -> xh + sum (p, xt)
    | xh :: xt -> sum (p, xt)

let p x = x > 0
sum (p, [1; -1; 2; 3; -4; -5; 6])

// 4.13

// 1.
let minElement (x : int list) : int =
    let current = x.[0]
    let rec min ((x, currentMinimum): int list*int) : int =
        match x with
        | [] -> currentMinimum
        | x1 :: xs when x1 < currentMinimum -> min (xs, x1)
        | x1 :: xs -> min (xs, currentMinimum)
    min (x, current)

// 2.
let rec delete ((a, xs) : int*int list) : int list =
    match xs with
    | [] -> []
    | xh :: xt when xh = a -> delete (a, xt)
    | xh :: xt -> xh :: delete (a, xt)

// 3.
let rec sort (xs : int list) : int list =
    let rec countOccurences (xs, a) =
        match xs with
        | [] -> 0
        | x :: xtail when x = a -> 1 + countOccurences (xtail, a)
        | x :: xtail -> countOccurences (xtail, a)
    let rec createList a l =
        if a = 0 then
            []
        else
            l :: (createList (a - 1) l)
    
    match xs with
    | [] -> []
    | xs -> let minEl = minElement xs
            let count = countOccurences (xs, minEl)
            (createList count minEl) @  sort (delete (minEl, xs))

sort ([1; 2; 4; 5; 6; 1; -1; 2; 4; 5; 10] @ [1..3])

// 4.14

let minimum (l: int list) : int option =
    if( l <> []) then
        let current = l.[0]
        let rec findMin ((l, currentMin) : int list*int) : int =
            match l with
            | [] -> currentMin
            | x :: xs when x < currentMin -> findMin (xs, x)
            | x :: xs -> findMin (xs, currentMin)
        Some (findMin (l, current))
    else
        (None)

minimum []
minimum [1;3;4;-1;-1;0;-20;9]

// 4.15
let revrev (l: 'a list list) : 'a list list =
    let r = List.rev l
    let rec reverse r = 
        match r with
        | [] -> []
        | x :: xs -> (List.rev x) :: reverse xs
    (reverse r)

revrev [[1;2]; [3;4;5]]

// 4.17

let rec P q = function
    | [] -> []
    | x :: xs -> let ys = P q xs
                 if q x then x :: ys else ys @ [x]

// ('a -> bool) -> 'a list -> 'a list

// 4.18
let rec f g = function
    | [] -> []
    | x :: xs -> g x :: f (fun y -> g (g y) ) xs

// ('a -> 'a) -> 'a list -> 'a list


