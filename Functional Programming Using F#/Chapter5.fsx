// 5.1
let filter (p: 'a -> bool) (l: 'a list) : 'a list =
    let f x b =
        if( p x ) then
            x :: b
        else
            b
    List.foldBack f l []

filter (fun x -> x > 0) [1; -1; 2; 4; -1; 0; -9; 10]

// 5.2
// fold
let revrevFold (l : 'a list list) : 'a list list =
    List.fold (fun s x -> [(List.rev x)] @ s) [] l

revrevFold [[1;2]; [3;4;5]]

// foldBack
let revrevFoldBack (l: 'a list list) : 'a list list =
    List.foldBack (fun x s -> s @ [(List.rev x)]) l []

revrevFoldBack [[1;2]; [3;4;5]]

// 5.3
// fold
let sumFold ((p, xs): (int->bool)*int list) : int =
    let f s x =
        if p x then
            x + s
        else
            s
    List.fold f 0 xs

sumFold ((fun x -> x > 0), [1; -1; 2; 4; -1; 0; -9; 10])

// foldBack
let sumFoldBack ((p, xs): (int->bool)*int list) : int =
    let f x s =
        if p x then
            x + s
        else
            s
    List.foldBack f xs 0

sumFoldBack ((fun x -> x > 0), [1; -1; 2; 4; -1; 0; -9; 10])

// 5.4


// 1.
let downto1 f n e =
    if ( n <= 0 ) then
        e
    else
        let l = [1..n]
        List.foldBack f l e

downto1 (fun a x -> x + a) 10 0

// 2.
let fact (n: int) =
    downto1 (*) n 1

fact 4

// 3.
let createList g n =
    let f g x e =
        g(x) :: e
    downto1 (f g) n []

createList (fun x -> x + 1) 10

// ...