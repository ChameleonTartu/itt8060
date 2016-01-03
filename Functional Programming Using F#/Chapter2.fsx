// 2.1
let f (n:int) :bool = 
    if( n % 2 = 0 && n % 3 = 0 && n % 5 <> 0 ) then
        (true)
    else
        (false)
f(24)
f(27)
f(29)
f(30)

// 2.2
let rec pow = function
    | (s, 0) -> ""
    | (s, n) when n < 0 -> failwith "Wrong arguments"
    | (s, n) -> s + pow (s, n-1)

pow ("s", 3)

// 2.3
let isIthChar ((str, i, ch): string*int*char) :bool =
    if( str.[i] = ch ) then
        true
    else
        false 

isIthChar ("string", 0, 's')
isIthChar ("string", 1, 's')

// 2.4

let rec occFromIth ((str,i,ch): string*int*char): int =
    match (str, i, ch) with
    | (str, i, ch) when i > str.Length -> 0
    | (str, i, ch) when str.[i] = ch -> 1 + occFromIth (str, i + 1, ch)
    | (str, i, ch) -> occFromIth (str, i + 1, ch)

// 2.5

let rec occInString ((str, ch): string*char): int =
    match (str, ch) with
    | (str, ch) when str.Length = 0 -> 0
    | (str, ch) when str.[0] = ch -> 1 + occInString ( str.[1..str.Length - 1], ch)
    | (str, ch) -> occInString (str.[1..str.Length - 1], ch)

occInString ("policy or not policy yes", 'y')

// 2.6

let notDivisible ((d,n): int*int) :bool =
    if( n % d = 0 ) then
        false
    else
        true

notDivisible(2,5)
notDivisible(3,9)

// 2.7
// 1.
let rec test ((a,b,c): int*int*int): bool = 
    match (a,b,c) with
    | (a,b,c) when a < b -> notDivisible(a,c) && test(a+1,b,c)
    | (a,b,c) when a = b -> notDivisible(a,c)
    | (a,b,c) -> failwith "Incorrect data"

test (3,6,14)

// 2.
let prime (n:int): bool =
    if( test(2,n-1,n) ) then
        true
    else
        false

prime 9
prime 7
prime 5
prime 27 


// 3.
let nextPrime (p: int): int = 
    let rec findPrime (p: int): int = 
        if( prime p = true ) then
            p
        else
            findPrime (p + 1)
    findPrime (p+1)

nextPrime 9
nextPrime 13
nextPrime 100

// 2.8
let rec bin ((n,k): int*int): int =
    match (n, k) with
    | (n, 0) -> 1
    | (n, k) when n = k -> 1
    | (n, k) when n <> 0 && k <> 0 && n > k -> bin(n-1, k-1) + bin(n-1, k)
    | _ -> failwith "Incorrect parametets"

bin (4,2)
bin (6,3)
bin (8,4)


// 2.9
let rec ff = function
    | (0, y) -> y
    | (x, y) -> ff(x-1, x*y)
  
//ff (-10, 2)
ff (2,3)
ff (2,4)


// 2.10
let testt (c,e) = if c then e else 0


let rec fact = function
    | 0 -> 1
    | n -> n * fact (n-1)

// Termination
//testt(false, fact (-1))

if false then fact -1 else 0


// 2.11
let VAT (n: int) (x: float) = 
    (1.0 + float n / 100.0) * x

let unVAT (n: int) (x: float) =
    x / (1.0 + float n / 100.0)

let vat = VAT 10 100.0
let unvat = unVAT 10 vat

// 2.12

let min (f: int -> int ) : int = 
    let rec find (f: int -> int) (n: int) =
        if( f n = 0 ) then
            n
        else
            f (n + 1)
    find f 0  


// 2.13



// 1.2

let h (x, y) = System.Math.Sqrt( x*x + y*y ) 
h (5.0, 3.0)
h (4.0, 3.0)