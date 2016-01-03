// 3.1

// 1.
let (.<) (tupleOne: int*int*string) (tupleTwo: int*int*string) : bool =
    let (h1, m1, f1) = tupleOne
    let (h2, m2, f2) = tupleTwo
    if( f1 < f2 ) then
        (true)
    elif ( f1 = f2 && (h1*60 + m1 <= (h2*60 + m2)) ) then
        (true)
    else
        (false)

(11, 59, "AM") .< (1, 15, "PM")
(11, 59, "AM") .< (11, 58, "AM")

// 2.

type Day = { hours: int; minutes: int; f: string }

let (<.) (day1: Day) (day2: Day): bool =
    let { hours = h1; minutes = m1; f = f1 } = day1
    let { hours = h2; minutes = m2; f = f2 } = day2
    if( f1 < f2 ) then
        (true)
    elif( f1 = f2 && (h1*60 + m1) <= (h2*60 + m2) ) then
        (true)
    else
        (false)

let day1 = {hours = 11; minutes = 59; f = "AM"}
let day2 = {hours = 1; minutes = 15; f = "PM"}
let day3 = {hours = 11; minutes = 58; f = "AM"}
(<.) day1 day2
(<.) day1 day3

// 3.2

// 1.
let (+.) (money1 : int*int*int) (money2 : int*int*int) : string =
    let (p1, s1, pen1) = money1
    let (p2, s2, pen2) = money2
    let m1 = p1*20*12 + s1*12 + pen1
    let m2 = p2*20*12 + s2*12 + pen2
    let m = m1 + m2
    string (m/(20*12), (m/12) % 20, m%12)

// 2.
let (-.) (money1 : int*int*int) (money2 : int*int*int) : string =
    let (p1, s1, pen1) = money1
    let (p2, s2, pen2) = money2
    let m1 = p1*20*12 + s1*12 + pen1
    let m2 = p2*20*12 + s2*12 + pen2
    let m = m1 - m2
    if( m < 0 ) then
        string ( 0, 0, m )
    else
        string ( m/(20*12), (m/12)%20, m%12 )
   
(+.) (1, 2, 3) (1, 2, 10)
(-.) (1, 2, 10) (1, 2, 3)

// 3.3

type Complex = float*float

// 1-2

let (.+.) (c1: Complex) (c2: Complex): Complex = 
    let (a1, b1) = c1
    let (a2, b2) = c2
    (a1 + a2, b1 + b2)

(.+.) (1.0, 2.0) (3.0, 4.0)

let (.-.) (c1: Complex) (c2: Complex) : Complex =
    let (a1, b1) = c1
    let (a2, b2) = c2
    (a1 - a2, b1 - b2)

(.-.) (2.0, 4.5) (1.25, -7.39)

let (.*.) (c1: Complex) (c2: Complex) : Complex =
    let (a1, b1) = c1
    let (a2, b2) = c2
    (a1*a2 - b1*b2, b1*a2 + a1*b2)

(.*.) (1.0, 2.0) (1.35, 9.5)

let (./) (c1: Complex) (c2: Complex) : Complex = 
    let (a2, b2) = c2
    (.*.) c1 (a2/(a2*a2+b2*b2), -b2/(a2*a2 + b2*b2))

(./) (1.0, 1.0) (1.0, 1.0)
// 3.
let (./.) (c1: Complex) (c2: Complex) : Complex = 
    let (a2, b2) = c2
    let d = a2*a2 + b2*b2
    (.*.) c1 (a2/d, -b2/d)

(./.) (1.0, 1.0) (1.0, 1.0)

// 3.4

// 1.
type StraightLine = float*float

// 2.
let mirrorX (s: StraightLine) : StraightLine =
    let (a, b) = s
    (a, -b)

let mirrorY (s: StraightLine) : StraightLine =
    let (a, b) = s
    (-a, b)

// 3.
let stringRepresentation (s: StraightLine) : string =
    let (a, b) = s
    if( a = 0.0 && b = 0.0 ) then
        "y = 0"
    elif( a = 0.0 && b <> 0.0 ) then
        "y=" + string b
    elif( a <> 0.0 && b = 0.0 ) then
        "y=" + string a + "x"
    elif( b < 0.0 ) then
        if( a = 1.0 ) then
            "y=x" + string b
        elif( a = -1.0 ) then
            "y=-x" + string b
        else
            "y=" + string a + "x" + string b
    else
        if( a = 1.0 ) then
            "y=x+" + string b
        elif( a = -1.0 ) then
            "y=-x+" + string b
        else
            "y=" + string a + "x+" + string b

stringRepresentation (1.0, 2.0)
stringRepresentation (-1.0, 2.0)
stringRepresentation (-1.5, 0.0)

// 3.5

type Solution = | TwoRoots of float*float
                | OneRoot of float
                | NoRoots

let solve ((a, b, c): float*float*float) : Solution =
    let D = b*b - 4.0*a*c
    if( a = 0.0 || D < 0.0 ) then
        NoRoots
    elif( D = 0.0 ) then
        OneRoot (-b/(2.0*a))
    else
        TwoRoots ((-b + System.Math.Sqrt(D))/(2.0*a), (-b - System.Math.Sqrt(D))/(2.0*a))

solve (1.0, 2.0, 1.0)


// 3.6
type DayTime = | AM | PM

type Time = { Hours: int; Minutes: int; F: DayTime }

let (.<.) (one: Time) (two: Time) : bool =
    if( one.F < two.F ) then
        (true)
    elif( one.F = two.F && (one.Hours*60 + one.Minutes) <= (two.Hours*60 + two.Minutes) ) then
        (true)
    else
        (false)

(.<.) {Hours=11; Minutes=59; F=AM} {Hours=1; Minutes=40; F=PM}
(.<.) {Hours=11; Minutes=59; F=AM} {Hours=11; Minutes=58; F=AM}

// 3.7

type Shape = | Circle of float
             | Square of float
             | Triangle of float*float*float

let isShape = function
    | Circle r -> r > 0.0
    | Square a -> a > 0.0
    | Triangle (a, b, c) -> a > 0.0 && b > 0.0 && c > 0.0 &&
                            b < c + a && c < a + b && a < b + c

let  area  x =
    match x with
    | _ when not (isShape x) -> failwith "not a legal shape"
    | Circle r -> System.Math.PI * r * r
    | Square a -> a * a
    | Triangle (a,b,c) -> let s = (a + b + c)/2.0
                          sqrt( s*(s - a)*(s - b)*(s - c) )

area (Triangle (3.0, 4.0, 5.0))
// Stoped due to error.
//area (Square -1.0)