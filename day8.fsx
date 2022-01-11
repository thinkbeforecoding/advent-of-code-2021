
let data =
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input/day8.txt")
    |> Array.map (fun line ->
        let [| inputs; outputs |] = line.Split(" | ")
        inputs.Split(' '), outputs.Split(' ')
    )

// check if string has givent length 
let hasLength n (x:string) = x.Length = n

// check if specific number by segment count
let is1 = hasLength 2
let is4 = hasLength 4
let is7 = hasLength 3
let is8 = hasLength 7

let outputs =
    data |> Array.map snd

// result
outputs
|> Array.collect id // flatten all lists
|> Array.filter (fun x -> is1 x || is4 x || is7 x || is8 x) // filter specific number
|> Array.length // count remaining

// part 2
// looking on sample
// acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab
// ab is necessarily '1'. but we don't know in which order
// dab is '7'. d is the top segement
// eafb is '4'. e and f ar top left, and middle segemnts
// there a 3 digits with 5 segments : '2', '3', '5'
// digit '2' must have one of the '1' a/b segments, and one of the '4' e/f segements, it is gcdfa
// digit '3' must have both '1' a/b segments, and one of the '4' e/f segements, it is  fbcad
// digit '5' must have one of the '1' a/b segments, and both '4' e/f segments, is is cdfbe
// here '5' contains b, this is the bottom right segment, and a is the top right segment
// here '3' contains the middle segement of '4', so f is the middle segment
// '5' also contains top left segment, e (whichi is also part of '4')
// the remaining segment on '3' is the bottom segement, c
// digit '2' contains the bottom left segment, g, the remaining one





let hasSignal (s: char) (x: string) = x.Contains(s)

let hasAll (s: string) (x: string) = s |> Seq.forall (fun s -> hasSignal s x)
let hasExactlyOneOf (s: string) (x: string) =
    (s |> Seq.sumBy (fun c -> if hasSignal c x then 1 else 0)) = 1

let (--) (dx: string) (dy: string) = dx |> Seq.filter (fun s -> not (hasSignal s dy)) |> Seq.toArray |> System.String

let ( ** ) (dx: string) (dy: string) = dx |> Seq.filter (fun s -> (hasSignal s dy)) |> Seq.toArray |> System.String

let (==) x y = 
    (x ** y) = x && (y ** x) = y

let find1 = Array.find is1 
let find4 = Array.find is4
let find7 = Array.find is7
let find8 = Array.find is8

let is2 d1 ud4 (d: string) =
    d.Length=5 &&
    hasExactlyOneOf d1 d && hasExactlyOneOf ud4 d

let find2 d1 ud4 = Array.find (is2 d1 ud4)

let is3 d1 ud4 (d: string)=
    d.Length=5 &&
    hasAll d1 d && hasExactlyOneOf ud4 d

let find3 d1 ud4 = Array.find (is3 d1 ud4)

let is5 d1 ud4 (d: string) =
    d.Length=5 &&
    hasExactlyOneOf d1 d && hasAll ud4 d

let find5 d1 ud4 = Array.find (is5 d1 ud4)

let is6 topRight d =
    hasLength 6 d &&  not (hasAll topRight d)

let is9 bottomLeft d =
    hasLength 6 d && not (hasAll bottomLeft d)
    
let is0 topRight bottomLeft d =
    hasLength 6 d && hasAll (topRight + bottomLeft) d

let find6 topRight =
    Array.find (is6 topRight)

let find9 bottomLeft  =
    Array.find (is9 bottomLeft)

let find0 topRight bottomLeft  =
    Array.find (is0 topRight bottomLeft)

let processLine (input, output: string[]) =
    let d1 = find1 input
    let d4 = find4 input
    let d7 = find7 input
    let d8 = find8 input

    let ud4 = d4 -- d1

    let d2 = find2 d1 ud4 input


    let d3 = find3 d1 ud4 input



    let d5 = find5 d1 ud4 input


    let top = (d7 -- d1)
    let bottomRight = (d5 ** d1)
    let topRight = (d2 ** d1)
    let mid = ((d3 ** d2 ** d5) -- d7) ** d4
    let bottom = (d3 ** d2 ** d5) -- d7 -- d4
    let topLeft = (d5 -- d3)
    let bottomLeft = (d2 -- d3)


    let d6 = find6 topRight input
    let d9 = find9 bottomLeft input
    let d0 = find0 topRight bottomLeft input

    let digits = [| d0; d1; d2; d3; d4; d5; d6; d7; d8; d9 |]

    let decode x =
        digits |> Array.findIndex (fun v ->  v == x) 

    (decode output[0]) * 1000 + (decode output[1]) * 100 + (decode output[2]) * 10 + decode output[3]


let input = [| "acedgfb"; "cdfbe"; "gcdfa"; "fbcad"; "dab"; "cefabd"; "cdfgeb"; "eafb"; "cagedb"; "ab" |]
let output = [| "cdfeb"; "fcadb"; "cdfeb"; "cdbaf" |]

// test with example
processLine (input, output)

let input,output = data[4]
processLine data[4]

data
|> Array.map processLine
|> Array.sum


