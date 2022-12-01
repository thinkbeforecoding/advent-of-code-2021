open System
open System.Diagnostics


type Number =
    | Digit of byte
    | Pair of Number * Number



let rec printNum =
    function
    | Digit d -> string d
    | Pair(l,r) -> $"[{printNum l},{printNum r}]"


fsi.AddPrinter printNum

let rec parseNumberloop (s: ReadOnlySpan<char>) =
    let c = s[0]
    if c = '[' then
        let left, lenLeft = parseNumberloop (s.Slice(1))
        let right, lenRight = parseNumberloop (s.Slice(2+lenLeft))
        Pair(left,right), lenLeft+lenRight+3
    else
        Digit(byte c - '0'B), 1
            
let parseNumber (s: string) = parseNumberloop (s.AsSpan()) |> fst
        
parseNumber "[[[[1,2],[1,8]],[[4,3],[8,6]]],[[[5,1],8],[8,1]]]"



    

let data =
    IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input/day18.txt")
    |> Array.map parseNumber

let add x y =
    Pair(x,y)

let x = add (parseNumber "[1,2]")  (parseNumber  "[[3,4],5]")

type Explode =
    | Explode of Number * left: byte * right: byte
    | NoChange of Number

let rec applyLast dl n =
    match n with
    | Digit v -> Digit(v+dl)
    | Pair(l,r) -> Pair(l, applyLast dl r)

let rec applyFirst dr n =
    match n with
    | Digit v -> Digit(v+dr)
    | Pair(l,r) -> Pair(applyFirst dr l, r)


let rec explode depth n  =
    match n with
    | Digit _ -> NoChange n
    | Pair(Digit l,Digit r) when depth = 4 ->
            Explode (Digit 0uy, l, r)
    | Pair(l, r) ->
        match explode (depth+1) l with
        | NoChange nl ->
            match explode (depth+1) r with
            | NoChange nr -> NoChange (Pair(nl,nr))
            | Explode(nr, dl, dr) ->
                Explode(Pair(applyLast dl nl,nr), 0uy, dr)
        | Explode(nl, dl, dr) ->
            Explode(Pair(nl, applyFirst dr r), dl, 0uy)
            

type Split =
    | Split of Number
    | NotSplit of Number

let rec split n =
    match n with
    | Digit v when v < 10uy -> NotSplit n
    | Digit v -> Split ( Pair( Digit(v/2uy), Digit((v+1uy)/2uy) ) )
    | Pair (l,r) ->
        match split l with
        | Split( nl ) -> Split(Pair(nl, r))
        | NotSplit(nl) ->
            match split r with
            | Split(nr) -> Split(Pair(nl,nr))
            | NotSplit(nr) -> NotSplit(Pair(nl,nr))

let rec reduce n =
    match explode 0 n with
    | Explode(exp, _,_) ->
        reduce exp
    | NoChange(n) ->
        match split n with
        | Split sp ->
            reduce sp
        | NotSplit sp ->
            sp
            


parseNumber "[[[[[9,8],1],2],3],4]" |> explode 0
parseNumber "[7,[6,[5,[4,[3,2]]]]]" |> explode 0
            
parseNumber "[[6,[5,[4,[3,2]]]],1]" |> explode 0

parseNumber "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" |> reduce

add (parseNumber "[[[[4,3],4],4],[7,[[8,4],9]]]") (parseNumber "[1,1]") |> reduce

let addReduce x y = add x y |> reduce

let rec magnitude n =
    match n with
    | Digit v -> int v
    | Pair(l,r) -> 3 * magnitude l + 2 * magnitude r


["[1,1]"
 "[2,2]"
 "[3,3]"
 "[4,4]" ]
|> List.map parseNumber
|> List.reduce addReduce

["[1,1]"
 "[2,2]"
 "[3,3]"
 "[4,4]"
 "[5,5]"
 "[6,6]"]
|> List.map parseNumber
|> List.reduce addReduce

["[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
 "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
 "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
 "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
 "[7,[5,[[3,8],[1,4]]]]"
 "[[2,[2,2]],[8,[8,1]]]"
 "[2,9]"
 "[1,[[[9,3],9],[[9,0],[0,7]]]]"
 "[[[5,[7,4]],7],1]"
 "[[[[4,2],2],6],[8,7]]" ]
 |> List.map parseNumber
 |> List.reduce addReduce
 |> magnitude
 
["[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
 "[[[5,[2,8]],4],[5,[[9,9],0]]]"
 "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
 "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
 "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
 "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
 "[[[[5,4],[7,7]],8],[[8,3],8]]"
 "[[9,3],[[9,9],[6,[4,9]]]]"
 "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
 "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"]
|> List.map parseNumber
|> List.reduce addReduce
|> magnitude

data
|> Array.reduce addReduce
|> magnitude

(data,data)
||> Seq.allPairs
|> Seq.map(fun (x,y) -> addReduce x y |> magnitude)
|> Seq.max

