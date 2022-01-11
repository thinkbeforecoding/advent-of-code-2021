open System

type Fold =
    | FoldX
    | FoldY

type Paper = 
    { Dots: byte[]
      Width: int
      Height: int
      Folds : (Fold * int) list }

    member this.dot(c,l) =
        if c >= 0 && c < this.Width && l >= 0 && l < this.Height then
            this.Dots[c + this.Width * l]
        else
            0uy

    member this.Count =
        this.Dots |> Seq.map int |> Seq.sum
        


let print (p: Paper) =
    [ ""
      for y in 0 .. p.Height-1 do
        p.Dots[y*p.Width .. (y+1)*p.Width-1] |> Array.map (function 0uy -> '.' | _ -> '#') |> String
      $"{p.Folds.Length}"
    ] |> String.concat "\n"

fsi.AddPrinter print

let lines = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input/day13.txt")
let test = [|
    "6,10"
    "0,14"
    "9,10"
    "0,3"
    "10,4"
    "4,11"
    "6,0"
    "6,12"
    "4,1"
    "0,13"
    "10,12"
    "3,4"
    "3,0"
    "8,4"
    "1,10"
    "2,14"
    "8,10"
    "9,0"
    ""
    "fold along y=7"
    "fold along x=5"
    |]

let paper lines =
    let points = 
        lines
        |> Array.takeWhile (String.IsNullOrEmpty >> not)
        |> Array.map (fun l -> let [|x;y|] = l.Split(',') in int x, int y)

    let folds =
        lines
        |> Array.skipWhile (String.IsNullOrEmpty >> not)
        |> Array.skip 1
        |> Array.map (fun l -> 
            match l.Split('=') with
            | [|"fold along x";x|] -> FoldX, int x
            | [|"fold along y";y|] -> FoldY, int y)
        |> Array.toList

    let w = (points |> Array.map fst |> Array.max) + 1
    let h = (points |> Array.map snd |> Array.max) + 1

    let dots = Array.zeroCreate (w * h)
    for x,y in points do
        dots[x+y*w] <- 1uy
    { Dots = dots
      Width = w
      Height = h
      Folds = folds }




let fold (p: Paper) =
    match p.Folds with
    | (FoldX, fx) :: rest ->
        let w = fx
        let h = p.Height
        let dots = Array.zeroCreate (w*h)

        for l in 0 .. h-1 do
            for c in 0 .. w-1 do
                dots[l*w+c] <- p.dot(c,l) ||| p.dot(2*fx-c,l)
        

        { Dots = dots
          Width = w
          Height = h
          Folds = rest
        }
    | (FoldY, fy) :: rest ->
        let w = p.Width
        let h = fy
        let dots = Array.zeroCreate (w*h)

        for l in 0 .. h-1 do
            for c in 0 .. w-1 do
                dots[l*w+c] <- p.dot(c,l) ||| p.dot(c, 2*fy-l)

        

        { Dots = dots
          Width = w
          Height = h
          Folds = rest
        }
    | _ -> p

let count (p: Paper) = p.Count

lines
|> paper
|> fold
|> count


lines
|> paper
|> fold
|> fold
|> fold
|> fold
|> fold
|> fold
|> fold
|> fold
|> fold
|> fold
|> fold
|> fold

