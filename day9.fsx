let data =
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input/day9.txt")
    |> Array.map (fun line -> line |> Seq.map (fun c -> byte(int c - int '0')) |> Seq.toArray)

//let data =
//    [| "2199943210"
//       "3987894921"
//       "9856789892"
//       "8767896789"
//       "9899965678" |]
//    |> Array.map (fun line -> line |> Seq.map (fun c -> byte(int c - int '0')) |> Seq.toArray)

let printData (data: byte[][]) =
    for line in data do
        for d in line do
            if d = 255uy then
                printf "."
            else
                printf "%d" d
        printfn ""

printData data
let w = data.[0].Length
let h = data.Length

let getData x y =
    if x < 0 || x >= w  || y < 0 || y >= h then
        10uy
    else 
        data[y][x]

let getIfMin x y =
    let up = getData x (y-1)
    let down = getData x (y+1)
    let left = getData (x-1) y
    let right = getData (x+1) y
    let mid = data[y][x]
    let isMin = mid < up && mid < down && mid < left && mid < right
    if isMin then 
        int mid+1
    else
        0

let mins = 
    data
    |> Array.mapi(fun y line -> line |> Array.mapi (fun x _ -> getIfMin x y))

mins |> Array.sumBy Array.sum

let minsxy =
    data
    |> Array.mapi(fun y line -> line |> Array.mapi (fun x _ -> if getIfMin x y > 0 then Some(x,y) else None) |> Array.choose id)
    |> Array.concat

open System.Collections.Generic
let getBassinSize(x,y) =
    let stack = Stack()
    stack.Push(x,y)
    let mutable size = 0
    while stack.Count > 0 do
        let x,y = stack.Pop()
        if getData x y < 9uy then
            data[y][x] <- 0xffuy
            size <- size+1
        let left = getData (x-1) y
        if left < 9uy then
            stack.Push (x-1,y)
        let right = getData (x+1) y
        if right < 9uy then
            stack.Push(x+1,y)
        let up = getData x (y-1)
        if up < 9uy then
            stack.Push(x,y-1)
        let down = getData x (y+1)
        if down < 9uy then
            stack.Push(x,y+1)
    size
            
let results = 
    minsxy
    |> Array.map getBassinSize


results |> Array.sortByDescending id |> Array.take 3 |> Array.fold (*) 1




