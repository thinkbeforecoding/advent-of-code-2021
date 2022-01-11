open System.IO

// model Point / Segment
[<Struct>]
type Point = { x: int; y: int}

[<Struct>]
type Segment = { p1: Point; p2: Point}

module Point =
    let parse (txt: string) =
        let [|x;y|] = txt.Split(',')
        { x = int x; y = int y}

    let toString p =
        sprintf $"{p.x},{p.y}"

module Segment =
    let parse (line: string) =
        let [| p1; p2 |] =line.Split(" -> ")
        { p1 = Point.parse p1; p2 = Point.parse p2}

    let toString s =
        sprintf $"{Point.toString s.p1} -> {Point.toString s.p2}"


fsi.AddPrinter(Point.toString)
fsi.AddPrinter(Segment.toString)

// load and parse data
let data =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input/day5.txt")
    |> Array.map Segment.parse

// part 1

// check whether segement is horizontal
let isHorizontal s = s.p1.y = s.p2.y 
// check whether segement is vertical
let isVertical s = s.p1.x = s.p2.x

// mark vent in array
let markVent s (vent: int[]) =
    if isHorizontal s then
        // segment is horizontal, iterate on x, from min to max
        let y = s.p1.y
        let minx = min s.p1.x s.p2.x
        let maxx = max s.p1.x s.p2.x
        for x in minx .. maxx do
            vent[x+y*1000] <- vent[x+y*1000] + 1
    elif isVertical s then
        // segment is vertial, iterate on y, from min to max
        let x = s.p1.x
        let miny = min s.p1.y s.p2.y
        let maxy = max s.p1.y s.p2.y
        for y in miny .. maxy do
            vent[x+y*1000] <- vent[x+y*1000] + 1
    else
        // part 2
        // segment is diagonal
        let dx = s.p2.x - s.p1.x // change in x
        let dy = s.p2.y - s.p1.y // change in y

        let len = abs dx        // total number of steps (should be equalt to abs dy)
        let stepx = sign dx     // x direction of 1 step
        let stepy = sign dy     // y direction of 1 step

        // iterate for whole length
        for i in 0 .. len do
            let x = s.p1.x + stepx * i  // x offset from start
            let y = s.p1.y + stepy * i  // y offset from start
            vent[x+1000*y] <- vent[x+1000*y] + 1

// mark vents for all segments in data
let vents =
    let v = Array.zeroCreate (1000*1000)
    for s in data do
        markVent s v
    
    v

// count entries with 2 occurence or more
vents |> Array.sumBy (fun x -> if x >= 2 then 1 else 0)
