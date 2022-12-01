let test = 
    [| "1163751742"
       "1381373672"
       "2136511328"
       "3694931569"
       "7463417111"
       "1319128137"
       "1359912421"
       "3125421639"
       "1293138521"
       "2311944581" |]

let data =
    System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "/input/day15.txt")


let manhattan (x1,y1) (x2,y2) =
    abs (x2-x1) + abs (y2-y1)

type Map =
    { Width: int
      Map: byte[] }

    member this.End = this.Width-1, this.Width-1

    member this.Item(x,y) =
        if x >= 0 && x < this.Width && y >= 0 && y < this.Width then
            int this.Map[y*this.Width+x]
        else
            100000

    member this.Point(x,y, dist) =
        let value = this[x,y]
        dist + value + manhattan (x,y) (this.End), ((x,y), dist + value)





let parse (data: string[])  =
    let w = data.Length
    let map = Array.zeroCreate (w*w)
    for l in 0 .. w-1 do
        let line = data[l]
        for c in 0 .. w-1 do
            map[l*w+c] <- (int line[c]) - int '0' |> byte
    { Width = w; Map = map }

    
let prepare map =
    let start = 0,0
    let dest = map.Width-1,map.Width-1
    map , Map.ofList [(0,0), (manhattan start dest, 0)] , set  [ manhattan start dest , (start, 0) ]
    



let point (_,(p,_)) = p
let tdst (d,_) = d
let dst(_,(_,d)) = d


let rec tryFindPath (m: Map) visited toVisit =
    let totalDist, ((x,y), dist) = Set.minElement toVisit

    if (x,y) = m.End then
        dist
    else

        let top = m.Point(x,y-1, dist)
        let bottom = m.Point(x,y+1, dist)
        let left = m.Point(x-1,y, dist)
        let right = m.Point(x+1,y, dist)

        let addIfNotVisited info toVisit =
            let p = point info
            if fst info > 10000 then
                toVisit
            else

                match Map.tryFind p visited with
                | Some (td, d) when td > tdst info -> 
                    Set.add info toVisit
                | _ -> Set.add info toVisit

        let ifBettter (tdist, (p, dist)) visited =
            if tdist > 10000 then
                visited
            else
                match Map.tryFind p visited with
                | Some (td,d) when td > tdist ->
                    Map.add p (tdist, dist) visited
                | _ -> visited

        let newtoVisit =
            Set.remove (totalDist,((x,y),dist)) toVisit
            |> addIfNotVisited top
            |> addIfNotVisited bottom
            |> addIfNotVisited left
            |> addIfNotVisited right

        let newVisited =
            visited
            |> ifBettter top
            |> ifBettter bottom
            |> ifBettter left 
            |> ifBettter right

        tryFindPath m newVisited newtoVisit
            

parse test
|> prepare
|||> tryFindPath 


parse data
|> prepare
|||> tryFindPath



[| "11111"
   "99991"
   "11112"
   "29999"
   "11111"
   |]
|> parse 
|> prepare
|||> tryFindPath

    
let grow map =
    let w = map.Width * 5
    { Width = w
      Map = 
        let m = Array.zeroCreate (w * w)
        for ld in 0 .. 4 do
            for cd in 0 .. 4 do
                for l in 0 .. map.Width-1 do
                    for c in 0 .. map.Width-1 do
                        m[(ld*map.Width+l)*w + (cd*map.Width+c)] <- 
                            let v = map.Map[l * map.Width + c] 
                            ((v+byte ld+byte cd-1uy)%9uy)+1uy
        m
    }
[| "11111"
   "99991"
   "11112"
   "29999"
   "11111"
   |]
|> parse 
|> grow

    

parse test
|> grow
|> prepare
|||> tryFindPath 


parse data
|> grow
|> prepare
|||> tryFindPath