let data =
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input/day11.txt")
    |> Seq.collect (fun line -> line |> Seq.map (fun c -> int c - int '0' |> byte)  )
    |> Seq.toArray

let inc (data: byte[]) =
    data |> Array.map ((+) 1uy)

let get x y (data: byte[]) =
    if x < 0 || x > 9 || y < 0 || y > 9 then
        0uy
    else
        data[x+y*10]

let getScore x y data =
    if get x y data > 9uy then
        1uy
    else
        0uy

let flash (data: byte[]) =
    let newData = Array.zeroCreate<byte> data.Length
    let mutable flashes = 0
    for y in 0 .. 9 do
        for x in 0 .. 9 do
            newData[x+y*10] <-
                match get x y data with
                | 0uy -> 0uy
                | v when v > 9uy ->
                    flashes <- flashes+1
                    0uy
                    

                | v ->
                    v + getScore (x-1) (y-1) data
                      + getScore x (y-1) data
                      + getScore (x+1) (y-1) data
                      + getScore (x+1) y data
                      + getScore (x+1) (y+1) data
                      + getScore x (y+1) data
                      + getScore (x-1) (y+1) data
                      + getScore (x-1) y data
            
    newData, flashes


let rec flashAll data flashes =
    let newData, newFlashes = flash data
    if newFlashes = 0 then
        newData, flashes
    else
        flashAll newData (flashes+newFlashes)

let rec step n data flashes =
    if n = 0 then
        data, flashes
    else
        let newData, newFlashes = flashAll (inc data) flashes
        step (n-1) newData newFlashes
        

let printR(data: byte[], flashes: int) =
    data
    |> Array.chunkBySize 10
    |> Array.map (fun line -> line |> Seq.map string |> String.concat "")
    |> String.concat "\n"
    |> fun r -> "\n" + r + "\n" + string flashes

fsi.AddPrinter printR
    

let test =
    [| "5483143223"
       "2745854711"
       "5264556173"
       "6141336146"
       "6357385478"
       "4167524645"
       "2176841721"
       "6882881134"
       "4846848554"
       "5283751526" |]
    |> Seq.collect (fun line -> line |> Seq.map (fun c -> int c - int '0' |> byte)  )
    |> Seq.toArray
step 100 test 0
step 100 data 0


let rec findStep n data =
    let newData, newFlashes = flashAll (inc data) 0
    if newFlashes = 100 then
        n
    else
        findStep (n+1) newData

findStep 1 test
findStep 1 data

