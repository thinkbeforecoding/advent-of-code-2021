open System
let data =
    IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/day6.txt")
    |> (fun s -> s.Split(','))
    |> Seq.map (byte)
    |> Seq.toList

// part 1
// make a fish life iteration
// the order is not important, 
// we insert new fish next to the one who produced it 
let iter (fishes: byte list) = 
    fishes
    |> List.fold (fun result fish -> 
        if fish = 0uy then
            // new fish is ready,
            // put both fish with life 6 and 8 in result list
            6uy :: 8uy :: result
        else
            // fish is just getting older
            // put age-1 in result list
            (fish-1uy) :: result           

    ) [] 


// iterate operation f, n times on fishes data
let rec itern f n fishes =
    if n = 0 then
        fishes
    else
        itern f (n-1) (f fishes)

// test on example
[ 3uy;4uy;3uy;1uy;2uy ]
|> itern iter 80 // do ite 80 times
|> List.length

// result
data
|> itern iter 80 // do ite 80 times
|> List.length


// part 2
// fishes are growing too fast, even with the small sample, 
// I had to stop it before getting a result. It take to much
// memory to keep track of all the fishes

// a better strategy is to see that fishes can only have 9 differents ages
// and all fishes with the same age will do the same action.

// we can represent all fishes as an array of 9 values. 
// Each index contains the number of fishes of that age 

// converts a list of fish to a array of fishes by age
let countFishes (fishes: byte list) =
    // start with an empty array
    let counts = Array.zeroCreate 9
    for fish in fishes do
        // increment the entry for fish age
        counts[int fish] <- counts[int fish] + 1L
    counts

// process one iteration of fishes aging
let iterCount (counts: int64[]) =
    // create a new array for fish ages
    let newCounts = Array.zeroCreate 9
    // all ages from 1..8 get 1day younger
    for age in 1 .. 8 do
        newCounts[age-1] <- counts[age]
    // fishes that were 0day old are now 6, 
    // and should be add to those already counted (those who where 7)
    newCounts[6] <- newCounts[6] + counts[0]
    // fishes that were 0day spawned the same number of 8day old fishes
    newCounts[8] <- counts[0] 
    newCounts

// try on sample
[ 3uy;4uy;3uy;1uy;2uy ]
|>  countFishes
|> itern iterCount 256
|> Array.sum


#time
// try on data (counters have to be int64 to avoid overflow)
data
|> countFishes
|> itern iterCount 256
|> Array.sum

// result is now instant !!
