let data =
    System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/input/day7.txt")
    |> (fun s -> s.Split(','))
    |> Array.map (int)

// part 1

// the distance from x to y
let dist x y = abs(x-y)

// the total fuel to move all crabs to position n
// the sum of distance of each crab to n
let totalFuel n (crabs: int[]) =
    Array.sumBy (dist n) crabs 

// compute the full range to explore
let range (crabs: int[]) =
    let min = Array.min crabs
    let max = Array.max crabs
    [| min .. max |]

// find the minimum of total fuel for each value in the range
let leastFuel (crabs: int[]) =
    let leastPos =
        range crabs
        |> Array.minBy (fun n -> totalFuel n crabs) 
    leastPos, totalFuel leastPos crabs
    

// test on example
[|16;1;2;0;4;2;7;1;2;14|]
|>  leastFuel

// result
data
|> leastFuel


// part2

// Now, computing the fuel is a bit more complicated
// move 1 -> 1
// move 2 -> 1 + 2
// move 3 -> 1 + 2 + 3 
// move n -> 1 + 2 + 3 + ... + n 
// this is known as the Gauss Formula 
// (wheter it's true or not https://hsm.stackexchange.com/questions/384/did-gauss-find-the-formula-for-123-ldotsn-2n-1n-in-elementary-school)
// and its value is n*(n+1)/2

// Compute the Gauss formula for n (using right shift by 1 to divide by 2) 
let gauss n = n * (n+1) >>> 1

// distance between x and y using gauss formula 
// to compute fuel on this distance 
let gdist x y = gauss (abs (x-y)) 

// test on a single example
gdist 16 5

// sum all the fuel to move to n for all crabs
let gTotalFuel n (crabs: int[]) =
    Array.sumBy (gdist n) crabs 

// find the minimal fuel for all positions in range
let gLeastFuel (crabs: int[]) =
    let leastPos =
        range crabs
        |> Array.minBy (fun n -> gTotalFuel n crabs) 
    leastPos, gTotalFuel leastPos crabs

// test on sample
[|16;1;2;0;4;2;7;1;2;14|]
|>  gLeastFuel


// result on data
data
|> gLeastFuel