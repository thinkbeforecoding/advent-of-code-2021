open System.IO

let data =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input/day2.txt")
    |> Array.map (fun s -> 
        match s.Split(' ') with
        | [| cmd; x |] -> cmd, int x
    )

// part 1

// group by direction and sum
let dirs = 
    data
    |> Array.groupBy fst    // group by direction
    |> Array.map (fun (cmd, values) -> cmd, Array.sumBy snd values ) // sum grouped values
    |> Map.ofArray  // create a map


// compute result
dirs["forward"] * (dirs["down"] - dirs["up"])

// part2

type Action =
    | Forward 
    | Down
    | Up

// parse data as actions
let data2 =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/day2.txt")
    |> Array.map (fun s -> 
        match s.Split(' ') with
        | [| "forward"; x |] -> Forward, int x
        | [| "down"; x |] -> Down, int x
        | [| "up"; x |] -> Up, int x
    )

// evolution function
// state contains current x pos, depth and aim
let evolve (pos,depth, aim) (action, x) =
    match action with
    | Down -> (pos, depth, aim+x)  // increase aim
    | Up -> (pos, depth, aim-x)    // decrease aim
    | Forward -> (pos+x, depth + aim*x, aim) // move x, apply aim to depth

// test on example
let (pos,depth,_) =
    [|  
    Forward, 5
    Down, 5
    Forward, 8
    Up, 3
    Down, 8
    Forward, 2 |]
    |> Array.fold evolve (0,0,0)
pos * depth

// do it on data
let p,d,_ =
    data2
    |> Array.fold evolve (0,0,0)

// result
p*d
