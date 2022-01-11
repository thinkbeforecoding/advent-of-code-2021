let data = 
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input/day1.txt")
    |> Array.map int

// part 1
// pairwise returns an array of all adjascent pairs
data
|> Array.pairwise   // group with next
|> Array.sumBy (fun (x, y) -> if y > x then 1 else 0 )  // count when y > x

// part 2
// Use window to get elements by 3, and sum them
data
|> Array.windowed 3         // window by 3
|> Array.map Array.sum      // sum each window
|> Array.pairwise           // group with next
|> Array.sumBy (fun (x, y) -> if y > x then 1 else 0 ) // count when y > x
