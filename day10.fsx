let data =
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input/day10.txt")
    |> Array.map (fun line -> line |> Seq.toList )

let rec findError line stack =
    match line, stack with
    | '(' :: rest, _ -> findError rest ('(' :: stack) 
    | '[' :: rest, _ -> findError rest ('[' :: stack)
    | '{' :: rest, _ -> findError rest ('{' :: stack)
    | '<' :: rest, _ -> findError rest ('<' :: stack)
    | ')' :: rest, '(' :: reststack 
    | ']' :: rest, '[' :: reststack 
    | '}' :: rest, '{' :: reststack 
    | '>' :: rest, '<' :: reststack -> findError rest reststack
    | ')' :: _, _ -> 3
    | ']' :: _, _ -> 57
    | '}' :: _, _ -> 1197
    | '>' :: _, _ -> 25137
    | _ -> 0

data
|> Array.sumBy (fun line -> findError line [])


let rec score stack s =
    match stack with
    | '(' :: rest -> score rest (s * 5L + 1L)
    | '[' :: rest -> score rest (s * 5L + 2L)
    | '{' :: rest -> score rest (s * 5L + 3L)
    | '<' :: rest -> score rest (s * 5L + 4L)
    | _ -> s

let rec correctIncomplete line stack =
    match line, stack with
    | '(' :: rest, _ -> correctIncomplete rest ('(' :: stack) 
    | '[' :: rest, _ -> correctIncomplete rest ('[' :: stack)
    | '{' :: rest, _ -> correctIncomplete rest ('{' :: stack)
    | '<' :: rest, _ -> correctIncomplete rest ('<' :: stack)
    | ')' :: rest, '(' :: reststack 
    | ']' :: rest, '[' :: reststack 
    | '}' :: rest, '{' :: reststack 
    | '>' :: rest, '<' :: reststack -> correctIncomplete rest reststack
    | ')' :: _, _ 
    | ']' :: _, _ 
    | '}' :: _, _ 
    | '>' :: _, _ -> 0L
    | [], _ -> score stack 0L
    | _ -> 0L

// correctIncomplete (Seq.toList "<{([{{}}[<[[[<>{}]]]>[]]") []

let scores = 
    data 
    |> Array.map (fun line -> correctIncomplete line []) 
    |> Array.filter (fun x -> x <> 0)
    |> Array.sort

scores[scores.Length/2+1 ]

