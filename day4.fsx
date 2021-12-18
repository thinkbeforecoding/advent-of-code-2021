open System.IO
open System


// A board as a 1000*1000 array
type Board = int[][]

let data =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input/day4.txt")

// part 1

// extract numbers from data
let numbers = 
    data[0].Split(',')
    |> Array.map int
    |> Array.toList

// extract boards from data
let boards =
    data[1..]
    |> Array.chunkBySize 6
    |> Array.map(fun b-> 
        b[1..]
        |> Array.map (fun l ->
            l.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int
        )
    )

// we mark value by using binary not
// this way, marked values are always < 0
// and it also works for 0 -> -1
// if value is same, invert it, otherwise, keep value
let markValue num x =
    if num = x then
        ~~~x
    else
        x

// mark cells in board that are equal to num
let markBoard num (board: Board) : Board =
    board
    |> Array.map (Array.map (markValue num))
    
// mark all boards with num
let markBoards num (boards: Board[]) : Board[] =
    boards
    |> Array.map (markBoard num)

// test whether value x is marked
let isMarked x = x < 0

// test whether line n is full
let testLine (board : Board) n =
    Array.forall isMarked board[n]

// test whether column n is full
let testColumn (board: Board) n =
    board
    |> Array.forall (fun line -> isMarked line[n] )

// test board (test lines/columns for 0 to 4)
let testBoard (board: Board) =
    [|0..4|] |> Array.exists (fun n -> testLine board n || testColumn board n)

// try to find a winning board if any
let tryFindBoard (boards: Board[]) =
    boards
    |> Array.tryFind testBoard

// test marks
markBoards numbers[0] boards

// recursively draw number, markboard and try to find winner
let rec findWinner nums boards =
    match nums with
    | [] -> failwith "Not found"
    | num :: rest -> 
        printfn "%d" num
        // mark all boards
        let markedBoards = markBoards num boards
        // look for winner
        match tryFindBoard markedBoards with
        | None ->
            // not found, try with next number
             findWinner rest markedBoards
        | Some winner ->
            // found, return last number and winner board
            num,winner 

let num,winner = findWinner numbers boards 
// print the board
let printBoard (board : Board) =
    for l in 0..4 do
        for c in 0..4 do
            let v = board[l][c]
            if v < 0 then
                printf " x%2d" (~~~v)
            else
                printf "  %2d" v
        printfn ""

printBoard winner

// compute boad score (sum unmarked values)
let boardScore (board : Board) =
    board
    |> Array.sumBy (Array.sumBy(fun x -> if isMarked x then 0 else x))

// result
boardScore winner * num

// part 2

// recursively try to find looser by filtering out winning boards
let rec findLooser nums boards =
    match nums with
    | [] -> failwith "Not found"
    | num :: rest -> 
        printfn "%d" num
        // mark boards
        let markedBoards = markBoards num boards
        // filter out winning boards
        let remainingBoards = markedBoards |> Array.filter (not << testBoard)

        if Array.isEmpty remainingBoards then
            // all boards have won, there was only 1 board remaining
            num, Array.exactlyOne markedBoards
        else
            // continue with next number on remaining boards 
            findLooser rest remainingBoards

let numLooser, looser = findLooser numbers boards

printBoard looser

// result
boardScore looser * numLooser