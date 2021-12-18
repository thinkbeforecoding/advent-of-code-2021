open System.IO

let data =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input/day3.txt")
    |> Array.map (fun l -> System.Convert.ToInt32(l,2))

// part 1
// returns a int witn nth bit set
let bit n = 1<<<n

// check whether x has nth bit set (returns 1 if set, 0 if not)
let hasbit n x =
    if (x &&& bit n) = 0 then
        0
    else
        1

// return an int with the nth bit set if it was more present than absent
// return 0 if not
let mostbit n =
    let count = data |> Array.sumBy(hasbit n)
    if count>data.Length/2 then
        bit n
    else
        0 

// combines all bits together
let gamma =
    [| for n in 0..11 -> mostbit n |] 
    |> Array.fold (|||) 0

// espsilon is binary not of gamma on 12 bits
let epsilon =
    ~~~gamma &&& 0b111111111111

// result
gamma*epsilon

// part 2
// here we count presence an absence and compare
let hasmostbit n data =
    let count = data |> Array.sumBy(hasbit n)
    let notCount = data.Length - count
    count>=notCount

// 1 for bit not set
let hasnotbit n x = 1 - hasbit n x  

// compere persence and absence of 0 
let hasleastbit n data =
    let count = data |> Array.sumBy(fun x -> hasnotbit n x)
    let notCount = data.Length - count
    count <= notCount

// filter input data for oxygen
let filterOxy n data  =
    if hasmostbit n data then
        data |> Array.filter (fun x -> hasbit n x = 1)
    else
        data |> Array.filter (fun x -> hasbit n x = 0)

// filter input data for CO2
let filterCO2 n data  =
    if hasleastbit n data then
        data |> Array.filter (fun x -> hasbit n x = 0)
    else
        data |> Array.filter (fun x -> hasbit n x = 1)

// test on example
[| 
    0b00100
    0b11110
    0b10110
    0b10111
    0b10101
    0b01111
    0b00111
    0b11100
    0b10000
    0b11001
    0b00010
    0b01010
|]
|> filterOxy 4
|> filterOxy 3
|> filterOxy 2
|> filterOxy 1
|> filterOxy 0
|> fun a -> a |> Array.iter (printfn "%B") 

[| 
    0b00100
    0b11110
    0b10110
    0b10111
    0b10101
    0b01111
    0b00111
    0b11100
    0b10000
    0b11001
    0b00010
    0b01010
|]
|> filterCO2 4
|> filterCO2 3
|> filterCO2 2
|> filterCO2 1
// |> filterCO2 0
|> fun a -> a |> Array.iter (printfn "%B") 


// filter oxy until 1 data left
let [| oxy |] =
    filterOxy 11 data
    |> filterOxy 10
    |> filterOxy 9
    |> filterOxy 8
    |> filterOxy 7
    |> filterOxy 6
    |> filterOxy 5
    |> filterOxy 4
    |> filterOxy 3
    |> filterOxy 2
    |> filterOxy 1
    |> filterOxy 0

// filter co2 until 1 data left
let [|co2|]= 
    data
    |> filterCO2 11
    |> filterCO2 10
    |> filterCO2 9
    |> filterCO2 8
    |> filterCO2 7
    |> filterCO2 6
    |> filterCO2 5
    |> filterCO2 4
    |> filterCO2 3

// result
oxy * co2



