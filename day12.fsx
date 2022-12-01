open System

type CaveType =
    | Start
    | End
    | Big
    | Small

type Cave =
    { Name: string
      Type: CaveType
      Paths: ResizeArray<Cave>
    }

let printPath (cave : Cave) =
    $"""{cave.Name} -> { cave.Paths |> Seq.map (fun c -> c.Name) |> String.concat ", " }"""

fsi.AddPrinter printPath


let test =
    //  System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input/day12.txt")
    [|  "start-A"
        "start-b"
        "A-c"
        "A-b"
        "b-d"
        "A-end"
        "b-end" |]
let data =
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input/day12.txt")
    |> Array.map (fun line -> 
        match line.Split('-') with
        | [| x; y |] -> x,y 
        | _ -> failwith "bad input"
        )

let caves =
    let allCaves = 
        data
        |> Seq.collect (fun (s,e) -> seq { s; e } )
        |> Seq.distinct
        |> Seq.map (fun name ->
            let t =
                match name with
                | "start" -> Start
                | "end" -> End
                | _ when Char.IsUpper name[0] -> Big
                | _ -> Small
            name, { Name = name; Type = t; Paths = ResizeArray() }
        )
        |> Map.ofSeq


    for s,e in data do
        let startCave = allCaves[s]
        let endCave = allCaves[e]
        startCave.Paths.Add(endCave)
        endCave.Paths.Add(startCave)

    allCaves

let result = 
    let start = caves["start"]

    let paths = ResizeArray()
    let rec findPaths cave (path: string list)  (visited: string Set) =
        match cave.Type with
        | End -> paths.Add( path )
        | (Start | Small) when visited.Contains cave.Name ->
                ()
        | _ ->
                let newPath = cave.Name :: path
                let newVisited = Set.add cave.Name visited
                for neighbor in cave.Paths do
                    findPaths neighbor newPath newVisited

    findPaths start [] Set.empty

    paths

result
result.Count

let result2 = 
    let start = caves["start"]

    let paths = ResizeArray()
    let rec findPaths cave (path: string list) (visited: string Set) twice =
        match cave.Type with
        | End -> paths.Add( path )
        | Start when visited.Contains cave.Name -> ()
        | Small when visited.Contains cave.Name && twice ->
            ()
        | _ ->
                let newPath = cave.Name :: path
                let newVisited = Set.add cave.Name visited
                let newTwice =
                    if cave.Type = Small && visited.Contains cave.Name then
                        true
                    else
                        twice
                    

                for neighbor in cave.Paths do
                    findPaths neighbor newPath newVisited newTwice

    findPaths start [] Set.empty false

    paths

result2.Count