let test =
    [| 
        "NNCB"
        ""
        "CH -> B"
        "HH -> N"
        "CB -> H"
        "NH -> C"
        "HB -> C"
        "HC -> B"
        "HN -> C"
        "NN -> C"
        "BH -> H"
        "NC -> B"
        "NB -> B"
        "BN -> B"
        "BB -> N"
        "BC -> B"
        "CC -> N"
        "CN -> C"
    |]

let data =
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input/day14.txt")


module Map =
    let inc k (v: int64) map =
        match Map.tryFind k map with
        | None -> Map.add k v map
        | Some x -> Map.add k (v+x) map

let parse (data: string[]) =
    let mappings =
        data
        |> Seq.skip 2
        |> Seq.map (fun l -> let [|pair;ins|] = l.Split(" -> ") in pair, (pair.Substring(0,1)+ins, ins+pair.Substring(1)))
        |> Map.ofSeq

    let template = data[0]

    let counts =
        template
        |> Seq.pairwise
        |> Seq.map (fun (c1,c2) -> $"{c1}{c2}" )
        |> Seq.countBy id
        |> Seq.map (fun (k, v) -> k, int64 v)
        |> Map.ofSeq
    let s = template[0]
    let e = template[template.Length-1]
    (s,e), mappings, counts

let run (ext,mappings, (counts: Map<string,int64>)) =
    ext, mappings,
        counts
        |> Map.fold (fun acc k (v: int64) -> 
            match Map.tryFind k mappings with
            | None -> failwith "Mapping not found"
            | Some (p1,p2) -> acc |> Map.inc p1 v |> Map.inc p2 v 
            ) Map.empty 

let rec nrun n v =
    if n = 0 then 
        v
    else
        nrun (n-1) (run v)

let letterCounts ((s,e), _, counts ) =
    Map.toSeq counts
    |> Seq.collect (fun (k: string,v: int64) -> 
        [k[0],v
         k[1],v] )
    |> Seq.groupBy fst
    |> Seq.map (fun (k, vs) -> 
        let sum = Seq.sumBy snd vs + (if k = s then 1L else 0L) + (if k = e then 1L else 0L)
        k, sum/2L)
    |> Seq.toList

let result (letters: List<_ * int64>) =
    let _,max = List.maxBy snd letters
    let _,min = List.minBy snd letters
    max - min
    

parse test
|> nrun 10
|> letterCounts
|> result
        
parse data
|> nrun 10
|> letterCounts
|> result

test



parse data
|> nrun 40
|> letterCounts
|> result

