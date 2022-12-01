open System

[<Struct>]
type Point =
    { X: int
      Y: int
      Z: int }

    static member (+) (a,b) =
        { X = a.X + b.X
          Y = a.Y + b.Y
          Z = a.Z + b.Z}

    static member (-) (a,b) =
        { X = a.X - b.X
          Y = a.Y - b.Y
          Z = a.Z - b.Z}

    member this.Manhatan =
        abs this.X + abs this.Y + abs this.Z

[<Struct>]
type Matrix =
    Matrix of int[]
    with
    static member (*) (Matrix a,Matrix b) =
        Matrix [|
            for l in 0..3 do
                for c in 0..3 do
                    let mutable v = 0 
                    for n in 0..3 do
                        v <- v + a[l*4+n] * b[n*4+c]
                    v
        |]


    static member (*) (Matrix a, p) =
        { X = p.X * a[0] + p.Y * a[1] + p.Z * a[2] + a[3] 
          Y = p.X * a[4] + p.Y * a[5] + p.Z * a[6] + a[7]
          Z = p.X * a[8] + p.Y * a[9] + p.Z * a[10] + a[11] }
        

    static member (*) (a: Matrix, scan: Point list) = 
        scan |> List.map (fun p -> a * p)


let ncostab = [| 1; 0; -1; 0|]
let nsintab = [| 0; 1; 0; -1 |]    

let ncos n = ncostab[n%4]
let nsin n = nsintab[n%4]

   
module Matrix =
    let Id =
        Matrix 
            [| 1; 0; 0; 0
               0; 1; 0; 0
               0; 0; 1; 0
               0; 0; 0; 1 |]



    let rotZ n =
        Matrix
            [| ncos n;-nsin n; 0; 0
               nsin n; ncos n; 0; 0
               0     ; 0     ; 1; 0
               0     ; 0     ; 0; 1 |]
           
    let rotY n =
        Matrix
            [| ncos n; 0;-nsin n; 0
               0     ; 1; 0     ; 0
               nsin n; 0; ncos n; 0
               0     ; 0; 0     ; 1 |]

    let rotX n =
        Matrix
            [| 1 ; 0     ;       0; 0
               0 ; ncos n; -nsin n; 0
               0 ; nsin n;  ncos n; 0
               0 ; 0     ; 0      ; 1 |]

    let invX =
        Matrix
            [| -1; 0; 0; 0
               0 ; 1; 0; 0
               0 ; 0; 1; 0
               0 ; 0; 0; 1 |]

    let invY =
        Matrix
            [| 1; 0; 0; 0
               0;-1; 0; 0
               0; 0; 1; 0
               0; 0; 0; 1 |]

    let invZ =
        Matrix
            [| 1; 0; 0; 0
               0; 1; 0; 0
               0; 0;-1; 0
               0; 0; 0; 1 |]

    let trans p =
        Matrix
            [| 1; 0; 0; p.X
               0; 1; 0; p.Y
               0; 0; 1; p.Z
               0; 0; 0; 1 |]



let parseBeacon (line: string) =
    match line.Split(',') with
    | [| x;y;z |] -> { X = int x; Y = int y; Z = int z }
    | _ -> failwith "invalid format"

let rec parseBeacons lines result =
    match lines with
    | [] -> List.rev result, []
    | "" :: r -> List.rev result, r
    | line :: r ->
        parseBeacons r (parseBeacon line :: result)
    

let rec parse result lines =
        match lines with
        | [] -> List.rev result
        | _ :: rest ->
            let beacons, next = parseBeacons rest []
            parse (beacons :: result) next




let data =
    IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input/day19.txt")
    |> Array.toList
    |> parse []


let sample =
    """--- scanner 0 ---
-1,-1,1
-2,-2,2
-3,-3,3
-2,-3,1
5,6,-4
8,0,7

--- scanner 0 ---
1,-1,1
2,-2,2
3,-3,3
2,-1,3
-5,4,-6
-8,-7,0

--- scanner 0 ---
-1,-1,-1
-2,-2,-2
-3,-3,-3
-1,-3,-2
4,6,5
-7,0,8

--- scanner 0 ---
1,1,-1
2,2,-2
3,3,-3
1,3,-2
-4,-6,5
7,0,8

--- scanner 0 ---
1,1,1
2,2,2
3,3,3
3,1,2
-6,-4,-5
0,7,-8""".Split('\n')
    |> Array.toList
    |> parse []

let scan1 = sample[0]
let scan2 = sample[1]


let trans =
    [ for rx in 0..3 do
      for ry in 0..3 do
      for rz in 0..3 do
        let m = Matrix.rotX rx * Matrix.rotY ry * Matrix.rotZ rz
        m ]
    |> List.distinct



let intersect min scan1 scan2 =
    scan1 |> List.tryPick(fun p1 ->
        scan2 |> List.tryPick(fun p2 ->

                let v = p1-p2
                let scan2m = List.map (fun p -> p + v) scan2
                let inter = Set.intersect (set scan2m) (set scan1)
                if inter.Count >= min then
                    Some (Matrix.trans v)
                else
                    None))

let intAll min scan1 scan2 =
    trans |> List.tryPick (fun m ->
        match intersect min scan1 (m * scan2) with
        | Some t -> Some (t * m)
        | None -> None
    )
    

intAll 6 sample[0] sample[2]

let sample2 =
    """--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14""".Split('\n')
    |> Array.toList
    |> parse []
            



let t10 = intAll 12 sample2[0] sample2[1] |> Option.get
let t31 = intAll 12 sample2[1] sample2[3] |> Option.get
let t41 = intAll 12 sample2[1] sample2[4] |> Option.get
let t24 = intAll 12 sample2[4] sample2[2] |> Option.get

let r= set sample2[0] + set (t10 * sample2[1]) + set (t10 * t31 * sample2[3]) + set (t10 * t41 * sample2[4]) +
        set (t10 * t41 * t24 * sample2[2])

r.Count



let graph (sample2 : _ list)  =
    let mutable graph = Map.empty
    let mutable failed = Set.empty
                    
    for i in 0..sample2.Length-1 do
        for j in 0..sample2.Length-1 do
            if i <> j then
                if not (Set.contains (j,i) failed) then
                    match intAll 12 sample2[i] sample2[j] with
                    | Some t ->
                        graph <-
                            match Map.tryFind j graph with
                            | None -> Map.add j [i,t] graph
                            | Some l -> Map.add j ((i,t) :: l) graph
                    | None -> failed <- Set.add (i,j) failed
                    

    graph

let paths (scans: Point list list) (graph: Map<int,(int * Matrix) list>) =
    let assocs = 
        graph
        |> Map.toSeq 
        |> Seq.collect (fun (i,l) ->
            l |> Seq.map (fun (j,t) -> (i,j),t ))
        |> Map.ofSeq

    let mutable graph = graph
    let mutable visited = Set.empty
    let mutable result = []

    let rec loop n m =
        if not (Map.isEmpty graph) then
            if not (Set.contains n visited) then
                result <-  set (m * scans[n]) :: result
                visited <- Set.add n visited
                let l = graph[n]
                graph <- Map.remove n graph
                for i,_ in l do
                    let t = assocs[(i,n)]
                    
                    loop i (m*t)

    loop 0 Matrix.Id


    result


graph sample2
|> paths sample2
|> Set.unionMany
|> Set.count




#time
graph data
|> paths data
|> Set.unionMany
|> Set.count

        
    

let scanPos (scans: Point list list) (graph: Map<int,(int * Matrix) list>) =
    let assocs = 
        graph
        |> Map.toSeq 
        |> Seq.collect (fun (i,l) ->
            l |> Seq.map (fun (j,t) -> (i,j),t ))
        |> Map.ofSeq

    let mutable graph = graph
    let mutable visited = Set.empty
    let mutable result = []

    let rec loop n m =
        if not (Map.isEmpty graph) then
            if not (Set.contains n visited) then
                let (Matrix mx) = m
                result <-  {X = mx[3]; Y = mx[7]; Z = mx[11]} :: result
                visited <- Set.add n visited
                let l = graph[n]
                graph <- Map.remove n graph
                for i,_ in l do
                    let t = assocs[(i,n)]
                    
                    loop i (m*t)

    loop 0 Matrix.Id


    result
    
    
graph sample2
|> scanPos sample2
|> fun l -> List.allPairs l l |> List.map (fun (x, y) -> (x-y).Manhatan) |> List.max


graph data
|> scanPos data
|> fun l -> List.allPairs l l |> List.map (fun (x, y) -> (x-y).Manhatan) |> List.max





            
    


