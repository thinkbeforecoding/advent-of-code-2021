open System

type Bitmap =
    { Pixels: int[][]
      Ext: int
      Codes: int[]
    }

    member this.Item(c,l) = 
        if l<0 || l >= this.Pixels.Length then
            this.Ext
        else
            let line = this.Pixels[l]
            if c < 0 || c >= line.Length then
                this.Ext
            else
                line[c]

    override this.ToString() =
        this.Pixels
        |> Array.map(fun l -> 
            l |> Array.map (fun c -> if c=0 then '.' else '#') |> String
        )
        |> String.concat "\n"


fsi.AddPrinter (fun (b: Bitmap) -> string b )


let parse (lines: string[]) =
    let code = 
        lines[0]
        |> Seq.map (function '.' -> 0 | _ -> 1 )
        |> Seq.toArray
    let bitmap = 
        lines[2..lines.Length-1]
        |> Array.map (fun line ->
            line
            |> Seq.map (function '.' -> 0 | _ -> 1)
            |> Seq.toArray
        )
    { Pixels = bitmap
      Ext = 0
      Codes = code
    }
let input =
    let lines = IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input/day20.txt")
    parse lines

let sample =
    """..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###""".Split('\n') |> parse



module Bitmap =
    let enhance (bitmap: Bitmap) =
        { Pixels = 
            [| for l in -1 .. bitmap.Pixels.Length do
                [|
                    for c in -1 .. bitmap.Pixels[0].Length do
                        let index = 
                            bitmap[c-1,l-1] <<< 1
                            ||| bitmap[c,l-1] <<< 1
                            ||| bitmap[c+1,l-1] <<< 1
                            ||| bitmap[c-1,l] <<< 1
                            ||| bitmap[c,l] <<< 1
                            ||| bitmap[c+1,l] <<< 1
                            ||| bitmap[c-1,l+1] <<< 1
                            ||| bitmap[c,l+1] <<< 1
                            ||| bitmap[c+1,l+1]

                        bitmap.Codes[index]
                |]
                
                
        
            |]
          Ext = bitmap.Codes[if bitmap.Ext = 0 then 0 else 0x1ff]
          Codes = bitmap.Codes
        
        }

    let rec nenhance n bitmap =
        if n = 0 then
            bitmap
        else
            nenhance (n-1) (enhance bitmap)

let r=  input |> Bitmap.enhance |> Bitmap.enhance
r.Pixels |> Array.sumBy Array.sum


let r2 = input |> Bitmap.nenhance 50
r2.Pixels |> Array.sumBy Array.sum

