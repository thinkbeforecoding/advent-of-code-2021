// Input target area: x=25..67, y=-260..-200

type Rect = { Left: int; Top: int; Right: int; Bottom: int}

let data = { Left = 25; Top = -200; Right = 67; Bottom = -260}

type Vec = { X: int; Y: int}
    with 
    static member (+) (v1,v2) =
        { X = v1.X+v2.X; Y = v1.Y+v2.Y}

let p = { X = 0; Y = 0}

type State =
    { Pos: Vec
      Velocity: Vec
      Area: Rect }

    member this.Reached = this.Pos.X >= this.Area.Left && this.Pos.X <= this.Area.Right
                            && this.Pos.Y >= this.Area.Bottom && this.Pos.Y <= this.Area.Top

let drag v =
    { X = if v.X > 0 then -1
          elif v.X<0 then 1
          else 0
      Y = -1 }

let step s =
    { s with Pos = s.Pos + s.Velocity
             Velocity = s.Velocity + drag s.Velocity
    }



let rec run maxy s  =
    let newS = step s
    if newS.Pos.Y < s.Area.Bottom then
        None
    elif newS.Reached then
        Some maxy
    else
        run (max newS.Pos.Y maxy) newS

{ Pos = { X = 0; Y = 0}
  Velocity = { X = 6; Y = 9 }
  Area = { Left = 20; Right = 30; Bottom = -10; Top = -5 }
  }
|> run 0

{ Pos = { X = 0; Y = 0}
  Velocity = { X = 7; Y = 300 }
  Area = data
  }
|> run 0

let mutable maxy = 0 
let mutable maxv = { X=0; Y=0}
let mutable count = 0
for vx in 0 .. data.Right do
    for vy in data.Bottom .. 1000 do

        let y = 
            { Pos = { X = 0; Y = 0}
              Velocity = { X = vx; Y = vy }
              Area = data
            }
            |> run 0
        match y with
        | Some v  -> 
            count <- count+1
            if v > maxy then
                maxy <- v
                maxv <- { X = vx; Y = vy }
        | _ -> ()

        






