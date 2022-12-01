

[<Struct>]
type Player =
    { Pos: int
      Score: int }

type Game =
    { Players: Player[]
      Rolls: int
      Dice: int
      Player: int
      }

let deterministic p1 p2 =
    { Players = [| {Pos = p1; Score = 0}; {Pos = p2; Score = 0}  |]
      Rolls = 0
      Dice = 1
      Player = 1
    }
    
let sample = deterministic 4 8
let input = deterministic 7 10
// kind of module but from 1 to y included
let (%%) x y = ((x-1)%y)+1


let play game =
    let p = game.Players[game.Player-1]
    let roll = (game.Dice + 1) * 3  // d + (d+1) + (d+2)
    let player = 
        let pos = (p.Pos+roll) %% 10
        { Pos = pos
          Score = p.Score + pos
        }

    { Players = [| for p in 0 .. 1 ->
                    if p+1 = game.Player 
                    then player
                    else game.Players[p]
                |]
      Rolls = game.Rolls + 3
      Dice = (game.Dice+3) %% 100
      Player = (game.Player + 1) %% 2
    }

    
sample |> play |> play |> play

let rec playAll game =
    if game.Players[0].Score >= 1000 then
        game.Players[1].Score * game.Rolls
    elif game.Players[1].Score >= 1000 then
        game.Players[0].Score * game.Rolls
    else
        playAll (play game)

playAll sample


module Dirac =
    open System.Collections.Generic
    
    (*
        3: 1
        4: 3
        5: 6
        6: 7
        7: 6
        8: 3
        9: 1


        111 3
        112 4
        113 5
        121 4
        122 5
        123 6
        131 5
        132 6
        133 7

        211 4
        212 5
        213 6
        221 5
        222 6
        223 7
        231 6
        232 7
        233 8

        311 5
        312 6
        313 7
        321 6
        322 7
        323 8
        331 7
        332 8
        333 9

*)


    let rolls = [| 0L; 0; 0; 1L; 3L; 6L; 7L; 6L; 3L; 1L |]

    type Player = { Pos: int; Score: int}
    type Game =
        { P1: Player
          P2: Player }

    
    let play p1 p2 =
        let mutable victory1 = 0L
        let mutable victory2 = 0L
        let advance pidx (games: IDictionary<Game, int64>) =
            let newgames = Dictionary<Game, int64>()
            for roll in 3 .. 9  do
                let count = rolls[roll]
                for KeyValue(game, total) in games do
                    if pidx = 1 then
                        let p = game.P1
                        let newPos = (p.Pos + roll) %% 10
                        let newScore = p.Score + newPos
                        if newScore >= 21 then
                            victory1 <- victory1 + total * count
                        else
                            let newGame = 
                                { game with
                                    P1 = { Pos = newPos; Score = newScore  }
                                }
                            let prevCount= 
                                match newgames.TryGetValue(newGame) with
                                | true, prevCount -> prevCount
                                | false, _ -> 0
                            newgames[newGame] <- prevCount + total*count
                    else
                        let p = game.P2
                        let newPos = (p.Pos + roll) %% 10
                        let newScore = p.Score + newPos
                        if newScore >= 21 then
                            victory2 <- victory2 + total * count
                        else
                            let newGame = 
                                { game with
                                    P2 = { Pos = newPos; Score = newScore  }
                                }
                            let prevCount= 
                                match newgames.TryGetValue(newGame) with
                                | true, prevCount -> prevCount
                                | false, _ -> 0
                            newgames[newGame] <- prevCount + total*count
            newgames :> IDictionary<_,_>

        let start p1 p2 =
            dict [ { P1 = { Pos = p1; Score = 0 }; P2 = {Pos = p2; Score = 0}}, 1L  ]

        let rec loop games =
            let newGames =
                games
                |> advance 1
                |> advance 2

            if newGames.Count = 0 then
                victory1, victory2
            else
                loop newGames

        start p1 p2 |> loop

    play 4 8

    play 7 10

            





