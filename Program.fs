
type BowlingEvent =
    RollWasRegistered of int

module Store =
    type public T = BowlingEvent list
    let public empty : T = []
    let public append (t:T) (e:BowlingEvent) = e :: t

module Bowling =
    let public sum_of_rolls (events: Store.T) = 
        List.fold (fun a -> function | RollWasRegistered pins -> a + pins | _ -> a ) 0 events

module Game =
    type public T = Store.T

    let public start () : T = Store.empty

    let public register (t:T) (roll: int) : T = 
        Store.append t (RollWasRegistered roll)

    let public score (t:T) = Bowling.sum_of_rolls t


let bowling rolls =
    List.fold (Game.register) (Game.start()) rolls |> Game.score


let test subject (rolls,expected) =
    let actual = subject rolls
    if actual = expected
        then "OK\n"
        else sprintf "Expected %d, but found %d\n" expected actual

let testcases = 
    [ [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ], 0
      [ 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ], 1
      [ 1; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ], 5
      [ 5; 5; 3; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ], 15 + 3
      [ 10; 3; 2; 5; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ], 20 + 5
      [ 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10 ], 300 ]

[<EntryPoint>]
let main argv =
    testcases |> List.map(test bowling) |> List.iter (printf "%s")    
    0 // return an integer exit code
