
type FrameType = Normal | Spare | Strike

type BowlingEvent =
    | RollWasRegistered of int
    | FrameWasCompleted of FrameType * int
    | BonusWasRegistered of int
    | BonusRequired of int

module Store =
    type public T = BowlingEvent list
    let public empty : T = []
    let public append (t:T) (e:BowlingEvent) = 
        // printf "%A\n" e
        (e :: List.rev t) |> List.rev

module Bowling =

    type public GameOperation = (Store.T -> Store.T)

    let public sum_of_rolls (events: Store.T) = 
        List.fold (fun a -> function | RollWasRegistered pins -> a + pins | _ -> a ) 0 events

    let public sum_of_bonuses (events: Store.T) = 
        List.fold (fun a -> function | BonusWasRegistered pins -> a + pins | _ -> a ) 0 events

    let public frames_completed (events: Store.T) = 
        List.fold (fun a -> function | FrameWasCompleted _ -> a + 1 | _ -> a ) 0 events

    let public net_score_in_active_frame (events: Store.T) = 
        List.fold 
            (fun a -> function 
                | RollWasRegistered pins -> a + pins 
                | FrameWasCompleted _ -> 0
                | _ -> a ) 0 events

    let public rolls_in_active_frame (events: Store.T) = 
        List.fold 
            (fun a -> function 
                | RollWasRegistered _ -> a + 1
                | FrameWasCompleted _ -> 0
                | _ -> a ) 0 events

    let public pending_bonus (events: Store.T) = 
        List.fold (fun a -> function 
            | BonusRequired rolls -> rolls :: a
            | BonusWasRegistered _ -> a |> List.map (fun r -> r - 1) |> List.filter (fun r -> r > 0)
            | _ -> a ) [] events

    let public frame_needs_bonus (events: Store.T) = pending_bonus events |> List.length

    let public register_roll pins = fun state -> Store.append state (RollWasRegistered pins)

    let public register_frame_completed frame_type score = fun state -> Store.append state (FrameWasCompleted (frame_type, score))

    let public register_bonus pins = fun state -> Store.append state (BonusWasRegistered pins)

    let public bonus_required rolls = fun state -> Store.append state (BonusRequired rolls)

    let public game_completed state = (frames_completed state) = 10

module Game =
    type public T = Store.T

    let public start () : T = Store.empty

    let public register (state:T) (roll: int) : T = 
        let state = 
            let bonuses = Bowling.frame_needs_bonus state
            if (bonuses > 0)
            then Bowling.register_bonus (bonuses * roll) state
            else state

        let state = 
            if not (Bowling.game_completed state)
            then 
                let state = Bowling.register_roll roll state
                let rolls = Bowling.rolls_in_active_frame state
                let score = Bowling.net_score_in_active_frame state
                let state = 
                    if rolls = 2 || score = 10
                    then 
                        let score = Bowling.net_score_in_active_frame state
                        let frame_type = 
                            match score, rolls with
                            | 10, 1 -> Strike
                            | 10, 2 -> Spare
                            | _ -> Normal
                        let state = Bowling.register_frame_completed frame_type score state
                        match frame_type with
                        | Normal -> state
                        | Spare -> Bowling.bonus_required 1 state
                        | Strike -> Bowling.bonus_required 2 state
                    else state
                state
            else state
        state

    let public score (t:T) = Bowling.sum_of_rolls t + Bowling.sum_of_bonuses t


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
    // bowling [ 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10 ] |> printf "%d"
    testcases |> List.map(test bowling) |> List.iter (printf "%s")    
    0 // return an integer exit code
