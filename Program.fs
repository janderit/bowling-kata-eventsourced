// Event-sourced solution to the Bowling Kata
// This is obviously not a 'simple' solution, but to demonstrate event sourcing in a minimalistic domain
// Philip Jander, 2019
// Thanks to Ralf Westphal (@ralfw) for the challenge.

/// Computation Expression Builder for functional event sourcing
module EventSourced = 
    type EventSourcedBuilder() =
        member __.Bind (v, f) = fun history -> f (v history) history
        member __.Yield e = fun history -> List.concat [ history; [ e ] ]
        member __.Zero () = id
        member __.Combine (f, g) = f >> (g())
        member __.Delay f = f
        member __.Run f = f ()

    let script = new EventSourcedBuilder()

/// Bowling domain. Contains structure, projections, command handlers and query handlers.
module Bowling =

    type FrameType = 
        /// a frame in which the player did not clear all pins
        | Normal 
        /// a frame in which the player cleared all pins in 2 rolls, leads to one bonus roll
        | Spare 
        /// a frame in which the player cleared all pins in a single roll, leads to two bonus roll
        | Strike

    type BowlingEvent =
        /// a roll was taken by the player that counts towards normal score
        | RollWasRegistered of int 
        
        /// a frame (2 rolls or a strike roll) was completed 
        | FrameWasCompleted of FrameType 
        
        /// a frame requires one or two bonus rolls
        | BonusRequired of int 
        
        /// a roll counts towards pending bonuses with the total score given
        | BonusWasRegistered of int 

    /// projection: net score of all rolls
    let sum_of_rolls events = 
        List.fold (fun a -> function | RollWasRegistered pins -> a + pins | _ -> a ) 0 events

    /// projection: bonus score of all rolls counting towards bonuses
    let sum_of_bonuses events = 
        List.fold (fun a -> function | BonusWasRegistered pins -> a + pins | _ -> a ) 0 events

    /// projection: number of frames completed up to now
    let private frames_completed events = 
        List.fold (fun a -> function | FrameWasCompleted _ -> a + 1 | _ -> a ) 0 events

    /// projection: are all normal rolls completed (bonus might still be pending)
    let all_frames_completed state = (frames_completed state) = 10

    /// projection: net score in the current frame 
    let net_score_in_active_frame events = 
        List.fold 
            (fun a -> function 
                | RollWasRegistered pins -> a + pins 
                | FrameWasCompleted _ -> 0
                | _ -> a ) 0 events

    /// projection: number of rolls in the current frame 
    let rolls_in_active_frame events = 
        List.fold 
            (fun a -> function 
                | RollWasRegistered _ -> a + 1
                | FrameWasCompleted _ -> 0
                | _ -> a ) 0 events

    /// projection: a list of number of bonus rolls still required, e.g. [1;2] means that from previous frames, there are still 1 and two bonus rolls pending.
    let private pending_bonus events = 
        List.fold (fun a -> function 
            | BonusRequired rolls -> rolls :: a
            | BonusWasRegistered _ -> a |> List.map (fun r -> r - 1) |> List.filter (fun r -> r > 0)
            | _ -> a ) [] events

    /// projection: number of previous frames, the next/current roll should count towards as a bonus
    let bonuses_required_for_next_roll events = pending_bonus events |> List.length

    /// game state: simply the event store
    type public T = BowlingEvent list

    /// command handler to start a game
    let public start () = []       

    /// command handler to register the next roll
    let public register (pins: int) = 
        EventSourced.script {
            
            let! bonuses_pending = bonuses_required_for_next_roll
            if bonuses_pending > 0 then yield (BonusWasRegistered (bonuses_pending * pins))

            let! completed = all_frames_completed
            if (not completed)
            then 

                yield (RollWasRegistered pins)

                let! rolls = rolls_in_active_frame
                let! score = net_score_in_active_frame                
                if rolls = 2 || score = 10
                    then 
                        let frame_type = 
                            match score, rolls with
                            | 10, 1 -> Strike
                            | 10, 2 -> Spare
                            | _ -> Normal
                        yield (FrameWasCompleted frame_type)
                        if frame_type = Spare then yield (BonusRequired 1)
                        if frame_type = Strike then yield (BonusRequired 2)
        }

    /// query handler to return the current score
    let public score (t:T) = sum_of_rolls t + sum_of_bonuses t

let flip f = fun a b -> f b a

/// game function - subject under test
let bowling rolls = rolls |> List.fold (flip Bowling.register) (Bowling.start()) |> Bowling.score

module Test =
    let run subject (rolls,expected) =
        let actual = subject rolls
        if actual = expected
            then "OK\n"
            else sprintf "Expected %d, but found %d\n" expected actual

    let cases = 
        [ [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ], 0
          [ 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ], 1
          [ 1; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ], 5
          [ 5; 5; 3; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ], 15 + 3
          [ 10; 3; 2; 5; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ], 20 + 5
          [ 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10 ], 300 ]

[<EntryPoint>]
let main argv =
    // bowling [ 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10 ] |> printf "%d"
    Test.cases |> List.map(Test.run bowling) |> List.iter (printf "%s")    
    0 // return an integer exit code
