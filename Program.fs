// Event-sourced solution to the Bowling Kata
// This is obviously neither a 'simple' solution for the Kata, nor an infrastructure focussed event sourcing demo,
// but rather intended to demonstrate event sourcing *principles* in a minimalistic domain.
// Philip Jander, 2019
// (thanks to Ralf Westphal (@ralfw) for posing the challenge)

// ATTN: functional code, read from the end of the file :)

/// Computation Expression Builder for functional event sourcing
module EventSourced = 
    type EventSourcedBuilder() =
        member __.Bind (v, f) = fun history -> f (v history) history
        member __.Yield e = fun history -> List.concat [ history; [ e ] ]
        member __.YieldFrom f = f
        member __.Zero () = id
        member __.Combine (f, g) = f >> (g())
        member __.Delay f = f
        member __.Run f = f ()

    let expression = new EventSourcedBuilder()

/// Bowling domain. Contains structure, events, projections, command handlers and query handlers.
module Bowling =

    [<Literal>]
    let private FRAMES_PER_GAME = 10

    [<Literal>]
    let private MAXIMUM_ROLLS_PER_FRAME = 2

    [<Literal>]
    let private ALL_PINS = 10

    [<Literal>]
    let private BONUS_ROLLS_AFTER_SPARE = 1

    [<Literal>]
    let private BONUS_ROLLS_AFTER_STRIKE = 2

    type BowlingEvent =
        /// a roll was taken by the player that counts towards normal score
        | RollCountsTowardsScore of int 
        
        /// a frame (2 rolls or a strike roll) was completed 
        | FrameWasCompleted 
        
        /// a spare or strike lead to a bonus
        | BonusRequired of int
        
        /// a roll counts towards pending bonuses with the total score given
        | RollCountsTowardsBonus of int 

    /// projection: net score of all rolls
    let sum_of_rolls events = 
        List.fold (fun a -> function | RollCountsTowardsScore pins -> a + pins | _ -> a ) 0 events

    /// projection: bonus score of all rolls counting towards bonuses
    let sum_of_bonuses events = 
        List.fold (fun a -> function | RollCountsTowardsBonus pins -> a + pins | _ -> a ) 0 events

    /// projection: number of frames completed up to now
    let private frames_completed events = 
        List.fold (fun a -> function | FrameWasCompleted -> a + 1 | _ -> a ) 0 events

    /// projection: are all normal rolls completed (bonus might still be pending)
    let all_frames_completed state = (frames_completed state) = FRAMES_PER_GAME

    /// projection: net score in the current frame 
    let net_score_in_active_frame events = 
        List.fold 
            (fun a -> function 
                | RollCountsTowardsScore pins -> a + pins 
                | FrameWasCompleted _ -> 0
                | _ -> a ) 0 events

    /// projection: number of rolls in the current frame 
    let rolls_in_active_frame events = 
        List.fold 
            (fun a -> function 
                | RollCountsTowardsScore _ -> a + 1
                | FrameWasCompleted _ -> 0
                | _ -> a ) 0 events

    /// projection: a list of number of bonus rolls still required, e.g. [1;2] means that from previous frames, there are still 1 and two bonus rolls pending.
    let private pending_bonus events = 
        List.fold (fun a -> function 
            | BonusRequired rolls -> rolls :: a
            | RollCountsTowardsBonus _ -> a |> List.map (fun r -> r - 1) |> List.filter (fun r -> r > 0)
            | _ -> a ) [] events

    /// projection: number of previous frames, the next/current roll should count towards as a bonus
    let bonuses_required_for_next_roll events = pending_bonus events |> List.length

    /// game state: simply the event store
    type public T = BowlingEvent list

    /// command handler to start a game
    let public start () = []       

    // Helpers for the register_roll command handler
    let private apply_bonus_if_pending pins_hit = EventSourced.expression {
        let! bonuses_pending = bonuses_required_for_next_roll
        if bonuses_pending > 0 then 
            let total_bonus = bonuses_pending * pins_hit
            yield RollCountsTowardsBonus total_bonus }

    let private check_if_frame_complete = EventSourced.expression {
        let! pins_hit = net_score_in_active_frame
        let! rolls_used = rolls_in_active_frame        

        match pins_hit, rolls_used with        
        | ALL_PINS, MAXIMUM_ROLLS_PER_FRAME -> 
            yield FrameWasCompleted
            yield BonusRequired BONUS_ROLLS_AFTER_SPARE
        | ALL_PINS, _ -> 
            yield FrameWasCompleted
            yield BonusRequired BONUS_ROLLS_AFTER_STRIKE
        | _, MAXIMUM_ROLLS_PER_FRAME -> 
            yield FrameWasCompleted
        | _ -> () }
        
    /// command handler to register the next roll
    let public register_roll pins_hit = EventSourced.expression {        
        yield! apply_bonus_if_pending pins_hit
        match! all_frames_completed with
        | false ->         
            yield RollCountsTowardsScore pins_hit
            yield! check_if_frame_complete
        | true -> () }

    /// query handler to return the current score
    let public score (t:T) = sum_of_rolls t + sum_of_bonuses t

/// game function - subject under test
let bowling rolls = 
    Bowling.start()
    |> List.foldBack (Bowling.register_roll) (rolls |> List.rev)
    |> Bowling.score

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
    let results = Test.cases |> List.map (bowling |> Test.run) 
    results |> List.iter (printf "%s")    
    if results |> List.forall ((=) "OK\n") then 0 else 1
