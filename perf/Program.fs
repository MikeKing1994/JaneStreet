open System

type Player = 
    | Aaron
    | Barron
    | Caren
    | Darrin

type Game = 
    {
        Aaron: bool
        Barron: bool
        Caren: bool
        Darrin: bool

        LastPlayedIndex: int
    }

module Game = 
    let create() = 
        {
            Aaron = true
            Barron = true
            Caren = true
            Darrin = true

            LastPlayedIndex = 0
        }

    let isOver game = 
        let mutable remainingPlayers = 0
        if game.Aaron then remainingPlayers <- remainingPlayers + 1
        if game.Barron then remainingPlayers <- remainingPlayers + 1
        if game.Caren then remainingPlayers <- remainingPlayers + 1
        if game.Darrin then remainingPlayers <- remainingPlayers + 1
        remainingPlayers = 1

    let findWinner game = 
        if game.Aaron then Player.Aaron
        else if game.Barron then Player.Barron
        else if game.Caren then Player.Caren
        else if game.Darrin then Player.Darrin
        else failwith "no winner found"

type GameState = 
    | Ongoing of Game
    | WonBy of Player

module GameState = 
    let isOngoing  = 
        function
        | Ongoing _ -> true 
        | _ -> false

    let unpackGame =  
        function
        | Ongoing g -> g 
        | _ -> failwith "tried to unpack finished game"


let rollDice (randomGenerator: Random) = 
    randomGenerator.Next(100)

let isEliminated randomGenerator = 
    rollDice randomGenerator < 50

let fireArrow randomGenerator game = 
    let isEliminated = isEliminated randomGenerator

    if not isEliminated 
        then { game with LastPlayedIndex = (game.LastPlayedIndex + 1)%4}
    else 
        if game.LastPlayedIndex = 0
            then { game with Barron = false }
        else if game.LastPlayedIndex = 1
            then { game with Caren = false }
        else if game.LastPlayedIndex = 2
            then { game with Darrin = false }
        else if game.LastPlayedIndex = 3
            then { game with Aaron = false }
        else 
            failwithf "Unrecognised LastPlayedIndex of %d" game.LastPlayedIndex

let playRound randomGenerator game = 
    let g = fireArrow randomGenerator game
    if Game.isOver g 
        then Game.findWinner g |> GameState.WonBy
    else 
        GameState.Ongoing g
    

let play randomGenerator = 
    let mutable game = Game.create() |> GameState.Ongoing

    while GameState.isOngoing game 
        do game <- game |> GameState.unpackGame |> playRound randomGenerator

    match game with 
    | Ongoing _ -> failwith "broke out of while loop, but game wasn't over"
    | WonBy winner -> winner

module Results = 
    type WinCounts = 
        {
            TotalGames: int
            Aaron: int
            Barron: int
            Caren: int
            Darrin: int
        }

    module WinCounts = 
        let empty() = 
            {
                TotalGames = 0
                Aaron = 0
                Barron = 0
                Caren = 0
                Darrin = 0
            }

        let merge a b = 
            {
                TotalGames = a.TotalGames + b.TotalGames
                Aaron = a.Aaron + b.Aaron
                Barron = a.Barron + b.Barron
                Caren = a.Caren + b.Caren
                Darrin = a.Darrin + b.Darrin
            }

        let mergeArray counts = 
            counts |> Array.reduce merge

    let runOneSimulation randomGenerator winCounts = 
        let winner = play randomGenerator
        match winner with 
        | Player.Aaron -> 
            { winCounts with
                Aaron = winCounts.Aaron + 1
                TotalGames = winCounts.TotalGames + 1    
            }
        | Player.Barron -> 
            { winCounts with
                Barron = winCounts.Barron + 1
                TotalGames = winCounts.TotalGames + 1    
            }
        | Player.Caren -> 
            { winCounts with
                Caren = winCounts.Caren + 1
                TotalGames = winCounts.TotalGames + 1    
            }
        | Player.Darrin -> 
            { winCounts with
                Darrin = winCounts.Darrin + 1
                TotalGames = winCounts.TotalGames + 1    
            }    

    let runBatch batch = 
        async {
            let mutable counts = WinCounts.empty()
            let randomGenerator = new Random()
            batch |> Array.iter (fun _ -> counts <- runOneSimulation randomGenerator counts )
            return counts
        }

    let runSimulation degreeOfParallelism gameCount =      
        let allGames = [|0..gameCount-1|]
        
        let splitIntoChunks = allGames |> Array.chunkBySize (gameCount/degreeOfParallelism)
        let tasks = splitIntoChunks |> Array.map (fun chunk -> runBatch chunk)


        let allResults = tasks |> Async.Parallel |> Async.RunSynchronously 
        allResults |> WinCounts.mergeArray


[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let gameCount = 10000000
    let degreeOfParallelism = 10
    let ret = Results.runSimulation degreeOfParallelism gameCount    
    stopWatch.Stop()
    printfn "Ran %d simulations in %f ms, using %d threads" gameCount stopWatch.Elapsed.TotalMilliseconds degreeOfParallelism
    printfn "%A" ret

    //let mutable counts = Results.WinCounts.empty()
    //while true do 
    //    counts <- Results.runOneSimulation counts
    //printfn "%A" counts
    0
