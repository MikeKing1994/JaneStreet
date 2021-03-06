open System

[<Struct>]
type Player = 
    | Aaron
    | Barron
    | Caren
    | Darrin

[<Struct>]
type Game =
    struct
        val mutable Aaron: bool
        val mutable Barron: bool
        val mutable Caren: bool
        val mutable Darrin: bool

        val LastPlayedIndex: int

        new(aaron: bool, barron: bool, caren: bool, darrin: bool, lastPlayedIndex: int) = 
            { Aaron = aaron; Barron = barron; Caren = caren; Darrin = darrin; LastPlayedIndex = lastPlayedIndex}
    end 
    
module Game = 
    let create() = 
        new Game(true, true, true, true, 0)

    let isOver (game: Game) = 
        let mutable remainingPlayers = 0
        if game.Aaron then remainingPlayers <- remainingPlayers + 1
        if game.Barron then remainingPlayers <- remainingPlayers + 1
        if game.Caren then remainingPlayers <- remainingPlayers + 1
        if game.Darrin then remainingPlayers <- remainingPlayers + 1
        remainingPlayers = 1

    let findWinner (game: Game) = 
        if game.Aaron then Player.Aaron
        else if game.Barron then Player.Barron
        else if game.Caren then Player.Caren
        else if game.Darrin then Player.Darrin
        else failwith "no winner found"


[<Struct>]
type GameState =
    struct
        val mutable IsOngoing: bool
        val mutable OngoingGame: Game
        val mutable WonBy: Nullable<Player>

        new(isOngoing: bool, game: Game, wonBy: Nullable<Player>) = 
            { IsOngoing = isOngoing; OngoingGame = game; WonBy = wonBy}
    end 

let rollDice (randomGenerator: Random) = 
    randomGenerator.Next(100)

let isEliminated randomGenerator = 
    rollDice randomGenerator < 50

let fireArrow randomGenerator (game: Game) = 
    let isEliminated = isEliminated randomGenerator

    if not isEliminated 
        then new Game(game.Aaron, game.Barron, game.Caren, game.Darrin, (game.LastPlayedIndex + 1)%4)
    else 
        if game.LastPlayedIndex = 0
            then new Game(game.Aaron, false, game.Caren, game.Darrin, 1)
        else if game.LastPlayedIndex = 1
            then new Game(game.Aaron, game.Barron, false, game.Darrin, 2)
        else if game.LastPlayedIndex = 2
            then new Game(game.Aaron, game.Barron, game.Caren, false, 3)
        else if game.LastPlayedIndex = 3
            then new Game(false, game.Barron, game.Caren, game.Darrin, 0)
        else 
            failwithf "Unrecognised LastPlayedIndex of %d" game.LastPlayedIndex

let playRound randomGenerator game = 
    let g = fireArrow randomGenerator game
    if Game.isOver g 
        then new GameState(false, g, Game.findWinner g)
    else 
        new GameState(true, g, Nullable())
    

let play randomGenerator = 
    let mutable game = new GameState(true, Game.create(), Nullable())

    while game.IsOngoing
        do game <- game.OngoingGame |> playRound randomGenerator

    match game.IsOngoing with 
    | true -> failwith "broke out of while loop, but game wasn't over"
    | false -> game.WonBy.Value

module Results = 
    [<Struct>]
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

    //let random = Random()
    //let mutable counts = Results.WinCounts.empty()
    //while true do 
    //    counts <- Results.runOneSimulation random counts
    //printfn "%A" counts
    0
