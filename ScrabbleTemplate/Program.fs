﻿let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start)

let readLines filePath = System.IO.File.ReadLines(filePath)

let spawnMultiples name dict bot =
    let rec aux =
        function 
        | 0 -> []
        | x -> (sprintf "%s%d" name x, dict, bot)::aux (x - 1)
    aux >> List.rev

[<EntryPoint>]
let main argv =
    ScrabbleUtil.DebugPrint.toggleDebugPrint false

    System.Console.BackgroundColor <- System.ConsoleColor.Black
    System.Console.ForegroundColor <- System.ConsoleColor.White
    System.Console.Clear()

    //let board        = ScrabbleUtil.StandardBoard.standardBoard ()
    let board        = ScrabbleUtil.InfiniteBoard.infiniteBoard ()
    //let board        = ScrabbleUtil.RandomBoard.randomBoard ()
    //let board        = ScrabbleUtil.RandomBoard.randomBoardSeed (Some 42)
    //let board        = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoard ()
    //let board        = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoardSeed (Some 42)
    //let board        = ScrabbleUtil.HoleBoard.holeBoard ()
    //let board        = ScrabbleUtil.InfiniteHoleBoard.infiniteHoleBoard ()

    let words      = readLines "Dictionaries/English.txt"
    let handSize   = 7u
    let timeout    = None
    let tiles      = ScrabbleUtil.English.tiles 1u
    let seed       = None
    let port       = 13001

    let dictAPI = Some (Dictionary.empty, Dictionary.insert, Dictionary.step, None)     

    let (dictionary, _) = time (fun () -> ScrabbleUtil.Dictionary.mkDict words dictAPI)
    
    let players = [("FunctionalBot", dictionary, FunctionalBot.Scrabble.startGame)] @ ["OxypenButazone", dictionary, Oxyphenbutazone.Scrabble.startGame]
    //let players = spawnMultiples "FunctionalBot" dictionary FunctionalBot.Scrabble.startGame 1

    do ScrabbleServer.Comm.startGame 
          board dictionary handSize timeout tiles seed port players
    
    ScrabbleUtil.DebugPrint.forcePrint ("Server has terminated. Press Enter to exit program.\n")
    System.Console.ReadLine () |> ignore
    0
