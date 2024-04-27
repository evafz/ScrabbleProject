namespace FuncPro

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open System.IO
open ScrabbleUtil.DebugPrint

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =
    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State =
    type state = {
        board         : Parser.board
        dict          : Dictionary.Dict
        numPlayers    : uint32
        playerNumber  : uint32
        playerTurn    : uint32
        hand          : MultiSet.MultiSet<uint32>
    }

    let mkState b d np pn pt h = {board = b; dict = d; numPlayers = np; playerNumber = pn; playerTurn = pt; hand = h }
    let board st         = st.board
    let dict st          = st.dict
    let numPlayers st    = st.numPlayers
    let playerNumber st  = st.playerNumber
    let playerTurn st    = st.playerTurn
    let hand st          = st.hand

module Scrabble =
    open System.Threading
    //create trie method for finding a list of possible words given a start-string and the letters on my hand

    //Check if board is empty - if yes find word from your hand only
    //Else:
    //Find word on board by starting in center and going left or up until you find start of word (blank square)
    // Now go right or down until you find end of word (blank square), remember the letters as you go!
    //combine letters into word/string
    //check if that word as start-string + letters on hand are possible (method )
        //check if there is space for chosen word on board / no overlapping / no sidewords
        //if not find new word (rec?)
    //If not find new word (rec?)

    //method for finding the start of a word (first blank square)
    let rec findStartPos (st : State.state) (currentCoords : coord) = 
        match (st.board.squares currentCoords) with 
            | StateMonad.Success (Some _) -> findStartPos st (((fst currentCoords) - 1), (snd currentCoords))
            | StateMonad.Success None -> currentCoords
            | StateMonad.Failure err -> (0, 0)

    //method for finding the word that should start the word to be placed (assuming youve found the first blank square)
    //BUT WORD IS TYPE INT FOR SOME REASON, CANNOT FIND THE WORD OR ACCUMULATE CHARS YET
    let rec findStartWord (st : State.state) (coords : coord) (startWord) = 
        match (st.board.squares (fst coords + 1, snd coords)) with
            | StateMonad.Success (Some squareMap) ->
                match Map.tryFind (fst coords + 1) squareMap with
                    | Some squareFun ->
                        match squareFun [] 0 0 with
                            | StateMonad.Success word -> word
                            | StateMonad.Failure err -> startWord 
                    | None -> startWord
            | StateMonad.Success None -> startWord
            | StateMonad.Failure err -> startWord

    // let findPossibleWords (st : State.state) (hand : MultiSet.MultiSet<uint32>) =
    //     let result = List.Empty
    //     let handList = MultiSet.toList hand
    //     if (st.board.defaultSquare.IsEmpty) then
    //         result = getWords handList st.dict
    //     else 
    //         let startCoords = findStartPos st st.board.center
    //         let startWord = findStartWord st startCoords List.Empty
    //         result = getWords 

    //     return result
        
    let rec removeItem itm lst =
        match lst with
        | x::lst when x = itm -> lst
        | x::lst -> x::removeItem itm lst
        | _ -> []

    let getWords lst dict =
        let rec aux s lst dict =
            List.fold (fun acc elm -> 
                match Dictionary.step elm dict with
                    | None -> acc
                    | Some (b, newDict) ->
                        let newS = s + string elm
                        let newLst = removeItem elm lst
                        match b with
                            | false -> acc |> Set.union (aux newS newLst newDict)
                            | true -> acc |> Set.union (aux newS newLst newDict) |> Set.add newS
            ) Set.empty lst
        aux "" lst dict

    let playGame cstream pieces (st : State.state) =
        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            (*
            let result = getWords ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'] (State.dict st)
            for s in result do
                printf "%s \n" s
            *)
            
            let input =  System.Console.ReadLine()
            let move = RegEx.parseMove input

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move)
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move)

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                let st' = st

                // Update the hand
                let multiMs = MultiSet.ofList (List.map (fun x -> (fst(snd x))) ms)
                let lessHand = MultiSet.fold (fun acc elm _ -> MultiSet.removeSingle elm acc ) st'.hand multiMs
                
                let multiNewPieces = MultiSet.ofList (List.map (fun x -> fst x) newPieces)
                let moreHand = MultiSet.fold (fun acc elm _ -> MultiSet.addSingle elm acc ) lessHand multiNewPieces

                // Update player turn

                // Update board

                // Update points?

                // Maybe more should be updated?

                aux (State.mkState st'.board st'.dict st'.numPlayers st'.playerNumber st'.playerTurn moreHand)
            | RCM (CMPlayed (pid, ms, points)) ->
                let st' = st

                // Update player turn

                // Update board

                // Update points?

                // Maybe more should be updated?

                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                let st' = st

                // Update player turn

                // Maybe more should be updated?

                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st
        aux st

    let startGame
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        let dict = dictf false
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict numPlayers playerNumber playerTurn handSet)
