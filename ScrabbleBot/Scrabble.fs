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
        //board         : Parser.board
        boardFun      : coord -> bool
        dict          : Dictionary.Dict
        numPlayers    : uint32
        playerNumber  : uint32
        playerTurn    : uint32
        hand          : MultiSet.MultiSet<uint32>
        playedLetters : List<coord * (uint32 * (char * int))>
    }

    //let mkState b d np pn pt h pl = {board = b; dict = d; numPlayers = np; playerNumber = pn; playerTurn = pt; hand = h; playedLetters = pl}
    //let board st         = st.board
    let mkState b d np pn pt h pl = {boardFun = b; dict = d; numPlayers = np; playerNumber = pn; playerTurn = pt; hand = h; playedLetters = pl}
    let boardFun st = st.boardFun
    let dict st          = st.dict
    let numPlayers st    = st.numPlayers
    let playerNumber st  = st.playerNumber
    let playerTurn st    = st.playerTurn
    let hand st          = st.hand
    let playedLetters st = st.playedLetters

module Scrabble =
    open System.Threading

    //method for finding the start of a word (first blank square)
    let coordIsOccupied (playedLetters : List<coord * (uint32 * (char * int))>) (coord : coord) = 
        List.exists (fun (c, _) -> c = coord) playedLetters

    let rec findStartPos (st : State.state) (currentCoords : coord) = 
        match coordIsOccupied st.playedLetters currentCoords with 
            | true -> findStartPos st (fst currentCoords + 1, snd currentCoords)
            | false -> currentCoords

    //find startWord:
    let rec findStartWord (playedLetters : List<coord * (uint32 * (char * int))>) (startPos : coord) =
        match playedLetters with
            |[] -> []
            | ((x,y), (_, (letter, _))) :: rest -> 
                if (x, y) = startPos then
                    letter :: findStartWord rest (x + 1, y)
                else
                    findStartWord rest startPos

    
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

    let findPossibleWords (st : State.state) (hand : MultiSet.MultiSet<uint32>) =
        //converts multiset<uint32> to list<char>
        let handList = List.map (fun i -> char (uint i + 96u)) (MultiSet.toList hand)

        let possibleWords = Set.empty
        if st.playedLetters.IsEmpty then
            possibleWords = getWords handList st.dict
        else
            let startPos = findStartPos st (0,0) //or other square where we know something is placed, could be from playedLetters list?
            let startWord = findStartWord st.playedLetters startPos

            //this does not put the startwords strictly in front of the rest, needs more work
            possibleWords = getWords (List.append startWord handList) st.dict

            //still needs to check if chosen word is possible on the board (no overlapping, no sidewords, no nothing)


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
                let newPlayerTurn =
                    match st'.playerTurn with
                    | x when x = st'.numPlayers -> 1u
                    | x -> x + 1u

                // Update board / playedLetters
                let newPlayedLetters = List.append st'.playedLetters ms

                // Update points?

                // Maybe more should be updated?

                aux (State.mkState st'.boardFun st'.dict st'.numPlayers st'.playerNumber newPlayerTurn moreHand newPlayedLetters)
            | RCM (CMPlayed (pid, ms, points)) ->
                let st' = st

                // Update player turn
                let newPlayerTurn =
                    match st'.playerTurn with
                    | x when x = st'.numPlayers -> 1u
                    | x -> x + 1u

                // Update board / playedLetters
                let newPlayedLetters = List.append st'.playedLetters ms

                // Update points?

                // Maybe more should be updated?

                aux (State.mkState st'.boardFun st'.dict st'.numPlayers st'.playerNumber newPlayerTurn st'.hand newPlayedLetters)
            | RCM (CMPlayFailed (pid, ms)) ->
                let st' = st

                // Update player turn
                let newPlayerTurn =
                    match st'.playerTurn with
                    | x when x = st'.numPlayers -> 1u
                    | x -> x + 1u

                // Maybe more should be updated?

                aux (State.mkState st'.boardFun st'.dict st'.numPlayers st'.playerNumber newPlayerTurn st'.hand st'.playedLetters)
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
        //let board = Parser.mkBoard boardP
        let boardFun = Parser.mkBoardFun boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        let playedLetters = List.Empty

        //fun () -> playGame cstream tiles (State.mkState board dict numPlayers playerNumber playerTurn handSet playedLetters)
        fun () -> playGame cstream tiles (State.mkState boardFun dict numPlayers playerNumber playerTurn handSet playedLetters)
