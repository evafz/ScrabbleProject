namespace FuncPro

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open System.IO
open ScrabbleUtil.DebugPrint

module Print =
    let printHand tiles hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x tiles) i)) ()

module State =
    type state = {
        board         : coord -> bool
        center        : coord
        dict          : Dictionary.Dict
        numPlayers    : uint32
        playerNumber  : uint32
        playerTurn    : uint32
        hand          : MultiSet.MultiSet<uint32>
        playedLetters : Map<coord, (uint32 * (char * int))>
    }

    let mkInitialState b c d np pn pt h pl = {board = b; center = c; dict = d; numPlayers = np; playerNumber = pn; playerTurn = pt; hand = h; playedLetters = pl}
    let mkState st np pn pt h pl = {board = st.board; center = st.center; dict = st.dict; numPlayers = np; playerNumber = pn; playerTurn = pt; hand = h; playedLetters = pl}
    let board st = st.board
    let center st = st.center
    let dict st = st.dict
    let numPlayers st = st.numPlayers
    let playerNumber st = st.playerNumber
    let playerTurn st = st.playerTurn
    let hand st = st.hand
    let playedLetters st = st.playedLetters

module Scrabble =
    open System.Threading

    // Remove first occurence of element in list
    let rec removeElement itm lst =
        match lst with
        | x::lst when x = itm -> lst
        | x::lst -> x::removeElement itm lst
        | _ -> []

    // Find all legal words by giving a list of letters
    let getWords dict lst =
        let rec aux word dict lst =
            List.fold (fun acc elm -> 
                match Dictionary.step (fst (snd elm)) dict with
                | None -> acc
                | Some (b, newDict) ->
                    let newWord = word @ [elm]
                    let newLst = removeElement elm lst
                    match b with
                    | false -> Set.union (aux newWord newDict newLst) acc
                    | true -> Set.union (aux newWord newDict newLst) acc |> Set.add newWord
            ) Set.empty lst
        aux List.empty dict lst

    let getContinuations dict lst word =
        getWords (
            List.fold (fun acc elm ->
                match Dictionary.step (fst (snd (snd elm))) acc with
                | None -> acc
                | Some (_, newDict) -> newDict
            ) dict word
        ) lst

    let getTiles st tiles =
        State.hand st |>
        MultiSet.toList |>
        List.map (fun elm -> (elm, Set.minElement (Map.find elm tiles)))

    let rec getWordHorizontal pl (x, y) =
        match Map.containsKey (x, y) pl with
        | true -> [((x, y), Map.find (x, y) pl)] @ getWordHorizontal pl (x + 1, y)
        | false -> List.empty

    let rec getWordVertical pl (x, y) =
        match Map.containsKey (x, y) pl with
        | true -> [((x, y), Map.find (x, y) pl)] @ getWordVertical pl (x, y + 1)
        | false -> List.empty

    let getMovesHorizontal st tiles words =
        let aux dict lst move =
            getContinuations dict lst move

        let lst = getTiles st tiles
        let dict = State.dict st

        List.fold (fun acc elm -> 
            let endCoord = 
                List.fold (fun acc elm ->
                    let coord = fst elm

                    match fst coord > fst acc with
                    | true -> coord
                    | false -> acc
                ) (-1000, -1000) elm
            
            acc @ [(endCoord, aux dict lst elm)]
        ) List.empty words   

    let getMovesVertical st tiles words =
        let aux dict lst move =
            getContinuations dict lst move

        let lst = getTiles st tiles
        let dict = State.dict st

        List.fold (fun acc elm -> 
            let endCoord = 
                List.fold (fun acc elm ->
                    let coord = fst elm

                    match snd coord > snd acc with
                    | true -> coord
                    | false -> acc
                ) (-1000, -1000) elm
            
            acc @ [(endCoord, aux dict lst elm)]
        ) List.empty words 


    let pruneHorizontalMoves st pl moves start=
            List.fold (fun acc elm ->             
                let rec aux v (x, y) =
                    match Map.containsKey (x + 1, y) pl || Map.containsKey (x, y + 1) pl || Map.containsKey (x, y - 1) pl || not (State.board st (x, y)) || v > 7 with
                    | true -> v
                    | false -> aux (v + 1) (x + 1, y)
                let maxLength =
                    match start with
                    | true -> aux 0 (fst (fst elm), snd (fst elm))
                    | false -> aux 0 (fst (fst elm) + 1, snd (fst elm))

                acc @ 
                    [(fst elm, Set.fold (fun acc elm -> 
                        match List.length elm > maxLength with
                        | true -> acc
                        | false -> Set.add elm acc
                    ) Set.empty (snd elm))]
            ) List.empty moves

    let pruneVerticalMoves st pl moves start =
        List.fold (fun acc elm ->             
            let rec aux v (x, y) =
                match Map.containsKey (x, y + 1) pl || Map.containsKey (x + 1, y) pl || Map.containsKey (x - 1, y) pl || not (State.board st (x, y)) || v > 7 with
                | true -> v
                | false -> aux (v + 1) (x, y + 1)

            let maxLength =
                    match start with
                    | true -> aux 0 (fst (fst elm), snd (fst elm))
                    | false -> aux 0 (fst (fst elm), snd (fst elm) + 1)

            acc @ 
                [(fst elm, Set.fold (fun acc elm -> 
                    match List.length elm > maxLength with
                    | true -> acc
                    | false -> Set.add elm acc
                ) Set.empty (snd elm))]
        ) List.empty moves

    let getMove st tiles =
        let pl = State.playedLetters st

        let horizontalWords = 
            Map.fold (fun acc (x, y) _ -> 
                match Map.containsKey (x - 1, y) pl with
                | true -> acc
                | false -> acc @ [getWordHorizontal pl (x, y)]
            ) List.empty pl

        let horizontalMoves = getMovesHorizontal st tiles horizontalWords
        let prunedHorizontalMoves = pruneHorizontalMoves st pl horizontalMoves false
        let coordedHorizontalMoves = 
            List.map (fun elm -> 
                let (x, y) = fst elm

                Set.map (fun elm -> 
                    (List.mapi (fun i elm -> ((x + i + 1, y), elm)) elm)
                ) (snd elm)
            ) prunedHorizontalMoves

        let horizontal = 
            List.fold (fun acc elm -> 
                Set.union elm acc
            ) Set.empty coordedHorizontalMoves

        let verticalWords = 
            Map.fold (fun acc (x, y) _ -> 
                match Map.containsKey (x, y - 1) pl with
                | true -> acc
                | false -> acc @ [getWordVertical pl (x, y)]
            ) List.empty pl

        let verticalMoves = getMovesVertical st tiles verticalWords
        let prunedVerticalMoves = pruneVerticalMoves st pl verticalMoves false
        let coordedVerticalMoves = 
            List.map (fun elm -> 
                let (x, y) = fst elm

                Set.map (fun elm -> 
                    (List.mapi (fun i elm -> ((x, y + i + 1), elm)) elm)
                ) (snd elm)
            ) prunedVerticalMoves

        let vertical =
            List.fold (fun acc elm -> 
                Set.union elm acc
            ) Set.empty coordedVerticalMoves

        Set.union vertical horizontal

    let getNewPlayedLetters st ms = List.fold (fun acc (coord, tile) -> Map.add coord tile acc) (State.playedLetters st) ms
    let addPieces pieces hand = List.fold (fun acc (c, n) -> MultiSet.add c n acc) hand pieces
    let getNextPlayerTurn st =
        match State.playerTurn st with
        | x when x = State.numPlayers st -> 1u
        | x -> x + 1u

    let playGame cstream tiles (st : State.state) =
        let rec aux (st : State.state) =
            match State.playerNumber st = State.playerTurn st with
            | true ->
                match State.board st (State.center st) && Map.isEmpty (State.playedLetters st) with
                | true ->
                    let words = getWords (State.dict st) (getTiles st tiles)
                    let wordsLst = [(State.center st, words)]
                    let pl = st.playedLetters
                    let horStartMove = pruneHorizontalMoves st pl wordsLst true
                    let coordedHorizontalMoves = 
                        List.map (fun elm -> 
                            let (x, y) = fst elm

                            Set.map (fun elm -> 
                                (List.mapi (fun i elm -> ((x + i, y), elm)) elm)
                            ) (snd elm)
                        ) horStartMove

                    let horizontal = 
                        List.fold (fun acc elm -> 
                        Set.union elm acc
                        ) Set.empty coordedHorizontalMoves

                    let verStartMove = pruneVerticalMoves st pl wordsLst true
                    let coordedVerticalMoves = 
                        List.map (fun elm -> 
                            let (x, y) = fst elm

                            Set.map (fun elm -> 
                                (List.mapi (fun i elm -> ((x, y + i), elm)) elm)
                            ) (snd elm)
                        ) verStartMove

                    let vertical = 
                        List.fold (fun acc elm -> 
                        Set.union elm acc
                        ) Set.empty coordedVerticalMoves

                    let bestMove =
                        Set.fold (fun acc elm -> 
                            match List.length acc < List.length elm with
                            | true -> elm
                            | false -> acc
                        ) List.empty (Set.union horizontal vertical)

                    match List.length bestMove with
                    | 0 -> send cstream (SMChange (MultiSet.toList (State.hand st)))
                    | _ -> send cstream (SMPlay bestMove)
                
                | false -> 
                    let moves = getMove st tiles

                    let bestMove = 
                        Set.fold (fun acc elm -> 
                            match List.length acc < List.length elm with
                            | true -> elm
                            | false -> acc
                        ) List.empty moves

                    match List.length bestMove with
                    | 0 -> send cstream SMPass
                    | _ -> send cstream (SMPlay bestMove)
            | false -> ()

            match recv cstream with
            | RCM (CMPlaySuccess(ms, _, newPieces)) ->
                let newPlayerTurn = getNextPlayerTurn st
                let newPlayedLetters =  getNewPlayedLetters st ms
                let newHand = List.fold (fun acc elm -> MultiSet.removeSingle (fst (snd elm)) acc) (State.hand st) ms |> addPieces newPieces
                aux (State.mkState st (State.numPlayers st) (State.playerNumber st) newPlayerTurn newHand newPlayedLetters)
            | RCM (CMPlayed (_, ms, _)) ->
                let newPlayerTurn = getNextPlayerTurn st
                let newPlayedLetters = getNewPlayedLetters st ms
                aux (State.mkState st (State.numPlayers st) (State.playerNumber st) newPlayerTurn (State.hand st) newPlayedLetters)
            | RCM (CMChangeSuccess newPieces) ->
                let newPlayerTurn = getNextPlayerTurn st
                let newHand = addPieces newPieces MultiSet.empty
                aux (State.mkState st (State.numPlayers st) (State.playerNumber st) newPlayerTurn newHand (State.playedLetters st)) 
            | RCM (CMForfeit _) ->
                let newNumPlayers = State.numPlayers st - 1u
                
                let newPlayerNumber =
                    match State.playerNumber st with
                    | n when n > State.playerTurn st -> n - 1u
                    | n when n > newNumPlayers -> n - 1u
                    | n -> n
                
                let newPlayerTurn =
                    match State.playerTurn st with
                    | x when x > newNumPlayers -> 1u
                    | x -> x

                aux (State.mkState st newNumPlayers newPlayerNumber newPlayerTurn (State.hand st) (State.playedLetters st))
            | RCM (CMGameOver _) -> ()
            | RCM _ ->
                let newPlayerTurn = getNextPlayerTurn st
                aux (State.mkState st (State.numPlayers st) (State.playerNumber st) newPlayerTurn (State.hand st) (State.playedLetters st))
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
        let boardFun = Parser.mkBoardFun boardP
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        let playedLetters = Map.empty

        fun () -> playGame cstream tiles (State.mkInitialState boardFun boardP.center dict numPlayers playerNumber playerTurn handSet playedLetters)
