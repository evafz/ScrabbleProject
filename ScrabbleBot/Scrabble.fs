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
        dict          : Dictionary.Dict
        numPlayers    : uint32
        playerNumber  : uint32
        playerTurn    : uint32
        hand          : MultiSet.MultiSet<uint32>
        playedLetters : Map<coord, (uint32 * (char * int))>
    }

    let mkInitialState b d np pn pt h pl = {board = b; dict = d; numPlayers = np; playerNumber = pn; playerTurn = pt; hand = h; playedLetters = pl}
    let mkState st np pn pt h pl = {board = st.board; dict = st.dict; numPlayers = np; playerNumber = pn; playerTurn = pt; hand = h; playedLetters = pl}
    let board st = st.board
    let dict st = st.dict
    let numPlayers st = st.numPlayers
    let playerNumber st = st.playerNumber
    let playerTurn st = st.playerTurn
    let hand st = st.hand
    let playedLetters st = st.playedLetters

module Scrabble =
    open System.Threading

    // method for finding the start of a word (first blank square)
    // let coordIsOccupied (playedLetters : List<coord * (uint32 * (char * int))>) (coord : coord) = 
    //     List.exists (fun (c, _) -> c = coord) playedLetters

    // let rec findStartPos (st : State.state) (currentCoords : coord) = 
    //     match coordIsOccupied st.playedLetters currentCoords with 
    //         | true -> findStartPos st (fst currentCoords + 1, snd currentCoords)
    //         | false -> currentCoords

    (*
    let findRestOfWord (startPos : coord ) (playedLetters : Map<coord, (uint32 * (char * int))>) =
        //let listPlayedLetters = Map.fold (fun acc elm -> List. elm acc) List.Empty playedLetters
        let rec aux word startPos (playedLetters :  Map<coord, (uint32 * (char * int))>) = 
            Map.fold 
            match playedLetters with
                | ((x,y), (_, (letter, _))) -> if x = (fst startPos) + 1 then aux (letter :: word) (x, y) playedLetters

        let word = List.Empty
        aux word startPos playedLetters
    *)

    // Find start word
    (*
    let rec findWordHorizontal (playedLetters : Map<coord, (uint32 * (char * int))>) =
        match playedLetters with
            |((x,y), (_, (letter, _))) -> 
                    if !(Map.containsKey (x-1,y) playedLetters) then
                        match 
                // if Map. ((x,y), (1u, ('T', 2))) playedLetters then
                //     letter :: findWordHorizontal rest (x + 1, y)
                // else
                //     findWordHorizontal rest startPos
    *)

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

    (*
        let result = getWordContinuations (State.dict st) [(1,('E',1));(1,('L',1));(1,('R',1));(1,('S',1));] [(1,('T',1));(1,('O',1));(1,('W',1));];
        for word in result do
            forcePrint " :"
            for char in word do
                forcePrint (string (fst (snd char)))
    *)
    let getWordContinuations dict lst word =
        getWords (
            List.fold (fun acc elm ->
                match Dictionary.step (fst (snd elm)) acc with
                | None -> acc
                | Some (_, newDict) -> newDict
            ) dict word
        ) lst

    // Helper function to check that a word actually starts with a specific sequence of letters (another word)
    let rec startsWithLetters letters word =
        let rec startsWithLettersHelper letters word = 
            match letters, word with
            | [], _ -> true
            | _, [] -> false
            | (_, (x, _))::xs, (_, (y, _))::ys -> 
                match x = y with 
                | true -> startsWithLettersHelper xs ys
                | false -> false
        startsWithLettersHelper letters word

    // getWords but with start letters/already placed word
    let getWordsWithStartLetters dict lst startLetters =
        let rec aux word dict lst =
            List.fold (fun acc elm -> 
                match Dictionary.step (fst (snd elm)) dict with
                | None -> acc
                | Some (b, newDict) ->
                    let newWord = word @ [elm]
                    let newLst = removeElement elm lst
                    match b with
                    | false -> Set.union (aux newWord newDict newLst) acc
                    | true -> 
                        let words = aux newWord newDict newLst
                        match startsWithLetters startLetters newWord with
                        | true -> Set.union (Set.add newWord words) acc
                        | false -> Set.union words acc      
            ) Set.empty lst
        aux List.empty dict lst

    (*
    let findPossibleWords (st : State.state) (hand : MultiSet.MultiSet<uint32>) =
        //converts multiset<uint32> to list<char>
        let handList = List.map (fun i -> char (uint i + 96u)) (MultiSet.toList hand)

        let possibleWords = Set.empty
        if st.playedLetters.IsEmpty then
            possibleWords = getWords st.dict handList
        else
            let startPos = findStartPos st (0,0) //or other square where we know something is placed, could be from playedLetters list?
            let startWord = findStartWord st.playedLetters startPos

            //this does not put the startwords strictly in front of the rest, needs more work
            possibleWords = getWords st.dict (List.append startWord handList)

            //still needs to check if chosen word is possible on the board (no overlapping, no sidewords, no nothing)
    *)

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
                match State.board st (0, 0) && Map.isEmpty (State.playedLetters st) with
                | true ->
                    let cs = 
                        State.hand st |>
                        MultiSet.toList |>
                        List.map (fun elm -> (elm, Set.minElement (Map.find elm tiles)))

                    let words = getWords (State.dict st) cs
                    
                    let bestWord = 
                        Set.fold (fun acc elm -> 
                            match List.length acc < List.length elm with
                            | true -> elm
                            | false -> acc
                        ) List.empty words

                    match List.length bestWord with
                    | 0 -> send cstream (SMChange (MultiSet.toList (State.hand st)))
                    | _ -> send cstream (SMPlay (List.mapi (fun i elm -> ((i, 0), elm)) bestWord))
                
                // Change the "| false -> send cstream SMPass", with code that calculates a move when the board is not empty.
                | false -> send cstream SMPass
            | false -> ()

            match recv cstream with
            | RCM (CMPlaySuccess(ms, _, newPieces)) ->
                let newHand = List.fold (fun acc elm -> MultiSet.removeSingle (fst (snd elm)) acc) (State.hand st) ms |> addPieces newPieces
                let newPlayerTurn = getNextPlayerTurn st
                let newPlayedLetters =  getNewPlayedLetters st ms
                aux (State.mkState st (State.numPlayers st) (State.playerNumber st) newPlayerTurn newHand newPlayedLetters)
            | RCM (CMPlayed (_, ms, _)) ->
                let newPlayerTurn = getNextPlayerTurn st
                let newPlayedLetters = getNewPlayedLetters st ms
                aux (State.mkState st (State.numPlayers st) (State.playerNumber st) newPlayerTurn (State.hand st) newPlayedLetters)
            | RCM (CMChangeSuccess newPieces) ->
                let newHand = addPieces newPieces MultiSet.empty
                let newPlayerTurn = getNextPlayerTurn st
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

        fun () -> playGame cstream tiles (State.mkInitialState boardFun dict numPlayers playerNumber playerTurn handSet playedLetters)
