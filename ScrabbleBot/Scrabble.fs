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
        playedLetters : List<coord * (uint32 * (char * int))>
    }

    let mkState b d np pn pt h pl = {board = b; dict = d; numPlayers = np; playerNumber = pn; playerTurn = pt; hand = h; playedLetters = pl}
    let board st         = st.board
    let dict st          = st.dict
    let numPlayers st    = st.numPlayers
    let playerNumber st  = st.playerNumber
    let playerTurn st    = st.playerTurn
    let hand st          = st.hand
    let playedLetters st = st.playedLetters

module Scrabble =
    open System.Threading

    // method for finding the start of a word (first blank square)
    let coordIsOccupied (playedLetters : List<coord * (uint32 * (char * int))>) (coord : coord) = 
        List.exists (fun (c, _) -> c = coord) playedLetters

    let rec findStartPos (st : State.state) (currentCoords : coord) = 
        match coordIsOccupied st.playedLetters currentCoords with 
            | true -> findStartPos st (fst currentCoords + 1, snd currentCoords)
            | false -> currentCoords

    // Find start word
    let rec findStartWord (playedLetters : List<coord * (uint32 * (char * int))>) (startPos : coord) =
        match playedLetters with
            |[] -> []
            | ((x,y), (_, (letter, _))) :: rest -> 
                if (x, y) = startPos then
                    letter :: findStartWord rest (x + 1, y)
                else
                    findStartWord rest startPos

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
                            | false -> acc |> Set.union (aux newWord newDict newLst)
                            | true -> acc |> Set.union (aux newWord newDict newLst) |> Set.add newWord
            ) Set.empty lst
        aux List.empty dict lst

    let getWordContinuations (dict : Dictionary.Dict) lst word =
       
        getWords (List.fold (fun acc elm ->
            match Dictionary.step (fst (snd elm)) acc with
                | None -> acc
                | Some (b, newDict) -> newDict
        ) dict word) lst 

    //Helper function to check that a word actually starts with a specific sequence of letters (another word)
    let rec startsWithLetters (letters : list<'a * (char * 'b)>) (word : list<'a * (char * 'b)>) =
        let rec startsWithLettersHelper letters word = 
            match letters, word with
                |[], _ -> true
                |_, [] -> false
                |(_, (x, _))::xs, (_, (y, _))::ys -> if x=y then startsWithLettersHelper xs ys
                                                            else false
        startsWithLettersHelper letters word

    //getWords but with start letters/already placed word
    let getWordsWithStartLetters dict lst startLetters=
        let rec aux word dict lst =
            List.fold (fun acc elm -> 
                match Dictionary.step (fst (snd elm)) dict with
                    | None -> acc
                    | Some (b, newDict) ->
                        let newWord = word @ [elm]
                        let newLst = removeElement elm lst
                        match b with
                            | false -> acc |> Set.union (aux newWord newDict newLst)
                            | true -> let words = aux newWord newDict newLst
                                      if startsWithLetters startLetters newWord then
                                        acc |> Set.union (Set.add newWord words)
                                      else acc |> Set.union words
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

    let playGame cstream tiles (st : State.state) =
        let rec aux (st : State.state) =
            Thread.Sleep 1000
            (*
            let result = getWordContinuations (State.dict st) [(1,('E',1));(1,('L',1));(1,('R',1));(1,('S',1));] [(1,('T',1));(1,('O',1));(1,('W',1));];
            for word in result do
                forcePrint " :"
                for char in word do
                    forcePrint (string (fst (snd char)))
            *)
                 
            if st.playerNumber = st.playerTurn then
                if (State.board st) (0, 0) && List.isEmpty (State.playedLetters st) then
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
                else
                    // Switch this out with the code for finding words when the board is not empty.
                    send cstream SMPass

            match recv cstream with
            | RCM (CMPlaySuccess(ms, _, newPieces)) ->
                let st' = st
                
                let newHand = 
                    MultiSet.ofList (List.map (fun x -> fst (snd x)) ms) |>
                    MultiSet.fold (fun acc elm _ -> MultiSet.removeSingle elm acc) (State.hand st') |>
                    MultiSet.fold (fun acc elm _ -> MultiSet.addSingle elm acc) <|
                    MultiSet.ofList (List.map (fun x -> fst x) newPieces)

                let newPlayerTurn =
                    match st'.playerTurn with
                    | x when x = st'.numPlayers -> 1u
                    | x -> x + 1u

                let newPlayedLetters = List.append st'.playedLetters ms

                aux (State.mkState st'.board st'.dict st'.numPlayers st'.playerNumber newPlayerTurn newHand newPlayedLetters)
            | RCM (CMPlayed (_, ms, _)) ->
                let st' = st

                let newPlayerTurn =
                    match st'.playerTurn with
                    | x when x = st'.numPlayers -> 1u
                    | x -> x + 1u

                let newPlayedLetters = List.append st'.playedLetters ms

                aux (State.mkState st'.board st'.dict st'.numPlayers st'.playerNumber newPlayerTurn st'.hand newPlayedLetters)
            | RCM (CMPassed _) ->
                let st' = st

                let newPlayerTurn =
                    match st'.playerTurn with
                    | x when x = st'.numPlayers -> 1u
                    | x -> x + 1u

                aux (State.mkState st'.board st'.dict st'.numPlayers st'.playerNumber newPlayerTurn st'.hand st'.playedLetters)
            |RCM (CMChangeSuccess newPieces) ->
                let st' = st

                let newHand = 
                    MultiSet.ofList <|
                    List.fold (fun acc elm -> 
                        let rec aux elm =
                            match elm with
                            | (c, 1u) -> [c] 
                            | (c, n) -> [c] @ aux (c, n - 1u)              
                        acc @ aux elm
                    ) List.Empty newPieces
                
                let newPlayerTurn =
                    match st'.playerTurn with
                    | x when x = st'.numPlayers -> 1u
                    | x -> x + 1u

                aux (State.mkState st'.board st'.dict st'.numPlayers st'.playerNumber newPlayerTurn newHand st'.playedLetters) 
            | RCM (CMChange _) ->
                let st' = st

                let newPlayerTurn =
                    match st'.playerTurn with
                    | x when x = st'.numPlayers -> 1u
                    | x -> x + 1u

                aux (State.mkState st'.board st'.dict st'.numPlayers st'.playerNumber newPlayerTurn st'.hand st'.playedLetters)
            | RCM (CMPlayFailed _) ->
                let st' = st

                let newPlayerTurn =
                    match st'.playerTurn with
                    | x when x = st'.numPlayers -> 1u
                    | x -> x + 1u

                aux (State.mkState st'.board st'.dict st'.numPlayers st'.playerNumber newPlayerTurn st'.hand st'.playedLetters)
            | RCM (CMTimeout _) ->
                let st' = st

                let newPlayerTurn =
                    match st'.playerTurn with
                    | x when x = st'.numPlayers -> 1u
                    | x -> x + 1u

                aux (State.mkState st'.board st'.dict st'.numPlayers st'.playerNumber newPlayerTurn st'.hand st'.playedLetters)
            | RCM (CMForfeit _) ->
                let st' = st

                let newNumPlayers = st'.numPlayers - 1u
                
                let newPlayerNumber =
                    match st'.playerNumber with
                    | x when x > st'.playerTurn -> st'.playerNumber - 1u
                    | x when x > newNumPlayers -> st'.playerNumber - 1u
                    | x -> x

                let newPlayerTurn =
                    match st'.playerTurn with
                    | x when x > newNumPlayers -> 1u
                    | x -> x

                aux (State.mkState st'.board st'.dict newNumPlayers newPlayerNumber newPlayerTurn st'.hand st'.playedLetters)
            | RCM (CMGameOver _) -> ()
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
        let playedLetters = List.Empty

        fun () -> playGame cstream tiles (State.mkState boardFun dict numPlayers playerNumber playerTurn handSet playedLetters)
