namespace FuncPro

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open System.IO
open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.
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
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.
    type state = {
        board         : Parser.board
        dict          : Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
    }

    let mkState b d pn h = {board = b; dict = d;  playerNumber = pn; hand = h }
    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
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

    let findPossibleWords (st : State.state) (hand : MultiSet.MultiSet<uint32>) =
        failwith "not implemented"
        //let words = st.board.squares st.board.center
        
    let rec removeItem item cs =
        match cs with
        | c::cs when c = item -> cs
        | c::cs -> c::removeItem item cs
        | _ -> []

    let getWords cs dict =
        let rec aux (s : string) cs dict =
            List.fold (fun acc elm -> 
                match Dictionary.step elm dict with
                    | None -> Set.empty
                    | Some (b, newDict) ->
                        let newS = s + string elm
                        let newCs = removeItem elm cs
                        printf "%s %b \n" newS b
                        match b with
                        | false -> acc |> Set.union (aux newS newCs newDict)
                        | true -> acc |> Set.union (aux newS newCs newDict) |> Set.add newS
            ) Set.empty cs
        aux "" cs dict

    let playGame cstream pieces (st : State.state) =
        let rec aux (st : State.state) =
            //Print.printHand pieces (State.hand st)

            let result = getWords ['T'; 'O'; 'P'] (State.dict st)
            printf "\n"

            for s in result do
                printf "%s \n" s

            (*
            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            let input =  System.Console.ReadLine()
            let move = RegEx.parseMove input

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->                
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = st // This state needs to be updated

                //update the hand
                let multiMs = MultiSet.ofList (List.map (fun x -> (fst(snd x))) ms)
                let lessHand = MultiSet.fold (fun acc elm _ -> MultiSet.removeSingle elm acc ) st'.hand multiMs
                
                let multiNewPieces = MultiSet.ofList (List.map (fun x -> fst x) newPieces)
                let moreHand = MultiSet.fold (fun acc elm _ -> MultiSet.addSingle elm acc ) lessHand multiNewPieces

                Print.printHand pieces (moreHand)

                aux (State.mkState st'.board st'.dict st'.playerNumber moreHand)

            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st
            *)
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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet)
