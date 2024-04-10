module StateMonad
    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string   
        | EmptyStack        

    type Result<'a, 'b> =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved =
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
            match a s with
            | Success (b, s') -> 
                match f b with 
                | S g -> g s'
            | Failure err -> Failure err)

    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err : SM<'a> = S (fun _ -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))
    let pop : SM<unit> = S (fun s -> 
        match List.length s.vars with
        | 0 -> Failure (IndexOutOfBounds 0) 
        | _ -> Success ((), {s with vars = List.tail s.vars}))

    let wordLength : SM<int> = S (fun s -> Success (List.length s.word, s))

    let characterValue (pos : int) : SM<char> = S (fun s ->
        match List.tryItem pos s.word with
        | Some v -> Success (fst v, s)
        | None -> Failure (IndexOutOfBounds pos))
    
    let pointValue (pos : int) : SM<int> = S (fun s ->
        match List.tryItem pos s.word with
        | Some v -> Success (snd v, s)
        | None -> Failure (IndexOutOfBounds pos))

    let lookup (x : string) : SM<int> =
        let rec aux =
            function
            | [] -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None -> aux ms

        S (fun s -> 
            match aux (s.vars) with
            | Some v -> Success (v, s)
            | None -> Failure (VarNotFound x))

    let update (x : string) (v : int) : SM<unit> =
        let rec aux lst acc =
            match lst with
            | [] -> None
            | m :: ms ->
                match Map.tryFind x m with
                | Some _ -> Some (acc @ (Map.add x v m) :: ms)
                | None -> aux ms (m :: acc)

        S (fun s ->
            match aux s.vars List.empty with
            | Some v -> Success ((), {s with vars = v})
            | None -> Failure (VarNotFound x))

    let declare (x : string) : SM<unit> = 
        S (fun s -> 
        match s.vars  with
            | [] -> Failure EmptyStack
            | m :: ms-> 
                match Map.tryFind x m with
                    | Some _ -> Failure (VarExists x)
                    | None -> 
                        match Set.contains x s.reserved with 
                            | true -> Failure (ReservedName x)
                            | false -> Success ((), {s with vars = (Map.add x 0 m) :: ms})

        )  