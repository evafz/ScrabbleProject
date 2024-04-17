module Dictionary
    type Dict =
        | Leaf of bool 
        | Node of bool * Map<char, Dict>

    let empty (_:unit) = Leaf false
    
    let rec insert s dict =
        match dict with 
        | Leaf _ when String.length s = 0 -> Leaf true
        | Leaf b -> Node (b, Map.ofList [s[0], insert s[1..] (empty ())])
        | Node (_, dict) when String.length s = 0 -> Node (true, dict) 
        | Node (b, dict) ->
            let c = s[0]
            match Map.containsKey c dict with
            | true -> Node (b, Map.add c (insert s[1..] dict[c]) dict)
            | _ -> Node (b, Map.add c (insert s[1..] (empty ())) dict)

    let rec lookup s dict = 
        match dict with
        | Leaf b when String.length s = 0 -> b
        | Leaf _ -> false
        | Node (b, _) when String.length s = 0 -> b
        | Node (_, dict) -> 
            let c = s[0]
            match Map.containsKey c dict with
            | true -> lookup s[1..] dict[c]
            | _ -> false
    
    let step c dict =
        match dict with
        | Leaf _ -> None
        | Node (_, dict) ->
            match Map.containsKey c dict with
            | false -> None
            | _ ->
                let element = dict[c]
                match element with
                | Leaf b -> Some (b, element)
                | Node (b, _) -> Some (b, element)        
