module internal MultiSet
    type MultiSet<'a when 'a : comparison> = S of uint32 * Map<'a, uint32>

    let empty = S (0u, Map.empty)
    let isEmpty (S (_, table)) = Map.isEmpty table
    let size (S (size, _)) = size
    let contains a (S (_, table)) = Map.containsKey a table
    let numItems a (S (_, table)) = Map.tryFind a table |> Option.defaultValue 0u
    let add a n (S (size, table)) = S (size + n, Map.add a ((numItems a (S (size, table))) + n) table)
    let addSingle a s = add a 1u s
   
    let remove a n (S (size, table)) =
        let count = numItems a (S (size, table))
        match n >= count with
        | true -> S (size - count, Map.remove a table)
        | _ -> S (size - n, Map.add a (count - n) table)

    let removeSingle a s = remove a 1u s
    let fold f acc (S (_, table)) = Map.fold f acc table
    let foldBack f (S (_, table)) acc = Map.foldBack f table acc
    let ofList lst = List.fold (fun accumulator element -> addSingle element accumulator) empty lst
    let toList s = fold (fun accumulator element count -> accumulator @ List.init (int count) (fun _ -> element)) [] s
   
    let map f s =
        s 
        |> toList
        |> List.map f
        |> ofList

    let union s1 s2 = fold (fun accumulator element count1 ->
        let count2 = numItems element s2
        match count1 > count2 with
        | true -> add element count1 accumulator 
        | _ -> add element count2 accumulator) empty s1
        
    let sum s1 s2 = fold (fun accumulator element count -> add element count accumulator) s2 s1
    let subtract s1 s2 = fold (fun accumulator element count -> remove element count accumulator) s1 s2
    
    let intersection s1 s2 = fold (fun accumulator element count1 ->
        let count2 = numItems element s2
        match count1 > 0u && count2 > 0u with
        | false -> accumulator
        | _ ->
            match count1 > count2 with
            | false -> add element count1 accumulator
            | _ -> add element count2 accumulator) empty s1
