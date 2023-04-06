namespace Search

module GS = 
    let strategy h = {
        empty = Map.empty
        insert = fun pqueue x -> Map.add (h x, x.state) x pqueue
        remove = fun pqueue ->
        match Map.tryFindKey (fun _ _ -> true) pqueue with
        | Some k -> Some (Map.find k pqueue, Map.remove k pqueue)
        | None -> None
    }

    let key h n = n.state, h n