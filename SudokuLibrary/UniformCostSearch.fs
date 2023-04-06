namespace Search

module UCS =
    let strategy = {
        empty = Map.empty
        insert = fun pqueue x -> Map.add (x.path_cost, x.state) x pqueue
        remove = fun pqueue ->
            match Map.tryFindKey (fun _ _ -> true) pqueue with
            | Some k -> Some (Map.find k pqueue, Map.remove k pqueue)
            | None -> None
    }

module UCSL =
    let strategy l = {
        empty = Map.empty
        insert = fun pqueue x -> 
            if x.depth <= l then
                Map.add (x.path_cost, x.state) x pqueue
            else
                pqueue
        remove = fun pqueue ->
            match Map.tryFindKey (fun _ _ -> true) pqueue with
            | Some k -> Some (Map.find k pqueue, Map.remove k pqueue)
            | None -> None
    }