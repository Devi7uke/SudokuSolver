namespace Search
// A Star is a strategy that works by evaluating the cost of each possible path from the starting point to the target point using the addition of the estimate of the remaining distance and the actual distance traveled
module AAS =
    let strategy h = {
        empty = Map.empty
        insert = fun pqueue x -> Map.add (x.path_cost + h x, x.state) x pqueue
        remove = fun pqueue ->
        match Map.tryFindKey (fun _ _ -> true) pqueue with
        | Some k -> Some (Map.find k pqueue, Map.remove k pqueue)
        | None -> None
    }

    let key h n = n.state, n.path_cost + h n