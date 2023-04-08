namespace Search
    
type problem<'s, 'a> = {
    start       :   's
    successors  :   's -> list<'a * 's>
    goal        :   's -> bool
    cost        :   's -> 'a -> 's -> float
}

type node<'s, 'a> = {
    depth       :   int
    path_cost   :   float
    state       :   's
    action      :   option<'a>
    parent      :   option<node<'s, 'a>>
}

type strategy<'s, 'a, 'd> = {
    empty       :   'd
    insert      :   'd -> node<'s, 'a> -> 'd
    remove      :   'd -> option<node<'s, 'a> * 'd>
}

type solution<'s> = {
    grid        :   's
    nodes       :   float
    branching   :   float
    time        :   System.TimeSpan
}

module Chapter3 =
    let mutable expanded_nodes = 0.0

    let initialNode state = {
        state = state
        depth = 0
        path_cost = 0.0
        action = None
        parent = None
    }
    
    let expand problem parent = 
        expanded_nodes <- expanded_nodes + 1.0
        problem.successors parent.state
        |> List.map (fun (a, s) -> {
            depth = parent.depth + 1
            state = s
            action = Some a
            parent = Some parent
            path_cost = parent.path_cost + problem.cost parent.state a s
        })

    let treeSearch strategy problem =
        let root = initialNode problem.start
        let fringe = strategy.insert strategy.empty root
        let rec loop fringe =
            match strategy.remove fringe with
            | Some (n, fringe') ->
                if problem.goal n.state then
                    Some n
                else 
                    expand problem n
                    |> List.fold strategy.insert fringe'
                    |> loop
            | None -> None
        loop fringe

    let graphSearch key strategy problem =
        let root = initialNode problem.start
        let fringe = strategy.insert strategy.empty root
        let rec loop (fringe, processed) =
            match strategy.remove fringe with
            | Some (n, fringe') -> 
                if problem.goal n.state then
                    Some n 
                else
                    if Set.contains (key n) processed then 
                        loop (fringe', processed) 
                    else
                        expand problem n
                        |> List.fold strategy.insert fringe'
                        |> (fun fringe -> loop (fringe, Set.add (key n) processed))
            | None -> None
        loop (fringe, Set.empty)

    let rec actions n =
        match n.action, n.parent with
        | Some a, Some p -> actions p @ [a]
        | _ -> []

module Chapter4 =
    open Chapter3
    open System
    let hillClimbing h problem =
        let current = initialNode problem.start
        let rec loop current =
            let successors = expand problem current
            let neighbor = List.minBy h successors
            let b = h neighbor >= h current
            match b with
            | true -> current
            | false -> loop neighbor
        loop current

    let temperature T0 lamda t =
        let T = T0 * Math.Exp(-lamda * t)
        let b = T < 1E-6
        match b with
        | true -> 0.0
        | false -> T

    let simulatedAnnealing seed h temperature problem =
        let random = System.Random(seed)
        let current = initialNode problem.start
        let rec loop (t, current) =
            let T = temperature t
            let b = T = 0.0
            match b with
            | true -> current
            | false -> 
                let successors = expand problem current
                let i = random.Next(List.length successors)
                let next = List.item i successors
                let delta = h next - h current
                let b' = delta < 0.0 || random.NextDouble() <= Math.Exp (delta / T)
                match b' with
                | true -> loop (t + 1.0, next)
                | false -> loop (t + 1.0, current)
        loop (1.0, current)