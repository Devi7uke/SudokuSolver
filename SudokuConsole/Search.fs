namespace Search
//Datatype that describes the characteristics of a problem that can be used with the Search Algorithms
type problem<'s, 'a> = {
    start       :   's
    successors  :   's -> list<'a * 's>
    goal        :   's -> bool
    cost        :   's -> 'a -> 's -> float
}
//Datatype that describes the content of a vertex in the graph
type node<'s, 'a> = {
    depth       :   int
    path_cost   :   float
    state       :   's
    action      :   option<'a>
    parent      :   option<node<'s, 'a>>
}
//Datatype that describes the content of a strategy like BFS or DFS
type strategy<'s, 'a, 'd> = {
    empty       :   'd
    insert      :   'd -> node<'s, 'a> -> 'd
    remove      :   'd -> option<node<'s, 'a> * 'd>
}

(*
Datatype that contains the solution for a sudoku problem
The output of the function includes the final valid solution for the given grid, the number of nodes expanded during the search, 
the approximate average branching factor, and the time it takes for the algorithm to solve the problem.
*)
type solution<'s> = {
    grid        :   's
    nodes       :   float
    branching   :   float
    time        :   System.TimeSpan
}

module Chapter3 =
    //A variable that stores the nodes expanded during the execution.
    let mutable expanded_nodes = 0.0

    let initialNode state = {
        state = state
        depth = 0
        path_cost = 0.0
        action = None
        parent = None
    }
    //Fucntion that retrive the succesors of the current state and generates the nodes for solve the problem
    let expand problem parent = 
        expanded_nodes <- expanded_nodes + float (problem.successors parent.state).Length
        problem.successors parent.state
        |> List.map (fun (a, s) -> {
            depth = parent.depth + 1
            state = s
            action = Some a
            parent = Some parent
            path_cost = parent.path_cost + problem.cost parent.state a s
        })

    //Algorithm that recives a problem and a strategy to genereate a graph data structure to explore it based on the strategy til a solution is found
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
    //A variant of the algorithm described above, it includes a memory that disallow the algorithm from regenerating already visited nodes.
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
    //Function that returns the steps followed to reach the solution.
    let rec actions n =
        match n.action, n.parent with
        | Some a, Some p -> actions p @ [a]
        | _ -> []