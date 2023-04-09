namespace Search

module solution =
    // The function evaluates whether there are any duplicate numbers in elements where duplication is not allowed and whether the given initial state is already a goal state.
    let checkState state =
        let rows = state
        let cols = List.transpose state
        let sgrids = 
            state 
            |> List.chunkBySize 3
            |> List.map (fun chunk -> chunk |> List.transpose |> List.concat)
            |> List.concat
            |> List.chunkBySize 9
        // Function that returns a boolean value indicating if there is at least one duplicate value in a list of lists of integers.
        let duplicates list = 
            list
            |>List.forall (fun x -> 
                x 
                |> List.countBy id |> List.forall (fun y ->
                    match y with
                    | (0, _) -> true
                    | (_, 1) -> true
                    | (_, _) -> false
                )
            )
        // Fucntion that return a boolean value that indicates if the given state is a goal state
        let goal state =
            let checkSubGrid grid = List.sort grid = [1..9]
            let checkRow row = List.sort row = [1..9]
            let checkCol col = List.sort col = [1..9]
            cols|> List.forall checkCol && rows|> List.forall checkRow && sgrids|> List.forall checkSubGrid
        not(goal state) && rows |> duplicates && cols |> duplicates && sgrids |> duplicates
    // The function returns a datatype that contains the solution for the valid given state using the Depth First Search stretegy.
    let outputDFS state = 
        Chapter3.expanded_nodes <- 0;
        let b = checkState state
        match b with
        | true -> 
            let t0 = System.DateTime.UtcNow
            let rec newton_Raphson N K b =
                let f = b ** (K + 1.0) + b * (1.0 - N) - 1.0
                let f' = (K + 1.0) * b ** K
                let b' = b - f / f'
                let aux = abs(b' - b) < 0.00001
                match aux with
                | true -> b'
                | false -> newton_Raphson N K b'
            match Chapter3.treeSearch DFS.strategy (Sudoku.problem state) with
            | Some n ->
                let finalGrid = n.state
                let nodesExpanded = Chapter3.expanded_nodes
                let branchingFactor = newton_Raphson nodesExpanded n.depth 1.0
                let timeElapsed = System.DateTime.UtcNow - t0
                Some {
                    grid = finalGrid
                    nodes = nodesExpanded
                    branching = branchingFactor
                    time = timeElapsed
                }
            | None -> None
        | false -> None
    // The function returns a datatype that contains the solution for the valid given state using the A Star strategy.
    let outputAStar state =
        Chapter3.expanded_nodes <- 0;
        let b = checkState state
        match b with
        | true -> 
            let t0 = System.DateTime.UtcNow
            let rec newton_Raphson N K b =
                let f = b ** (K + 1.0) + b * (1.0 - N) - 1.0
                let f' = (K + 1.0) * b ** K
                let b' = b - f / f'
                let aux = abs(b' - b) < 0.00001
                match aux with
                | true -> b'
                | false -> newton_Raphson N K b'
            match Chapter3.graphSearch (AAS.key Sudoku.heuristicOne) (AAS.strategy Sudoku.heuristicOne) (Sudoku.problem state) with
            | Some n ->
                let finalGrid = n.state
                let nodesExpanded = Chapter3.expanded_nodes
                let branchingFactor = newton_Raphson nodesExpanded n.depth 1.0
                let timeElapsed = System.DateTime.UtcNow - t0
                Some {
                    grid = finalGrid
                    nodes = nodesExpanded
                    branching = branchingFactor
                    time = timeElapsed
                }
            | None -> None
        | false -> None