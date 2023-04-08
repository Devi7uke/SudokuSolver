namespace Search

module solution =
    let checkState state =
        let rows = state
        let cols = List.transpose state
        let sgrids = 
            state 
            |> List.chunkBySize 3
            |> List.map (fun chunk -> chunk |> List.transpose |> List.concat)
            |> List.concat
            |> List.chunkBySize 9
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
        let goal state =
            let checkSubGrid grid = List.sort grid = [1..9]
            let checkRow row = List.sort row = [1..9]
            let checkCol col = List.sort col = [1..9]
            cols|> List.forall checkCol && rows|> List.forall checkRow && sgrids|> List.forall checkSubGrid
        not(goal state) && rows |> duplicates && cols |> duplicates && sgrids |> duplicates
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
            match Chapter3.graphSearch (AAS.key Sudoku.heuristicTwo) (AAS.strategy Sudoku.heuristicTwo) (Sudoku.problem state) with
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