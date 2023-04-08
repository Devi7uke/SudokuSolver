namespace Search

module Sudoku =
    type action =
        | Place
    // Sudoku problem data structure type (Part of the definition of the Sudoku proble)
    type state = int list list
    //A function that retrieve the position on the state of a empty cell that can be filled
    let zero state =
        let i = state |> List.findIndex (fun x -> x |> List.exists (fun y -> y = 0))
        let j = List.item i state |> List.findIndex (fun x -> x = 0)
        (i , j)
    //Every single component that must be considered for the sudoku rules evaluation
    let rows state = state
    let cols state = List.transpose state
    let grids state = state |> List.chunkBySize 3 |> List.map (fun chunk -> chunk |> List.transpose |> List.concat) |> List.concat |> List.chunkBySize 9
    //Test meta function that evaluates when there is a node with a final valid state (Part of the definition of the Sudoku problem)
    let goal state =
        let checkSubGrid grid = List.sort grid = [1..9]
        let checkRow row = List.sort row = [1..9]
        let checkCol col = List.sort col = [1..9]
        cols state|> List.forall checkCol && rows state|> List.forall checkRow && grids state|> List.forall checkSubGrid
    //Cost of each action
    let cost _ _ _ = 1.0
    //The definition of the action that return a state for each candidate for an empty cell
    let successor (i, j) action state =
        let cell = state |> List.item i |> List.item j
        let row = rows state |> List.item i 
        let col = cols state |> List.item j
        let sgr = grids state |> List.item (i/3*3 + j/3)
        let guess = [1..9] |> List.filter (fun cand -> not (row |> List.contains cand || col |> List.contains cand || sgr |> List.contains cand))
        let place = guess |> List.map (fun x -> 
            action, state |> List.mapi (fun i' row -> 
                let b = i' = i
                match b with
                | false -> row
                | true -> row |> List.mapi (fun j' cell ->
                    let b' = j' = j
                    match b' with
                    | false -> cell
                    | true -> x
                )
            )
        )
        match action with
        | Place ->  if cell = 0 && guess <> [] then place else []
    //successors fucntion that receive an state and return a list of a successors for the current state (Part of the definition of the Sudoku proble)
    let successors state = 
        let zeroIndex = zero state
        successor zeroIndex Place state
    //Sudoku problem definition (Part of the definition of the Sudoku proble)
    let problem state = {
        start = state
        successors = successors
        goal = goal
        cost = cost
    }
    let heuristicOne node =
        let zeros = node.state |> List.mapi (fun i row ->
            row
            |> List.mapi (fun j x -> (i, j, x))
            |> List.filter (fun (_, _, x) -> x = 0)
            |> List.map (fun (i, j, _) -> (i, j))
        )
        let possibilities = List.concat zeros |> List.sumBy (fun (i, j) ->
            let row = rows node.state |> List.item i 
            let col = cols node.state |> List.item j
            let sgr = grids node.state |> List.item (i/3*3 + j/3)
            let guess = [1..9] |> List.filter (fun cand -> not (row |> List.contains cand || col |> List.contains cand || sgr |> List.contains cand))
            1/(guess.Length + 1) |> float
        )
        possibilities + float node.depth

    let heuristicTwo node =
        let zeros = node.state |> List.sumBy (fun i ->
            i |> List.filter (fun j -> j = 0) |> List.length
        )
        zeros |> float