namespace Search

module Sudoku =
    type action =
        | Place
    // Sudoku problem data structure type (Part of the definition of the Sudoku proble).
    type state = int list list
    //A function that retrieve the position on the state of a empty cell that can be filled.
    (*  
    The function searches the given grid (State) to find the row (i) that contains a 0 in its members. 
    It then searches within that row to find the column (j) that contains a 0. 
    Once both indices have been found, the function returns them as a tuple of integers.
    *)
    let zero state =
        let i = state |> List.findIndex (fun x -> x |> List.exists (fun y -> y = 0))
        let j = List.item i state |> List.findIndex (fun x -> x = 0)
        (i , j)
    //Every single component that must be considered for the sudoku rules evaluation.
    //Function that returns a copy of the original grid (state) to simplify comprehension in the code below.
    let rows state = state
    //Function that returns the transposed grid (state) in order to represent the columns in the grid.
    let cols state = List.transpose state
    //Function that return the sungrids.
    (*  
    The function takes a gird (state) and transforms it into a list of lists, where each sub-list represents a subgrid in the Sudoku puzzle.
    1. Firstable, the function slices the list into groups of 3 rows, resulting in a list of sub-lists, where each sub-list contains 3 subgrids.
    2. Next, for each sublist, the function applies a transpose operation, which rearranges the elements in a way that each list contains the 3 
       elements in the columns of the 3 rows, in other words, each 3 list of 3 elements will be a subgrid.
    3. Then, the function will concatenate 2 times for erase the separation resulting in a list of 81 integers.
    4. Finally, the function slices the list in a pieces of 9 elemets, in wich every sub-list will represent a subgrid.
    *)
    let grids state = state |> List.chunkBySize 3 |> List.map (fun chunk -> chunk |> List.transpose |> List.concat) |> List.concat |> List.chunkBySize 9
    //Test meta function that evaluates when there is a node with a final valid state (Part of the definition of the Sudoku problem).
    let goal state =
        let checkSubGrid grid = List.sort grid = [1..9]
        let checkRow row = List.sort row = [1..9]
        let checkCol col = List.sort col = [1..9]
        cols state|> List.forall checkCol && rows state|> List.forall checkRow && grids state|> List.forall checkSubGrid
    //Cost of each action.
    let cost _ _ _ = 1.0
    //The definition of the action that return a state for each candidate for an empty cell.
    let successor (i, j) action state =
        //In this function, we need to identify the elements that constrain the candidate values that can be placed in the given empty cell.
        let cell = state |> List.item i |> List.item j
        let row = rows state |> List.item i 
        let col = cols state |> List.item j
        let sgr = grids state |> List.item (i/3*3 + j/3)
        //Making use of the elemets identified above, the function generates the candidate values for the given emtpy cell.
        let guess = [1..9] |> List.filter (fun cand -> not (row |> List.contains cand || col |> List.contains cand || sgr |> List.contains cand))
        //Function that genereates all the possible grids (states (succesors)).
        (*
        The function makes use of each candidate returned in the guess function to generate a list of grids, in which each grid is a branch of the current state.
        1. The functions applies a funciton for each element in the candidates combining the result into a pair of (action, state).
        2. Then, mapi applies a function for the current grid, that if the index (i -> row) passed corresponds to the index of the empty cell, it applies 
           the function, otherwise, returns the unaffected row.
        3. In the following mapi, the function checks whether the column (j -> column) corresponds to the column of the empty cell,
           if it matches to the column, it will return the given candidate, otherwise it returns the unaffected cell. 
        *)
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
        //Here is what is the condition to generate succesors
        match action with
        | Place ->  if cell = 0 && guess <> [] then place else []
    //successors fucntion that receive an state and return a list of a successors for the current state (Part of the definition of the Sudoku proble)
    let successors state = 
        let zeroIndex = zero state
        successor zeroIndex Place state
    //Sudoku problem definition (Part of the definition of the Sudoku problem)
    let problem state = {
        start = state
        successors = successors
        goal = goal
        cost = cost
    }
    //Heristic that return the number of empty cells
    let heuristicOne node =
        let zeros = node.state |> List.sumBy (fun i ->
            i |> List.filter (fun j -> j = 0) |> List.length
        )
        zeros |> float