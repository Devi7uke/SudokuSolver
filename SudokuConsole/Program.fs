open Search
open System

let testCase1 = [
    [0; 6; 0; 1; 0; 4; 0; 5; 0];
    [0; 0; 8; 3; 0; 5; 6; 0; 0];
    [2; 0; 0; 0; 0; 0; 0; 0; 1];
    [8; 0; 0; 4; 0; 7; 0; 0; 6];
    [0; 0; 6; 0; 0; 0; 3; 0; 0];
    [7; 0; 0; 9; 0; 1; 0; 0; 4];
    [5; 0; 0; 0; 0; 0; 0; 0; 2];
    [0; 0; 7; 2; 0; 6; 9; 0; 0];
    [0; 4; 0; 5; 0; 8; 0; 7; 0]
]

let testCase2 = [
    [0 ;0 ;0 ;0 ;0 ;4 ;9 ;0 ;0];
    [0 ;0 ;5 ;3 ;2 ;0 ;0 ;0 ;0];
    [2; 0; 0; 0; 0; 6; 0; 4; 0];
    [8; 0; 4; 0; 0; 0; 0; 6; 0];
    [0; 5; 0; 0; 6; 0; 0; 1; 0];
    [0; 1; 0; 0; 0; 0; 3; 0; 9];
    [0; 2; 0; 8; 0; 0; 0; 0; 6];
    [0; 0; 0; 0; 7; 9; 1; 0; 0];
    [0; 0; 9; 5; 0; 0; 0; 0; 0]
]

let testCase3 = [
    [0; 0; 9; 0; 2; 8; 0; 0; 0];
    [0; 8; 0; 0; 0; 0; 9; 0; 0];
    [0; 7; 0; 0; 5; 0; 0; 0; 0];
    [0; 3; 8; 9; 0; 0; 1; 0; 5];
    [0; 0; 0; 0; 0; 0; 0; 0; 0];
    [6; 0; 4; 0; 0; 5; 2; 9; 0];
    [0; 0; 0; 0; 4; 0; 0; 6; 0];
    [0; 0; 6; 0; 0; 0; 0; 3; 0];
    [0; 0; 0; 7; 3; 0; 5; 0; 0]
]
//If you want to test the readGrid function, you can use the following valid input string by copying and pasting it.
//Test Case 1   //0 6 0 1 0 4 0 5 0 0 0 8 3 0 5 6 0 0 2 0 0 0 0 0 0 0 1 8 0 0 4 0 7 0 0 6 0 0 6 0 0 0 3 0 0 7 0 0 9 0 1 0 0 4 5 0 0 0 0 0 0 0 2 0 0 7 2 0 6 9 0 0 0 4 0 5 0 8 0 7 0
//Test Case 2   //0 0 0 0 0 4 9 0 0 0 0 5 3 2 0 0 0 0 2 0 0 0 0 6 0 4 0 8 0 4 0 0 0 0 6 0 0 5 0 0 6 0 0 1 0 0 1 0 0 0 0 3 0 9 0 2 0 8 0 0 0 0 6 0 0 0 0 7 9 1 0 0 0 0 9 5 0 0 0 0 0
//Test Case 3   //0 0 9 0 2 8 0 0 0 0 8 0 0 0 0 9 0 0 0 7 0 0 5 0 0 0 0 0 3 8 9 0 0 1 0 5 0 0 0 0 0 0 0 0 0 6 0 4 0 0 5 2 9 0 0 0 0 0 4 0 0 6 0 0 0 6 0 0 0 0 3 0 0 0 0 7 3 0 5 0 0
(*
    This function prompts the user to input a string of 81 elements separated by spaces, 
    each of which represents a value in a 9x9 grid. The function expects each value to be a single-digit integer 
    within the range of 1 to 9. If any value is not within this range, it will be replaced with 0.
*)
let read () =
    printfn "Enter a string of 81 elements separated by space"
    let input = Console.ReadLine();
    (input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries))
    |> List.ofArray
    |> List.map (fun x -> System.Int32.TryParse(x))
    |> List.map (fun (b, x) -> if b = true && x < 10 && x >= 0 then x else 0)

let rec readGrid () =
    let g = read ()
    match List.length g with
    | 81 ->  g |> List.chunkBySize 9
    | _  -> 
        printfn "The given string doesn't match the input rules\n\n"
        readGrid ()

let rec showGrid list = 
    list
    |> List.iter (fun row ->
        row
        |> List.iter (fun col ->
            printf "%A  " col
        )
        printfn ""
    )

let state = readGrid()
printfn "\n<-------SOLVIGN WITH DEPTH FIRST SEARCH------->\n"
//match solution.outputDFS testCase3 with
match solution.outputDFS (state) with
| Some n->
    
    showGrid n.grid
    printfn "Nodes: %A" n.nodes
    printfn "Average Branching Factor: %A" n.branching
    printfn "Time: %A" n.time
| None -> printfn "There is no solution for this imput :("
printfn "\n<-------SOLVIGN WITH A STAR------->\n"
//match solution.outputAStar testCase3 with
match solution.outputAStar (state) with
| Some n->
    showGrid n.grid
    printfn "Nodes: %A" n.nodes
    printfn "Average Branching Factor: %A" n.branching
    printfn "Time: %A" n.time
| None -> printfn "There is no solution for this imput :("