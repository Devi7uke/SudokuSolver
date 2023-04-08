namespace Search

module IDSDFS =
    open definitions
    let strategy problem =
        let rec loop l =
            match Chapter3.treeSearch (DFSL.strategy l) problem with
            | Some n -> Some n
            | None -> loop (l + 1)
        loop 0