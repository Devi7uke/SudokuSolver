namespace Search

module IDSDFS =
    let strategy problem =
        let rec loop l =
            match Chapter3.treeSearch (DFSL.strategy l) problem with
            | Some n -> Some n
            | None -> loop (l + 1)
        loop 0

module IDSUCS =
    let strategy problem =
        let rec loop l =
            match Chapter3.treeSearch (UCSL.strategy l) problem with
            | Some n -> Some n
            | None -> loop (l + 1)
        loop 0