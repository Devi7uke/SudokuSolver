namespace Search

type queue<'a> = list<'a>

module Queue = 
    let empty = []
    let enqueue queue x = queue @ [x]
    let dequeue queue =
        match queue with
        | h :: t -> Some(h ,t)
        | [] -> None