namespace Search

type stack<'a> = list<'a>

module Stack = 
    let empty = []
    let push stack x = x :: stack
    let pop stack =
        match stack with
        | h :: t -> Some(h ,t)
        | [] -> None