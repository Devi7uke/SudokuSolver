namespace Search

module DFSL =
    let strategy l = {
        empty = Stack.empty
        insert = fun stack n -> 
            if n.depth <= l then
                Stack.push stack n 
            else
                stack
        remove = Stack.pop
    }

    let key n = n.state