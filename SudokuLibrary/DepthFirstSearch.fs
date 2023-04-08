namespace Search

module DFS =
    let strategy = {
        empty = Stack.empty
        insert = Stack.push
        remove = Stack.pop
    }

    let key n = n.state