namespace Search

//DFS starts at specific vertex in the graph and explores as far as possible along each branch in the graph before backtracking
module DFS =
    let strategy = {
        empty = Stack.empty
        insert = Stack.push
        remove = Stack.pop
    }

    let key n = n.state