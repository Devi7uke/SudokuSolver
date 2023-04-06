namespace Search

module BFS =
    open definitions
    let strategy = {
        empty = Queue.empty
        insert = Queue.enqueue
        remove = Queue.dequeue
    }

    let key n = n.state