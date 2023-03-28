module hw2.treeMap

module Tree =
    type BinTree<'a> =
        | Tree of 'a * BinTree<'a> * BinTree<'a>
        | Empty

    let map transform tree =
        let rec mapRec tree =
            match tree with
            | Tree (value, l, r) -> Tree((transform value), (mapRec l), (mapRec r))
            | Empty -> Empty

        mapRec tree

    let mapCPS transform tree =
        let rec mapRecCPS tree continuation =
            match tree with
            | Tree (value, leftTree, rightTree) ->
                mapRecCPS leftTree (fun l -> mapRecCPS rightTree (fun r -> continuation (Tree(transform value, l, r))))
            | Empty -> continuation Empty

        mapRecCPS tree id
