import Data.List
import Data.List.Split
import System.IO
import Control.Monad

hasList([], _) = False
hasList(h:t, l) = if l == h then True else hasList(t, l)

data Graph = Graph {vertices :: [(Integer, [Integer])]}

insertVertices(0) = []
insertVertices(iter) = (iter, []):insertVertices(iter - 1)

insertEdges([], vertices) = vertices
insertEdges(from:to:rest, vertices) = insertEdges(rest, newVertices) 
    where newVertices = map (\(x, y) -> if x == from then (x, to:y) else (x, y)) vertices 

buildGraph(root, vertices) = Graph(insertEdges(vertices, insertVertices(root)))

makeStep(_, []) = []
makeStep(graph, v:rest) =  
    case lookup v (vertices(graph)) of 
        Just vToList -> vToList ++ makeStep(graph, rest)
        Nothing -> makeStep(graph, rest)

makeMove(_, 0, vertices) = vertices
makeMove(graph, diceValue, vertices) =
    makeMove(graph, diceValue - 1, foldl (\l e -> if elem e l then l else e:l) [] (makeStep(graph, vertices)))

diceHelper(finalNode, graph, depth, [], diceList, verticesList, borderLists) = 
    if hasList(borderLists, sort(verticesList))
        then -1 
        else diceHelper(finalNode, graph, depth, diceList, diceList, verticesList, sort(verticesList):borderLists)
diceHelper(finalNode, graph, depth, diceValue:rest, diceList, verticesList, borderLists) = 
    if elem finalNode verticesList
        then depth
        else diceHelper(finalNode, graph, depth + 1, rest, diceList, makeMove(graph, diceValue, verticesList), borderLists)

dice(finalNode, verticesList, diceList) = diceHelper(finalNode, buildGraph(finalNode, verticesList), 0, diceList, diceList, [1], [])

main::IO()
main = do
    hSetBuffering stdin LineBuffering
    s <- getLine
    let tests = read s :: Integer
    forM_ [1..tests] (\i -> do
        args <- getLine
        let graphData = map (\x -> read x :: Integer) (splitOn " " args)
        args <- getLine
        let edges = map (\x -> read x :: Integer) (splitOn " " args)
        args <- getLine
        let dices = map (\x -> read x :: Integer) (splitOn " " args)
        let res = dice(head(graphData), edges, dices)
        print res) 