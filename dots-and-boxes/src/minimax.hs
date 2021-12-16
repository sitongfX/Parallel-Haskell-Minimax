minimax player (edgeset,boxlist,AIscore) edge
    | terminal                         = (AIscore, edge)
    | player == False                  = bestMove [minimax True x e | (x, e) <- expandedStates]   
    | player == True                   = worstMove [minimax False x e | (x, e) <- expandedStates]  
    where
        expandedStates = [ (gameAction e (edgeset,boxlist,AIscore) , e) | e <- edgeset]
        terminal = Set.empty edgeset


bestMove [(score, edge)]       = (score, edge)
bestMove ((score, edge):(score', edge'):xs) = bestMove (if score >= score' then (score, edge) else (score', edge'):xs)

worstMove [(score, edge)]       = (score, edge)
worstMove ((score, edge):(score', edge'):xs) = worstMove (if score <= score' then (score, edge) else (score', edge'):xs)

