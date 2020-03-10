-- Nedeterministický automat
--
--              [1..n-1], symboly (eg. "ab"), start, finite
-- Number of states
-- Input alphabet
-- Initial state
-- Final states
-- Transformation function
type Automaton = (Int,  String, Int, [Int], [Transition])


type Transition = (Int, Char, [Int])

-- Current state
-- Character
-- Transformation function
-- Possible new states
transformations :: Int -> Char -> [Transition] -> [Int]
transformations current char fun = foldl (++) [] newStates
    where
        filtered = filter filterFun fun
            where filterFun (state, ch, _) = current == state && char == ch 
        newStates = map extractFun filtered
            where extractFun (_, _, lst) = lst

-- Vezme automat, vstupní slovo, vrátí True/False podle toho, zda automat slovo přijme
check :: Automaton -> String -> Bool
check (_, _, initial, final, _) "" = initial `elem` final
check automaton word =
    let
        (states, alphabet, initial, final, transitions) = automaton
        newStates = transformations initial (head word) transitions
        newAutomata = map (\state -> (states, alphabet, state, final, transitions)) newStates
        newWord = tail word
        result = any (\aut -> check aut newWord) newAutomata
    in
        result

binaryCheck = check binaryAutomaton
    where 
        binaryTransitions = [ (2, '1', [1]), (0, '1', [1]), (1, '1', [1]), (0, '0', [0]), (1, '0', [0]) ]
        binaryAutomaton = (3, "01", 2, [0, 1], binaryTransitions)

abaCheck = check abaAutomaton
    where
        abaTransitions = [ (0, 'a', [1]), (1, 'a', [1]), (1, 'b', [2]), (2, 'b', [4]), (2, 'a', [3]), (3, 'a', [3]) ] 
        abaAutomaton = (4, "ab", 0, [3], abaTransitions)


kotlinCheck = check kotlinAutomaton
    where
        kotlinTransitions = [ (state, char, [0]) | state <- [0..5], char <- (' ' : ['a'..'z']) ] ++ 
                            [ (0, 'k', [1]),  (1, 'o', [2]), (2, 't', [3]), (3, 'l', [4]), (4, 'i', [5]), (5, 'n', [6]) ] ++
                            [ (6, char, [6]) | char <- (' ' : ['a'..'z']) ]
        kotlinAutomaton = (6, ' ' : ['a'..'z'], 0, [6], kotlinTransitions)


