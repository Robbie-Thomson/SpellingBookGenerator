--SpellingBookGenerator.hs--

speller :: [[Char]] -> [Char]
speller words = foldl (\x y -> 

    --decides if a comma or and "and" should be places between words(if its the last one)
    if y == phrases !! (length phrases - 1)  
        then (x ++ " and " ++ y) 
    else (x ++ ", " ++ y)) 
    (head phrases) (tail phrases)

    -- selects the head of each word and inserts "is for" between the head and the word
    where phrases = map (\w -> (head w) : " is for " ++ w) words
    