import qualified Data.Set as S

type SplitPattern = (String, Char, String)
splits :: String -> [SplitPattern]
splits "" = []
splits (w:ord) = thisSplit:otherSplits
    where thisSplit = ("", w, ord)
          otherSplits = map (prependToFirst w) (splits ord)
          prependToFirst w (pre, ch, post)
            = (w:pre, ch, post)

deletion :: SplitPattern -> String
deletion (pre, _, post) = pre ++ post

changes :: SplitPattern -> [String]
changes (pre, _, post) = map (combine pre post) ['a'..'z']
    where combine pre post ch = pre ++ (ch:post)

insertionsAtStart :: String -> [String]
insertionsAtStart word = map (flip (:) word) ['a'..'z']

insertions :: SplitPattern -> [String]
insertions ("", ch, post) = insertionsAtStart (ch:post) ++
                        map (ch:) (insertionsAtStart post)
insertions (pre, ch, post) = map (pre ++) $ map (ch:) (insertionsAtStart post)

variations :: String -> [String]
variations s = concat (map splitvariations (splits s))
    where splitvariations pat = insertions pat ++ (deletion pat):(changes pat)

type WordList = S.Set String

isWord :: WordList -> String -> Bool
isWord wordList = flip S.member wordList

wordVariations :: WordList -> String -> [String]
wordVariations wordList word = filter (isWord wordList) (variations word)

network :: WordList -> String -> [String]
network wordList word = concat $ map (network wordList) (wordVariations wordList word)

buildWordList :: String -> WordList
buildWordList input = S.fromList (lines input)

answer :: String -> String
answer input = unlines $ network wordList "causes"
    where wordList = buildWordList input

main = do
    wordsStr <- getContents
    let wordList = buildWordList wordsStr
    putStr $ unlines $ network wordList "causes"
