import System.IO
import System.Environment
import Data.List
import Data.Char
import qualified Data.Map as Map

main = do
  -- Read the args list
  files <- getArgs
  freqs <- case files of
    -- If no args, read from stdin
    [] -> do
      contents <- getContents
      return $ getFreqs contents
    -- Otherwise, read the files
    fs -> do
      contents <- mapM readFile fs
      let freqList = map getFreqs contents
      -- Sum the frequencies for each file
      return (foldl (Map.unionWith (+)) Map.empty freqList)

  -- Compute the output and print
  putStr $ unlines $ freqsString $ sortFreqs freqs

-- Compute the histogram for a string
getFreqs :: String -> Map.Map String Int
getFreqs str = 
  let
    -- Split the file up into a list of words
    wordList = words (map toLower str)
    -- Strip punctuation.  Inspired by the Haskell code in the Wikipedia entry for "Trim"
    dropPunct = reverse . dropWhile isPunctuation
    stripPunct = dropPunct . dropPunct
    -- Define a funtion to update the frequency map if necessary
    updateEntry m word = if null stripped then m else Map.insertWith (+) stripped 1 m
      where stripped = stripPunct word
  in
    -- Add up the frequencies
    foldl updateEntry Map.empty wordList

-- Sort the elements of the map backwards by frequency, then lexicographically, and return (word, frequency pairs)
sortFreqs :: Map.Map String Int -> [(String, Int)]
sortFreqs m = sortBy freqThenLex (Map.toList m)
  where
    freqThenLex (w1,f1) (w2,f2) = if f1 == f2 then compare w1 w2 else compare f2 f1

-- Generate the output lines for the (word, frequency) pairs
freqsString :: [(String, Int)] -> [String]
freqsString freqs =
  let
    -- Compute the max word length and max frequency
    maxLen = maximum (map (length . fst) freqs)
    maxFreq = maximum (map snd freqs)
    -- Compute the scaling of the bars.  Don't scale up, only down to fit the 80 column max
    ratio =
      if maxLen + maxFreq + 1 > 80 then
        fromIntegral (80 - maxLen - 1) / fromIntegral maxFreq
      else 1
    -- Functions to find the sizes
    numSpaces word = maxLen - length word + 1
    numHashes freq = round (ratio * fromIntegral freq)
    -- Filter out words with small frequencies
    filteredFreqs = filter ((0 /=) . numHashes . snd) freqs
  in
    -- Generate the final line: the word, then the correct number of spaces, then the correct number of hashes
    map (\(w,l) -> (w ++ replicate (numSpaces w) ' ' ++ replicate (numHashes l) '#')) filteredFreqs
