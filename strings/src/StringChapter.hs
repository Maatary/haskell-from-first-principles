module StringChapter where
  

dropTake :: Int -> Int -> [a] -> [a]
dropTake d t = take t . drop d

takeDrop :: Int -> Int -> [a] -> [a]
takeDrop t d = drop d . take t

