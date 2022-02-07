module LearningBasicDataTypes where
  

-- Intro
type Name = String
data Pet = Cat | Dog Name deriving Show

-- Playing around
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

-- Basic Data Types definition  
data Mood = Blah | Woot deriving Show

-- Pattern Matching example
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood  _   = Blah



x = Node 2 (Node 3 Leaf Leaf) Leaf
