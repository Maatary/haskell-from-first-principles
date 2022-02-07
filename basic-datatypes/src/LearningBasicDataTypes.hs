module LearningBasicDataTypes where
  

-- Intro
type Name = String
data Pet = Cat | Dog Name deriving Show


-- Basic Data Types definition  
data Mood = Blah | Woot deriving Show

-- Pattern Matching example
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood  _   = Blah




