module MoodSwings where
    data Mood = Woot | Blah deriving Show
    changeMood Woot = Blah
    changeMood Blah = Woot
