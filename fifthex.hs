firstWord input = case words input of
    [] -> ""  -- Handle the case where there are no words in the input string
    (word:_) -> word