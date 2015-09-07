--file: ch03/Indentation.hs
foo = let firstDefinition = blah blah
        -- a comment-only line is treated as empty
                             continutation blah blah
        -- we reduce the indentation, so this is a new definition
           secondDefinition = yada yada
                            
                              continutation yada
    in whatever
