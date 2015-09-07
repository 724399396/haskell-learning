--file: ch03/letwhere.hs
bar = let b = 2
          c = true
      in let a = b
         in (a, c)

foor = x
    where x = y
                where y = 2
