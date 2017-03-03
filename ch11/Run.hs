import QC
import Prettify2
import Test.QuickCheck.Test
import Test.QuickCheck.Random

argsGen = do
  qcGen <- newQCGen
  return Args  {
        replay          = Just(qcGen, 100)
      , maxSuccess      = 200
      , maxDiscardRatio = 10
      , maxSize         = 200
      , chatty = True}
        

main = argsGen >>= (\args ->
  quickCheckWith args prop_empty_id >>
       quickCheckWith args prop_char >>
       quickCheckWith args prop_text >>
       quickCheckWith args prop_line >>
       quickCheckWith args prop_double >>
       quickCheckWith args prop_hcat >>
       quickCheckWith args prop_punctuate' >>
       quickCheckWith args prop_mempty_id)

