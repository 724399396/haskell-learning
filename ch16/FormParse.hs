import Text.ParserCombinators.Parsec
import Numeric
import Control.Monad

p_query :: CharParser () [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'

p_pair :: CharParser () (String, Maybe String)
p_pair = do
  name <- many1 p_char
  value <- optionMaybe (char '=' >> many p_char)
  return (name, value)

p_char :: CharParser () Char
p_char = oneOf urlBaseChars
  <|> (char '+' >> return ' ')
  <|> p_hex

urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

p_hex :: CharParser () Char
p_hex = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a,b]
  return . toEnum $ d
  
p_pair_app1 =
  liftM2 (,) (many1 p_char) (optionMaybe (char '=' >> many p_char))
