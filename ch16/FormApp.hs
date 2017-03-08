import Numeric (readHex)
import Control.Applicative hiding ((<|>),many)
import Control.Monad
import Text.ParserCombinators.Parsec

p_hex :: CharParser () Char
p_hex = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a,b]
  return . toEnum $ d

a_hex = hexify <$> (char '%' *> hexDigit) <*> hexDigit
  where hexify a b = toEnum . fst . head . readHex $ [a,b]

p_char :: CharParser () Char
p_char = oneOf urlBaseChars
  <|> (char '+' >> return ' ')
  <|> p_hex

urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

a_char :: CharParser () Char
a_char = oneOf urlBaseChars
  <|> (' ' <$ char '+')
  <|> a_hex

p_pair_app1 =
  liftM2 (,) (many1 p_char) (optionMaybe (char '=' >> many p_char))

a_pair :: CharParser () (String, Maybe String)
a_pair = liftA2 (,) (many1 a_char) (optionMaybe (char '=' *> many a_char))
