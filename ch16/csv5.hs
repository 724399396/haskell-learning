import Text.ParserCombinators.Parsec
import Text.Parsec.Prim

eol :: Text.Parsec.Prim.Stream s m Char =>
     Text.Parsec.Prim.ParsecT s u m Char
eol =
    do char '\n'
       char '\r' <|> return '\n'
