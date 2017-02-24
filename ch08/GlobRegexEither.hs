-- file: ch08/GlobRegexEither.hs
import Text.Regex.Posix ((=~))
import Control.Monad.Writer

type GlobError = String
globToRegex :: String -> Either GlobError String

globToRegex cs = case globToRegex' cs of
                   Right res -> Right $ '^' : res ++ "$"
                   err@(Left _) -> err

globToRegex' :: String -> Either GlobError String
globToRegex' "" = Right ""

globToRegex' ('*':cs) = eitherConnect ".*" $ globToRegex' cs
globToRegex' ('?':cs) = eitherConnect "." $ globToRegex' cs

globToRegex' ('[':'!':c:cs) = eitherConnect ("[^" ++ [c]) $ charClass cs
globToRegex' ('[':c:cs) = eitherConnect ('[' : [c]) $ charClass cs
globToRegex' ('[':_) = Left "unterminated character class"

globToRegex' (c:cs) = eitherConnect (escape c) (globToRegex' cs)

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"

charClass :: String -> Either GlobError String
charClass (']':cs) = eitherConnect "]" $ globToRegex' cs
charClass (c:cs) = eitherConnect [c] $ charClass cs
charClass [] = Left "unterminated character class"

matchesGlob :: FilePath -> String -> Either GlobError Bool
name `matchesGlob` patEither = case globToRegex patEither of
  Right pat -> Right $ name =~ pat
  Left e -> Left e

eitherConnect :: String -> Either GlobError String -> Either GlobError String
eitherConnect s (Right res) = Right (s ++ res)
eitherConnect _ x = x


