{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (readFile, unwords)
import Control.Monad (void)
import Data.Char (isAlphaNum)
import Data.Either (rights)
import Data.Maybe (catMaybes)
import Data.Text
import Data.Text.IO (readFile)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = putStrLn "Hello World!"

data RFun = RFun Text CType [RFunParam] deriving Show

data RFunParam = RFunParam Text CType deriving Show

data CType = CType Text deriving Show

type Parser = Parsec Void Text

padded :: Parser a -> Parser a
padded p = space *> p <* space

comment :: Parser Text
comment = strip <$> (chunk "//" *> takeWhileP (Just "any character") (/= '\n'))

names :: Parser [Text]
names = many name

name :: Parser Text
name = (try $ padded $ takeWhile1P (Just "name character") isAlphaNum) <|> (padded $ chunk "*")

splitLast :: a -> [a] -> (a, [a])
splitLast x xs =
    splitLast' (x, xs)
  where
    splitLast' :: (a, [a]) -> (a, [a])
    splitLast' (t, []) = (t, [])
    splitLast' (_, [y]) = (y, [])
    splitLast' (t, (y:ys)) =
      let (t', ys') = splitLast' (t, ys)
       in (t', y : ys')

function :: Parser RFun
function = do
    fnames <- names
    _ <- single '('
    params <- names `sepBy` (single ',')
    _ <- single ')'
    _ <- padded $ single ';'
    _ <- optional comment
    let (fname, ftypes) = splitLast "" fnames
    return $ RFun fname (CType $ unwords ftypes) (catMaybes $ toRFunParam <$> params)
  where
    toRFunParam :: [Text] -> Maybe RFunParam
    toRFunParam xs =
      case splitLast "" xs of
        ("void", [])    -> Nothing
        (pname, ptypes) -> Just $ RFunParam pname $ CType $ unwords ptypes

functions :: Parser [RFun]
functions = rights <$> (many $ eitherP (padded comment) (padded function))
