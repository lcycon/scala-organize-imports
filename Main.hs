-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import Data.Char (isUpper, isLower)
import Data.List
import Data.Ord
import Data.Text (pack)

data Import = Single [String] String
            | Multi [String] [String]
            | Wild [String]
  deriving (Show, Eq)

stem             :: Import -> [String]
stem (Single s _)  = s
stem (Multi s _) = s
stem (Wild s)    = s

prettyPrint :: Import -> String
prettyPrint (Single stem ident) = s ++ "." ++ ident
  where s = foldl1' (\a b -> a ++ "." ++ b) stem
prettyPrint (Wild stem) = s ++ "._"
  where s = foldl1' (\a b -> a ++ "." ++ b) stem
prettyPrint (Multi stem idents) = s ++ ".{" ++ ids ++ "}"
  where s   = foldl1' (\a b -> a ++ "." ++ b) stem
        ids = foldl1' (\a b -> a ++ ", " ++ b) idents

prettyPrintLine :: Import -> String
prettyPrintLine i = "import " ++ prettyPrint i

compareBreakTies :: Import -> Import -> Ordering
compareBreakTies (Single _ a) (Single _ b) = compareStrings a b
compareBreakTies (Multi _ a) (Multi _ b) = compareStems a b
compareBreakTies (Single _ _) (Multi _ _) = LT
compareBreakTies (Multi _ _) (Single _ _) = GT
compareBreakTies (Wild _) _ = LT
compareBreakTies _ (Wild _) = GT

compareChar :: Char -> Char -> Ordering
compareChar a b
  | isLower a && isUpper b = LT
  | isUpper a && isLower b = GT
  | otherwise              = compare a b

compareStrings :: String -> String -> Ordering
compareStrings [] [] = EQ
compareStrings [] _  = LT
compareStrings _  [] = GT
compareStrings (x:xs) (y:ys) = case compareChar x y of
                                 EQ -> compareStrings xs ys
                                 o  -> o

compareStems :: [String] -> [String] -> Ordering
compareStems [] [] = EQ
compareStems [] _  = LT
compareStems _  [] = GT
compareStems (x:xs) (y:ys) = case compareStrings x y of
                                    EQ -> compareStems xs ys
                                    o  -> o

instance Ord Import where
  compare a b = case compareStems (stem a) (stem b) of
                  EQ -> compareBreakTies a b
                  r  -> r

main = do fileData <- getContents
          let results = parseOnly (many1 lineP) (pack fileData)
          case results of
            Left _ -> putStrLn "FAILURE"
            Right imports -> mapM_ (putStrLn . prettyPrintLine) ((sort . concat) imports)

lineP :: Parser [Import]
lineP = do string "import"
           many1 $ char ' '
           imp <- try manyP <|> importList
           many' $ char ' '
           many1 endOfLine
           return imp
  where importList = do t <- basicImportP
                        return [t]

validIdentChar :: Char -> Bool
validIdentChar c = c /= '.' && c /= '{' && c /= '}' && c /= ',' && c /= '\n'

manyP :: Parser [Import]
manyP = basicImportP `sepBy1` (char ',' >> many' (char ' '))

basicImportP :: Parser Import
basicImportP = try singleP <|> try wildCardP <|> multiP

identP :: Parser String
identP = many1 $ satisfy validIdentChar

stemP :: Parser [String]
stemP = many1 $ identP <* char '.'

wildCardP :: Parser Import
wildCardP = Wild <$> (stemP <* char '_')

singleP :: Parser Import
singleP = Single <$> stemP <*> identP

multiP :: Parser Import
multiP = Multi <$> stemP <*> multiIdentsP
  where multiIdentsP = do char '{'
                          ids <- identP `sepBy1` (char ',' >> many' (char ' '))
                          char '}'
                          return $ sort ids