module Main where

import System.Environment
import Text.Parsec
import Control.Applicative ((<*))

main = putStrLn "Hello world!"

data Intersection = Black | White | Free
    deriving Show

type SimpleBoard = (String, [[Intersection]])

type Position = (Intersection, (Char, Int))
type PositionBoard = [(String, [Position])]

-- *** Parser ***

intersection :: Parsec String u Intersection
intersection = do
    string "\\"
    inter <-
            try (string "- @" >> return Black)
        <|> try (string "- !" >> return White)
        <|>     (string "0??" >> return Free)
    oneOf "<>[](*+"
    return inter

intersections :: Parsec String u [[Intersection]]
intersections = many (many intersection <* newline)

beforeBoard :: Parsec String u ()
beforeBoard = do
    manyTill anyChar $ try $ string "\\vbox{\\vbox" >> manyTill anyChar newline
    return ()

boardName :: Parsec String u String
boardName = do
    char '}' >> newline
    string "\\hfil "
    manyTill anyChar (try $ string "\\hfil\\break" >> newline)


simpleBoard :: Parsec String u SimpleBoard
simpleBoard = do
    beforeBoard
    inters <- intersections
    name <- boardName
    return (name, inters)

simpleBoards :: Parsec String u [SimpleBoard]
simpleBoards = many $ try simpleBoard
