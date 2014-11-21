module Main where

import System.Environment
import Text.Parsec
import Control.Applicative ((<*))
import Control.Arrow (second)

main = putStrLn "Hello world!"

data Intersection = Black | White | Free
    deriving (Eq, Show)

type SimpleBoard = (String, [[Intersection]])

type Position = (Intersection, (Char, Int))
type PositionalBoard = (String, [Position])

-- *** Formatter ***

addCoordinates :: [[Intersection]] -> [Position]
addCoordinates inters = removeFrees $ concat zipped
    where coordinates = [[(c, r) | c <- ['a'..'h'] ++ ['j'..'t']] | r <- [19, 18 .. 1]]
          zipped      = zipWith zip inters coordinates
          removeFrees = filter ((/= Free) . fst)

simpleToPositional :: SimpleBoard -> PositionalBoard
simpleToPositional = second addCoordinates

positionalToPSGO :: PositionalBoard -> String
positionalToPSGO (name, positions) = header ++ concatMap format positions ++ footer
    where header = "\\goproblem{" ++ name ++ "}{%\n" ++
                   "\t\\begin{psgopartialboard*}{(1," ++ show size ++ ")(19,19)}\n"
          size   = subtract 1 $ minimum $ map (snd . snd) positions
          footer = "\t\\end{psgopartialboard*}\n" ++
                   "}\n\n"
          format (Black ,(c, r)) = "\t\\stone{black}{" ++ [c] ++ "}{" ++ show r ++ "}\n"
          format (White ,(c, r)) = "\t\\stone{white}{" ++ [c] ++ "}{" ++ show r ++ "}\n"
          format _               = ""

simpleToPSGO :: SimpleBoard -> String
simpleToPSGO = positionalToPSGO . simpleToPositional

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
