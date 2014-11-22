module Main where

import System.Environment
import Text.Parsec
import Control.Arrow (second)
import Text.Read (readMaybe)
import Data.Char (isAscii)

main = do
    progName <- getProgName
    args <- getArgs
    case args of
        [input, output]         -> reformat input output (maxBound :: Int)
        [input, output, number] ->
            case readMaybe number of
                Just n  -> reformat input output (abs n)
                Nothing -> putStrLn $ "Error: '" ++ number ++ "' is not a number!"
        _ -> putStrLn $ "Usage: " ++ progName ++ " inputFile outputFile [maxProblems]"

reformat :: String -> String -> Int -> IO ()
reformat input output number = do
    content <- readFile input
    let asciiOnly = filter isAscii content
    case parse simpleBoards input content of
        Right boards -> writeFile output $ concatMap simpleToPSGO $ take number boards
        Left pError  -> print pError

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
intersections = many (manyTill intersection newline)

beforeBoard :: Parsec String u ()
beforeBoard = do
    let keySequence = newline >> string "\\vbox{\\vbox" >> manyTill anyChar newline
    manyTill anyChar $ try keySequence
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
