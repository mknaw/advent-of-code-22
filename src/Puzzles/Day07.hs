module Puzzles.Day07
  ( day07aSolve,
    day07bSolve,
  )
where

import qualified Data.Either as E
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as M
import qualified Data.Text as T
import Lib.Parse
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char

-- TODO maybe it works but this is pretty nasty,
-- should refactor or come up with some smoother ways.

day07aSolve :: PuzzleSolve [InputLine] Int
day07aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day07a
    }

day07bSolve :: PuzzleSolve [InputLine] Int
day07bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day07b
    }

data CD = CDRoot | CDParent | CD String

data LS = LS

type Command = Either CD LS

newtype Dir = Dir String deriving (Show)

data File = File Int String deriving (Show)

type Entry = Either Dir File

type InputLine = Either Command Entry

parse' :: T.Text -> [InputLine]
parse' = parseInput $ ((Left <$> parseCommand) <|> (Right <$> parseEntry)) `sepEndBy` eol
  where
    parseCommand :: Parser Command
    parseCommand = do
      string "$ "
      Left <$> parseCd <|> Right LS <$ string "ls"
      where
        parseCd :: Parser CD
        parseCd = do
          string "cd "
          choice
            [ CDParent <$ string "..",
              CDRoot <$ string "/",
              CD <$> some alphaNumChar
            ]

    parseEntry :: Parser Entry
    parseEntry = parseDir <|> parseFile
      where
        parseDir :: Parser Entry
        parseDir = Left . Dir <$> (string "dir " *> some alphaNumChar)

        parseFile :: Parser Entry
        parseFile = do
          sz <- parseInt
          space
          name <- some (alphaNumChar <|> char '.')
          return . Right $ File sz name

data FileTree = F Int | D (M.Map String FileTree) deriving (Show)

newFileTree :: FileTree
newFileTree = D M.empty

type FileTreeCursor = (FileTree, [(String, FileTree)])

cd :: FileTreeCursor -> CD -> FileTreeCursor
cd (F _, _) _ = error "Cannot cd into a file"
cd (root, []) CDRoot = (root, [])
cd cur CDRoot = cd (cd cur CDParent) CDRoot
cd (_, []) CDParent = error "Cannot cd into parent of root"
cd (ft, crumbs) CDParent = (ft', tail crumbs)
  where
    (prevDir, prevTree) = head crumbs
    ft' = case prevTree of
      D dir -> D $ M.insert prevDir ft dir
      _ -> error "Cannot cd into parent of a file"
cd (D dir, crumbs) (CD dirName) = (nextDir, (dirName, D dir') : crumbs)
  where
    nextDir = M.fromMaybe newFileTree (M.lookup dirName dir)
    dir' = M.insert dirName nextDir dir

ls :: FileTree -> [InputLine] -> (FileTree, [InputLine])
ls (D dir) xs = (D $ M.union xs' dir, rest)
  where
    (entries, rest) = span E.isRight xs
    xs' =
      M.fromList
        . map (\(File sz name) -> (name, F sz))
        -- TODO gross!
        . E.rights
        . E.rights
        $ entries
ls (F _) _ = error "Cannot ls a file"

construct :: FileTreeCursor -> [InputLine] -> FileTreeCursor
construct cur [] = cur
construct cur (Left (Right LS) : xs) = construct (updated, snd cur) xs'
  where (updated, xs') = ls (fst cur) xs
construct cur (Left (Left cd') : xs) = construct (cd cur cd') xs
construct _ _ = error "Cannot construct into a file"

size :: FileTree -> Int
size (F s) = s
size (D dir) = sum . map size . M.elems $ dir

-- TODO this is bad becaues it recurses multiple times!
collect :: [Int] -> FileTree -> [Int]
collect _ (F _) = []
collect sizes (D dir) = (sz : sizes) ++ concatMap (collect sizes . snd) (M.toList dir)
  where
    sz = size (D dir)

day07a :: [InputLine] -> Int
day07a inp = sum . filter (100000 >=) $ sizes
  where
    ft = fst $ cd (construct (newFileTree, []) inp) CDRoot
    sizes = collect [] ft

day07b :: [InputLine] -> Int
day07b inp = last . filter (target <=) $ sizes
  where
    ft = fst $ cd (construct (newFileTree, []) inp) CDRoot
    sizes = reverse . L.sort $ collect [] ft
    target = 30000000 - (70000000 - head sizes)
