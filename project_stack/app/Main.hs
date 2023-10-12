module Main where
import Data.Map as Map
import Data.List (sortBy, groupBy)
import Data.Function (on)
import System.Random
import Data.List
import System.IO.Unsafe
import System.IO
import Control.Exception

main :: IO ()
main= do 
        putStrLn "\n> Hello, welcome to Hidato Puzzles. \n\n [*] Please type help if you do not know how to play or exit to quit the game."
        Main.cycle
 
cycle:: IO()
cycle = do  putStrLn "\n> " 
            params <- getLine
            if ((words params)!!0)/="exit" then do
                 catch (options params) handler
                 Main.cycle
            else putStrLn "Thanks for playing"
            where 
               handler:: SomeException -> IO()
               handler ex = putStrLn ("There was an error") 

options:: String-> IO()
options option = case (optlist!!0) of
               "solve" -> (onlysolve (optlist!!1) (read(optlist!!2)) ) 
               "solve_save" -> (solveandsave (optlist!!1) (optlist!!2) (read(optlist!!3)))
               "solve_save_shape" -> (solvesaveshape (optlist!!1) (optlist!!2) (optlist!!3) (read(optlist!!4)))
               "generate" -> (onlygenerate (read(optlist!!1)) (read(optlist!!2)) (read(optlist!!3)) (read(optlist!!4)) (read(optlist!!5)))
               "generate_save"-> generatesave  (read(optlist!!1)) (read(optlist!!2)) (read(optlist!!3)) (read(optlist!!4)) (read(optlist!!5)) (optlist!!6) 
               "generate_save_shape" ->generatesaveshape (read(optlist!!1)) (read(optlist!!2)) (read(optlist!!3)) (read(optlist!!4)) (read(optlist!!5)) (optlist!!6) (optlist!!7) 
               "help" ->help xs
               otherwise -> putStrLn "Command not found"
               where
                    optlist = words option
                    (x:xs)= optlist


onlysolve:: [Char]->Int->IO()
onlysolve txt max |isValidBoard board = prettyPrintHidato (board!!0)
                  | otherwise =  putStrLn "Hidato is invalid"
                  where board= solve (readBoard txt) max

solveandsave:: [Char]->[Char]->Int-> IO()
solveandsave txt1 txt2 max | isValidBoard board = writeBoard txt2 (board!!0)
                           | otherwise = putStrLn"Hidato is invalid"
                           where board = solve (readBoard txt1) max 

solvesaveshape:: [Char]->[Char]->[Char]->Int-> IO()
solvesaveshape txt1 txt2 txt3 max | isValidBoard board = do 
                                                       writeBoard txt2 (board!!0)
                                                       writeBoardShape txt3 (board!!0)
                                  | otherwise = putStrLn"Hidato is invalid"
                                  where board = solve (readBoard txt1) max 

onlygenerate :: CellType -> Int -> Int -> DifficultyMode -> ObstaclesPercentage -> IO() 
onlygenerate ct row col dif obs = prettyPrintHidato (generate ct row col dif obs)

generatesave :: CellType -> Int -> Int -> DifficultyMode -> ObstaclesPercentage -> [Char] -> IO()
generatesave ct row col dif obs txt = writeBoard txt (generate ct row col dif obs)

generatesaveshape :: CellType -> Int -> Int -> DifficultyMode -> ObstaclesPercentage -> [Char] -> [Char] -> IO()
generatesaveshape ct row col dif obs txt1 txt2 = do
                                                  writeBoard txt1 (generate ct row col dif obs)
                                                  writeBoardShape txt2 (generate ct row col dif obs)

help :: [[Char]] -> IO()
help [] = putStrLn "\n Commands: \n\n  -solve \n  -solve_save \n  -solve_save_shape \n  -generate \n  -generate_save \n  -generate_save_shape \n  -exit"        
help command = case (command!!0) of
               "solve" -> putStrLn "\n - solve filename hidato_value \n\n Definition:\n  Solves Hidato Board read from filename and prints it.\n\n Params:\n  filename: Name of the file txt where the Hidato Board to be solved is written.\n  hidato_value: maximun value in the Hidato Board. \n\n Example : solve solvedBoard.txt 66"
               "solve_save" -> putStrLn "\n - solve_save filename filenamesave hidato_value \n\n Definition:\n  Solves Hidato Board read from filename and saves it in filenamesave.\n\n Params:\n  filename: Name of the txt file where the Hidato Board to be solved is written. \n  filenamesave: Name of the txt file where the solved Hidato Board will be saved. \n  hidato_value: maximun value in the Hidato Board.  \n\n Example: solve_save solvedBoard.txt saveBoard.txt 66"
               "solve_save_save" -> putStrLn "\n - solve_save_shape filename filenamesave filenamesaveshape hidato_value \n\n Definition:\n  Solves Hidato Board read from filename, saves it in its original format in filenamesave and saves it in its print format in filenameshape.\n\n Params:\n  filename: Name of the txt file where the Hidato Board to be solved is written. \n  filenamesave: Name of the txt file where the solved Hidato Board will be saved. \n  filenameshape: Name of the txt file where the solved board will be saved in its print format. \n  hidato_value: maximun value in the Hidato Board.  \n\n Example: solve solvedBoard.txt saveBoard.txt shapeBoard.txt 66"
               "generate" -> putStrLn "\n - generate CellType rows cols DifficultyMode ObstaclesPercentage \n\n Definition:\n  Generates a valid Hidato Board and prints it.\n\n Params:\n  CellType: Type of cells of the board. Values are Square and Hexagon. \n  rows: Number of rows of the Hidato Board to be generated. \n  cols: Number of columns of the Hidato Board to be generated. \n  DifficultyMode: Number of fixed cells in the board. Values are Easy, Medium and Difficult. \n  Obstaclespercentage: Number of invalid cells in the board. Values are O0, O15, O25 and O45. \n\n Example: generate Square 7 8 Medium O15"
               "generate_save" -> putStrLn "\n - generate CellType rows cols DifficultyMode ObstaclesPercentage filenamesave \n\n Definition:\n  Generates a valid Hidato Board and saves it in filenamesave.\n\n Params:\n  CellType: Type of cells of the board. Values are Square and Hexagon. \n  rows: Number of rows of the Hidato Board to be generated. \n  cols: Number of columns of the Hidato Board to be generated. \n  DifficultyMode: Number of fixed cells in the board. Values are Easy, Medium and Difficult. \n  Obstaclespercentage: Number of invalid cells in the board. Values are O0, O15, O25 and O45. \n  filenamesave: Name of the txt file where the generated Hidato Board will be saved.  \n\n Example: generate_save Square 7 8 Medium O15 generatedBoard.txt"
               "generate_save_shape" -> putStrLn "\n - generate CellType rows cols DifficultyMode ObstaclesPercentage filenamesave filenameshape \n\n Definition:\n  Generates a valid Hidato Board, saves it in its original format in filenamesave and saves it in its print format in filenameshape.\n\n Params:\n  CellType: Type of cells of the board. Values are Square and Hexagon. \n  rows: Number of rows of the Hidato Board to be generated. \n  cols: Number of columns of the Hidato Board to be generated. \n  DifficultyMode: Number of fixed cells in the board. Values are Easy, Medium and Difficult. \n  Obstaclespercentage: Number of invalid cells in the board. Values are O0, O15, O25 and O45. \n  filenamesave: Name of the txt file where the generated Hidato Board will be saved. \n  filenameshape: Name of the txt file where the solved board will be saved in its print format.  \n\n Example: generate_save Square 7 8 Medium O15 generatedBoard.txt generatedShapeBoard.txt"
               "exit" -> putStrLn "\n - exit \n\n Definition:\n Exits game."
               otherwise -> putStrLn "Command not found"                

-- main = putStrLn(printBoardList ([]:(fromBoardtoList (board!!0))) Hexagon)
--         where board= solve (fromListtoBoard[(5,(P 4 0)),(1,(P 6 0)),(2,(P 8 0)),(0,(P 10 0)),(6,(P 3 1)),(4,(P 5 1)),(3,(P 7 1)), (30,(P 9 1)),(0,(P 11 1)),  (7,(P 2 2)),(0,(P 4 2)), (0,(P 6 2)),(0,(P 8 2)),(0,(P 10 2)),(0,(P 12 2)), (0,(P 1 3)),(0,(P 3 3)),(37,(P 5 3)),(33,(P 7 3)),(0,(P 9 3)),(0,(P 11 3)),(26,(P 13 3)), (9,(P 2 4)),(0,(P 4 4)),(11,(P 6 4)),(0,(P 8 4)),(22,(P 10 4)),(25,(P 12 4)), (0,(P 3 5)),(0,(P 5 5)),(0,(P 7 5)),(20,(P 9 5)),(0,(P 11 5)), (0,(P 4 6)),(0,(P 6 6)),(0,(P 8 6)),(19,(P 10 6))] Hexagon) 37
-- main = putStr( mixingString ("  "++hexprintingTop [(P 1 0 ,0), (P 3 1,23),(P 5 1,0),(P 9 0,0),(P 11 0,33)]) (hexprintingBottom [(P 0 0,1 ), (P 2 0,23),(P 4 0,0),(P 10 0,0),(P 12 0,33)]))
-- main = putStrLn(" / \\ / \\ / \\ \n|   |   |   | \n \\ / \\ / \\ / \n  |   |   | \n   \\ / \\ /" ++ show(23) ) 

optionSelector:: String -> String
optionSelector s = case s of
                  "Solve" -> "Please type your Hidato Board file name \n Example: solve \"hidato.txt\""
                  "Solve and save" ->"Please type your Hidato Board file name and the file name for saving the solution \n Example: \"hidato.txt\" \"savehere.txt\" \n WARNING: you will override the saving file info with new one"
                  "Solve, save and save shape"-> "Please type your Hidato Board file name and the file name for saving the solution and the file name for placing Hidato shape \n Example: \"hidato.txt\" \"savehere.txt\" \n WARNING: you will override the saving file info with new one"


data IntegerOrderPair = P{xv :: Int, yv :: Int} deriving(Show,Eq,Ord, Read)
data CellType = Square | Hexagon deriving(Show,Read, Eq)
data HidatoBoard = HidatoBoard { fixed :: (Map Int IntegerOrderPair), available:: (Map IntegerOrderPair Int), cellt:: CellType} | Empty deriving(Read,Show, Eq)

data DifficultyMode = Easy | Medium | Difficult deriving(Show, Read)
data ObstaclesPercentage = O0 | O15 | O25 | O45 deriving(Show, Read)

sqareAdj :: IntegerOrderPair -> [IntegerOrderPair]
sqareAdj (P x y) = [P (x-1) y, P (x+1) y, P (x-1) (y-1), P (x-1) (y+1),P (x+1) (y-1), P (x+1) (y+1), P x (y-1), P x (y+1)]

hexagonAdj :: IntegerOrderPair -> [IntegerOrderPair]
hexagonAdj (P x y) = [P (x-2) y, P (x+2) y, P (x-1) (y-1), P (x-1) (y+1),P (x+1) (y-1), P (x+1) (y+1)]

boardAdj :: HidatoBoard -> (IntegerOrderPair -> [IntegerOrderPair])
boardAdj (HidatoBoard _ _ Square) = sqareAdj
boardAdj (HidatoBoard _ _ Hexagon) = hexagonAdj 

lengthAdjList :: CellType -> Int
lengthAdjList Square = 8
lengthAdjList Hexagon = 6


fixedCellsDictionary :: [(Int,IntegerOrderPair)] -> Map Int IntegerOrderPair
fixedCellsDictionary list = fromList [(x,y) | (x,y) <- list, x > 0]

availableCellsDictionary ::  [(Int,IntegerOrderPair)] -> Map IntegerOrderPair Int
availableCellsDictionary list = fromList [(y,x) | (x,y) <- list, x == 0]

possibleNextboard :: HidatoBoard->IntegerOrderPair-> Int-> [IntegerOrderPair]->[(HidatoBoard, IntegerOrderPair)]
possibleNextboard board@(HidatoBoard fixedvalues availablevalues cellt) p cv adj  | member cv fixedvalues = [(board,x) | x <- [fixedvalues ! cv], isadj x p cellt]
                                                                       | otherwise = [(HidatoBoard (Map.insert cv x fixedvalues) (Map.delete  x availablevalues) cellt,x)|x <- adj, member x availablevalues, (availablevalues ! x) == 0]

isadj:: IntegerOrderPair-> IntegerOrderPair ->CellType-> Bool
isadj x y Square = isadjSquare x y
isadj x y Hexagon = isadjHexagon x y

isadjSquare :: IntegerOrderPair-> IntegerOrderPair -> Bool 
isadjSquare (P x y) (P z w) = abs(x-z) <= 1 && abs(y - w)<=1       

isadjHexagon :: IntegerOrderPair-> IntegerOrderPair -> Bool 
isadjHexagon (P x y) (P z w) = (abs(x-z) == 1 && abs(y - w) ==1) || ( y == w && abs(x - z) ==2) 

fromListtoBoard :: [(Int,IntegerOrderPair)] -> CellType -> HidatoBoard
fromListtoBoard list ctype = HidatoBoard (fixedCellsDictionary list) (availableCellsDictionary list) ctype


walkDFS :: HidatoBoard-> IntegerOrderPair-> Int-> Int-> [HidatoBoard]
walkDFS currentBoard currentp currentv maxv |currentv > maxv = [currentBoard]
                                            | otherwise = [b | (x,y)<- pnBoards, b <-(walkDFS x y (currentv+1) maxv)] 
                                                where pnBoards = possibleNextboard currentBoard currentp currentv ((boardAdj currentBoard) currentp)


solve :: HidatoBoard -> Int-> [HidatoBoard]
solve board max = if (member 1 f) && (member max f) then walkDFS board (f!1) 2 max else []
                where f = fixed board

solveformlist :: [(Int,IntegerOrderPair)] ->CellType-> Int->[HidatoBoard]
solveformlist list ctype max=if (member 1 f) && (member max f) then walkDFS board (f!1) 2 max else []
                          where 
                                board = fromListtoBoard list ctype
                                f = fixed board

--Generator

generate :: CellType -> Int -> Int -> DifficultyMode -> ObstaclesPercentage -> HidatoBoard
generate Square rows cols difficulty obstacles = getBoard (fromListtoBoard [(x, P y z) | x <-[0], y <- [0,1..(rows-1)], z <- [0,1..(cols-1)]] Square) obstacles difficulty (rows*cols) 
generate Hexagon rows cols difficulty obstacles = getBoard (fromListtoBoard ([(x, P y z) | x <-[0], y <- [0,2..(rows-1)], z <- [0,2..(2*(cols-1))]] ++ [(x, P y z) | x <-[0], y <- [1,3..(rows-1)], z <- [1,3..(2*cols)]]) Hexagon) obstacles difficulty (rows*cols)
                                                    

getMax :: ObstaclesPercentage -> Int -> Int
getMax O0 max = max
getMax O15 max = max - (div (max * 15) 100)
getMax O25 max = max - (div (max * 25) 100)
getMax O45 max = max - (div (max * 45) 100)


getDifficulty :: DifficultyMode-> Int -> Int
getDifficulty Easy max = (div (max * 30) 100)
getDifficulty Medium max = (div (max * 45) 100)
getDifficulty Difficult max = (div (max * 60) 100)

getBoard :: HidatoBoard -> ObstaclesPercentage -> DifficultyMode -> Int -> HidatoBoard       
getBoard board obstacles difficulty max = removeElemsBoard filledBoard (fromBoardtoOneList filledBoard) hidatoValue removedElems 0 hidatoValue
                                            where
                                                pos = validPair board (P (-1) (-1)) max
                                                hidatoValue = getMax obstacles max
                                                removedElems = getDifficulty difficulty hidatoValue
                                                filledBoard = fillBoard board 1 pos hidatoValue    

        

validPair :: HidatoBoard -> IntegerOrderPair -> Int -> IntegerOrderPair
validPair board pair max | member pair (available board) = pair
                         | otherwise = validPair board newPair max
                         where 
                            possiblePair = unsafePerformIO (getRandomList 2 max) :: [Int]
                            x = (possiblePair !! 0)
                            y = (possiblePair !! 1) 
                            newPair = P x y

                    


fillBoard :: HidatoBoard -> Int -> IntegerOrderPair -> Int -> HidatoBoard
fillBoard board num pos max | num == max = HidatoBoard (fixed newBoard) Map.empty (cellt newBoard)
                            | noAdj board ((boardAdj board) pos) 0 = Empty
                            | otherwise = possibleBoard
                            where
                                newBoard = HidatoBoard (Map.insert num pos (fixed board)) (Map.delete pos (available board)) (cellt board)
                                newNum = num + 1
                                possibleBoard = nextPos 0

                                nextPos index | index == (length adj) = Empty
                                              | not (member newPos (available newBoard)) = nextPos (index + 1)
                                              | solution /= Empty = solution
                                              | otherwise = nextPos (index + 1)
                                              where
                                                adj = getRandomAdj ((boardAdj board) pos) (lengthAdjList (cellt board))
                                                newPos = adj !! index
                                                solution = fillBoard newBoard newNum newPos max



removeElemsBoard :: HidatoBoard -> [(IntegerOrderPair, Int)] -> Int -> Int -> Int -> Int -> HidatoBoard
removeElemsBoard board [] _ _ _ _= board
removeElemsBoard board boardList lengthBoardList numberOfElems index max | index >= numberOfElems = board
                                                                             | not(isValidBoard (solve newBoard max)) = removeElemsBoard board newBoardList newlengthBoardList numberOfElems index max
                                                                             | otherwise = removeElemsBoard newBoard newBoardList newlengthBoardList numberOfElems newIndex max
                                                                             where 
                                                                                newlengthBoardList = lengthBoardList - 1                                                               
                                                                                i = unsafePerformIO (getRandomNumber newlengthBoardList) :: Int
                                                                                elem = boardList !! i
                                                                                newBoardList = Data.List.delete elem boardList
                                                                                newBoard = HidatoBoard (Map.delete (snd elem) (fixed board)) (Map.insert (fst elem) 0 (available board)) (cellt board)
                                                                                newIndex = index + 1
                                                                

fromBoardtoOneList :: HidatoBoard -> [(IntegerOrderPair, Int)]
fromBoardtoOneList board = concat [[(x,y) | y<-keys(fixed board), x<-[(fixed board)!y] ],[(v,w)| v<- keys(available board), w<-[(available board!v)]]]

isValidBoard :: [HidatoBoard] -> Bool
isValidBoard [] = False
isValidBoard (x:y:xs) = False 
isValidBoard board = True

getRandomList :: Int -> Int -> IO [Int]
getRandomList n max = do
        seed <- newStdGen
        let list = (Data.List.map(\x -> (mod x max)) (randomList n seed))
        return list
        

randomList :: Int -> StdGen -> [Int]
randomList n = Prelude.take n . unfoldr (Just . random)

removeElems :: HidatoBoard -> [Int] -> Int -> HidatoBoard
removeElems board elems index | (index == (length elems)) = board
                              | otherwise = removeElems newBoard elems newIndex
                              where
                                elem = elems !! index
                                pos = ((fixed board) ! elem)
                                newBoard = HidatoBoard (Map.delete elem (fixed board)) (Map.insert pos elem (available board)) (cellt board)
                                newIndex = index + 1

getRandomAdj :: [IntegerOrderPair] -> Int -> [IntegerOrderPair]
getRandomAdj [] _ = []
getRandomAdj _ 0 = []
getRandomAdj adj lengthAdj = x:(getRandomAdj (Data.List.delete x adj) (lengthAdj -1))
                           where 
                                rand = unsafePerformIO (getRandomNumber (lengthAdj-1))
                                x = adj!!rand
 
getRandomNumber :: Int -> IO Int
getRandomNumber max = do
        seed <- newStdGen
        let number = fst $ randomR (0,max) seed
        return number

noAdj :: HidatoBoard -> [IntegerOrderPair] -> Int -> Bool
noAdj board adj index | index == (length adj) = True
                      | member (adj !! index) (available board) = False
                      | otherwise = noAdj board adj (index + 1)

replaceAt :: Int -> Int -> [(Int,IntegerOrderPair)] -> [(Int,IntegerOrderPair)]
replaceAt _ _ [] = []
replaceAt i newVal (x:xs) | i == 0 = (k:xs)
                          | otherwise = x:replaceAt (i-1) newVal xs
                          where k = (newVal,snd x)

---Print
                  
printingCells:: (IntegerOrderPair,Int) -> [Char]
printingCells (_,p)= if p==0 then "|   " else if len==1 then "| "++show(p) ++" " else if len==2 then "|" ++show(p) ++" " else "|" ++show(p)
                       where len =length(show(p)) 

hexprintingCells:: [(IntegerOrderPair,Int)] -> [Char]
hexprintingCells (((P x _),_):((P v _),p):xs) | v== (x + 2) = (printingCells (P v v,p))++hexprintingCells (((P v v),v):xs) 
                                              | otherwise =  "|"++generateEmptySpace((div (v-x-2) 2)-1)++"   "++hexprintingCells(((P (v-2) v),v):((P v v),p):xs)                 
hexprintingCells _ = "|\n" 

sqprintingCells:: [(IntegerOrderPair,Int)] -> [Char]
sqprintingCells (((P x _),_):((P v _),p):xs) | v== (x + 1) = (printingCells (P v v,p))++sqprintingCells (((P v v),v):xs)  
                                             | otherwise =  "|"++generateEmptySpace(v-x-2)++"   "++sqprintingCells(((P (v-1) v),v):((P v v),p):xs)                 
sqprintingCells _ = "|\n" 

generateEmptySpace::Int ->[Char]
generateEmptySpace 0 = ""
generateEmptySpace x = "    "++generateEmptySpace(x-1)

printingTopBottom:: [(IntegerOrderPair,Int)]->[Char]->Int->Int -> [Char]-> [Char]
printingTopBottom (((P x _),_):((P v _),p):xs) c inc m sc | v== (x + inc) = c ++printingTopBottom ((P v v,v):xs) c inc m sc
                                                       | otherwise = sc++generateEmptySpace((div (v-x-2) inc)-m)++"   "++printingTopBottom((P (v-inc) x,x):(P v x,x):xs) c inc m sc
printingTopBottom _ _ _ _ sc= sc 

hexprintingBottom:: [(IntegerOrderPair,Int)] -> [Char]
hexprintingBottom a = printingTopBottom a " \\ /" 2 1 " "

hexprintingTop:: [(IntegerOrderPair,Int)] -> [Char]
hexprintingTop a = printingTopBottom a " / \\" 2 1 " "

sqprintingLine::[(IntegerOrderPair,Int)] -> [Char] 
sqprintingLine a = printingTopBottom a "+---" 1 0 "+"

printBoardList:: [[(IntegerOrderPair,Int)]]->CellType->[Char]
printBoardList ([]:x:xs) Square = (sqprintingInterRow [] [x] xv)++(printBoardList (x:xs) Square)
                           where (P xv _)= fst(x !! 0)
printBoardList ([]:x:xs) Hexagon = (hexprintingInterRow [] [x] xv)++(printBoardList (x:xs) Hexagon)
                           where (P xv _)= fst(x !! 0)
printBoardList (x:xs) Square = generateEmptySpace(xv)++sqprintingCells((P (xv -1) yv,xv) :x)++(sqprintingInterRow x xs xv)++(printBoardList xs Square)
                          where (P xv yv)= fst(x !! 0)
printBoardList (x:xs) Hexagon | (mod xv 2) == 0 =  generateEmptySpace(div xv 2)++(hexprintingCells ((P (xv -2) yv,xv) :x))++(hexprintingInterRow x xs xv)++(printBoardList xs Hexagon)
                        | otherwise = "  "++generateEmptySpace(div (xv-1) 2)++(hexprintingCells ((P (xv -2) yv,xv) :x))++(hexprintingInterRow x xs xv)++(printBoardList xs Hexagon)
                        where (P xv yv)= fst(x !! 0) 
printBoardList [] _ = ""


mixingChar:: Char-> Char -> Char
mixingChar ' ' x = x
mixingChar x ' ' = x
mixingChar x y | x==y = x 
               | otherwise = ' ' 

mixingString :: [Char]-> [Char] -> [Char]
mixingString [] s2 = s2++"\n"
mixingString s1 [] = s1++"\n"
mixingString (a:s1) (b:s2) = (mixingChar a b):(mixingString s1 s2)

hexprintingInterRow::[(IntegerOrderPair,Int)] -> [[(IntegerOrderPair,Int)]]->Int->[Char]
hexprintingInterRow [] x xv = if (mod xv 2) == 0 then generateEmptySpace(div xv 2)++(hexprintingTop ((P (xv -2) xv,xv) :(x!!0)))++"\n" else generateEmptySpace(div (xv-1) 2)++"  "++(hexprintingTop ((P (xv -2) xv,xv) :(x!!0)))++"\n"
hexprintingInterRow x [] xv = if (mod xv 2) == 0 then generateEmptySpace(div xv 2)++(hexprintingBottom ((P (xv -2) xv,xv) :x))++"\n" else generateEmptySpace(div (xv-1) 2)++"  "++(hexprintingBottom ((P (xv -2) xv,xv) :x))++"\n"
hexprintingInterRow x (y:ys) xv | (mod xv 2) == 0 = mixingString (generateEmptySpace(div xv 2)++(hexprintingBottom ((P (xv -2) xv,xv) :x))) ("  "++generateEmptySpace(div (yv-1) 2)++(hexprintingTop((P (yv -2) yv,yv):y)))
                                | otherwise = mixingString (generateEmptySpace(div yv 2)++(hexprintingTop ((P (yv -2) yv,yv) :y))) ("  "++generateEmptySpace(div (xv-1) 2)++(hexprintingBottom ((P (xv -2) xv,xv):x)))
                                where (P yv _) = fst(y !! 0)

sqprintingInterRow:: [(IntegerOrderPair,Int)] -> [[(IntegerOrderPair,Int)]]-> Int->[Char] 
sqprintingInterRow x [] xv = generateEmptySpace(xv)++sqprintingLine((P (xv -1) xv,xv) :x)
sqprintingInterRow [] x xv = generateEmptySpace(xv)++sqprintingLine((P (xv -1) xv,xv) :(x!!0)) ++ "\n"
sqprintingInterRow x (y:ys) xv= mixingString (generateEmptySpace(xv)++sqprintingLine((P (xv -1) xv,xv) :x)) (generateEmptySpace(yv)++sqprintingLine((P (yv -1) yv,xv) :y))
                              where (P yv _) = fst(y !! 0)

fromBoardtoList:: HidatoBoard -> [[(IntegerOrderPair,Int)]] 
fromBoardtoList board = groupBy sameYvalue (sortBy sortPairs (concat [[(x,y) | y<-keys(fixed board), x<-[(fixed board)!y] ],[(v,w)| v<- keys(available board), w<-[(available board!v)]]])) 

sortPairs :: (IntegerOrderPair,Int)->(IntegerOrderPair,Int)->Ordering 
sortPairs (P x y,_) (P v w,_) = case compare y w of
                                EQ -> compare x v 
                                LT -> LT 
                                GT -> GT

sameYvalue::(IntegerOrderPair,Int)->(IntegerOrderPair,Int)->Bool 
sameYvalue (P _ y,_) (P _ w,_) = y==w

prettyPrintHidato :: HidatoBoard ->IO()
prettyPrintHidato board@(HidatoBoard _ _ t) = putStrLn(printBoardList ([]:(fromBoardtoList board)) t ++ "\n\n") 

prettyPrintHidatoStr :: HidatoBoard ->String
prettyPrintHidatoStr board@(HidatoBoard _ _ t) = printBoardList ([]:(fromBoardtoList board)) t

readBoard:: [Char]-> HidatoBoard 
readBoard txt = (read (unsafePerformIO (readFile txt)))

writeBoard:: [Char]->HidatoBoard -> IO()
writeBoard txt board = writeFile txt (show board)

writeBoardShape:: [Char]->HidatoBoard -> IO()
writeBoardShape txt board = writeFile txt (prettyPrintHidatoStr board)