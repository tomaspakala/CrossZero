import Data.List

type MoveInfo = (String, String)
type MoveInfos = (MoveInfo, MoveInfo, MoveInfo)
type Move = (String, MoveInfos)
type Moves = [Move]

data MarkType = Cross | Zero deriving (Eq)

msg :: String
msg = "{\"0\": {\"x\": 2, \"y\": 3, \"v\": \"x\"}, \"1\": {\"x\": 3, \"y\": 3, \"v\": \"o\"}, \"2\": {\"x\": 1, \"y\": 2, \"v\": \"x\"}, \"3\": {\"x\": 0, \"y\": 1, \"v\": \"o\"}, \"4\": {\"x\": 0, \"y\": 2, \"v\": \"x\"}, \"5\": {\"x\": 0, \"y\": 0, \"v\": \"o\"}, \"6\": {\"x\": 2, \"y\": 1, \"v\": \"x\"}, \"7\": {\"x\": 1, \"y\": 1, \"v\": \"o\"}}"

validate :: String -> Bool
validate str = 
    let 
        moves = sortOn fst (readMoves (drop (1) (removeSpaces str)) [])
    in (validatePositions moves) && (validateOrder moves Cross)

validatePositions :: Moves -> Bool
validatePositions [] = True
validatePositions moves = 
    if validatePosition (tail moves) (head moves)
        then validatePositions (tail moves)
        else False

validatePosition :: Moves -> Move -> Bool
validatePosition [] move = True
validatePosition moves move =
    let
        moveInfos = snd move
        movesHead = snd (head moves)
        in if (snd (fst' moveInfos) == snd (fst' movesHead)) && (snd (snd' moveInfos) == snd (snd' movesHead))
            then False
            else validatePosition (tail moves) move

validateOrder :: Moves -> MarkType -> Bool
validateOrder [] markType = True
validateOrder moves markType = 
    let
        value = snd (thr' (snd (head moves)))
        in if ((value == "x" || value == "X") && markType == Cross) 
            then validateOrder (tail moves) Zero
            else if ((value == "o" || value == "O" || value == "0") && markType == Zero)
            then validateOrder (tail moves) Cross
            else False

fst' :: MoveInfos -> MoveInfo
fst' (a,_,_) = a

snd' :: MoveInfos -> MoveInfo
snd' (_,b,_) = b

thr' :: MoveInfos -> MoveInfo
thr' (_,_,c) = c

removeSpaces :: String -> String
removeSpaces str = filter(/=' ') str

readMoves :: String -> Moves -> Moves
readMoves [] acc = acc
readMoves str acc =
    let
        (move, rest) = readMove str
    in readMoves rest (move : acc)

readMove :: String -> (Move, String)
readMove move = 
    let
        key = removeExtraSymbols (takeWhile (/= ':') move) 1 1
        withoutKey = drop (length key + 3) move
        values = takeWhile (/= '}') withoutKey
        rest = drop (length key + length values + 5) move
        xCoordinate = readCoordinate values
        yCoordinate = readCoordinate (drop (length xCoordinate + 5) values)
        value = readMoveValue (drop (length xCoordinate + length yCoordinate + 10) values)
    in ((key, (xCoordinate, yCoordinate, value)), rest)

readMoveValue :: String -> MoveInfo
readMoveValue pair =
    let 
        key = removeExtraSymbols (takeWhile (/= ':') pair) 2 1
        withoutKey = drop (length key + 3) pair
        rest = removeExtraSymbols (takeWhile (/= ',') withoutKey) 2 1
    in (key, rest)

readCoordinate :: String -> MoveInfo
readCoordinate pair =
    let 
        key = removeExtraSymbols (takeWhile (/= ':') pair) 2 1
        withoutKey = drop (length key + 4) pair
        rest = takeWhile (/= ',') withoutKey
    in (key, rest)

removeExtraSymbols :: String -> Int -> Int-> String
removeExtraSymbols symbols dropStart dropEnd=
    let
        withoutBegining = drop (dropStart) symbols
        finished = take (length withoutBegining - dropEnd) withoutBegining
    in finished