import System.IO
import Data.List(intercalate)
import Data.Char(toLower)

main :: IO ()
main = do {
    -- inputh <- openFile "keyboard_files/positions" ReadMode;
    -- showFile inputh
    keyboardName <- getLine;

    fpos <- readFile "keyboard_files/positions";
    ffmp <- readFile "keyboard_files/fingermap";
    fkmp <- readFile $ "keyboard_files/keyboard_maps/" ++ keyboardName;
    input <- readFile "test_files/test1";

    let positions = map readPair . lines $ fpos;
        fmp = map (map (\x -> read [x])) . lines $ ffmp :: FingerMap;
        kmp = concat [makeKeyboardMap i s | (i, s) <- zip [0..] (lines . map toLower $ fkmp)] :: KeyboardMap;
        dist = sum [totalDistance finger fmp kmp positions input | finger <- [0..7]] in
    -- putStrLn $ intercalate "\n" (map show fmp)
    -- putStrLn . intercalate "\n" . map show . map (getKey kmp) $ map toLower input
    -- putStrLn . intercalate "\n" . map show . map ((!!) fmp) . filter (>=0) . map (getKey kmp) $ map toLower input
    putStrLn ("total distance for " ++ keyboardName ++ " is " ++ show dist)
}
{-
a key is
    an int representing a physical key on the keyboard.
    Should probably make a picture of this, or you can just
    look at keyboard_files/keyboard_maps/qwerty and line numbers

a finger is
    an int [0..7], where 0 = left pinky, 1 = left ring, 2 = left middle, . . . 6 = right ring, 7 = right pinky
-}
type Finger = Int
type Key = Int
type KeyboardMap = [(Char, Key)] -- maps a character to its corresponding key (ex: `('~', 0)`)
type FingerMap = [[Finger]] -- indexed by key, ints for possible fingers


--                                                                         positions |  input stream |  distance
totalDistance :: (Eq a, Floating a) => Finger -> FingerMap -> KeyboardMap -> [(a, a)] -> String      -> a
totalDistance finger fingermap keyboardmap positions input = sum .
                                                             deltaDistances $
                                                             map ((!!) positions)
                                                             [k | k <- fks, inList finger (fingermap!!k)] -- todo: wtf
                                                             where fks = filter (>=0) . map (getKey keyboardmap . toLower) $ input


deltaDistances :: (Eq a, Floating a) => [(a, a)] -> [a]
deltaDistances [] = []
deltaDistances [x] = []
deltaDistances (a:b:xs) = (dist a b):deltaDistances (b:xs)

getKey :: KeyboardMap -> Char -> Int
getKey [] c = -1
getKey ((k,i):ks) c = if c == k then i else getKey ks c

sq :: (Eq a, Floating a) => a -> a
sq x = x * x

dist :: (Eq a, Floating a) => (a, a) -> (a, a) -> a
dist (a, b) (x, y) = sqrt ( sq (x-a) + sq (y-b) )

inList :: Eq a => a -> [a] -> Bool
inList element = (>0) . length . filter ((==) element)

makeKeyboardMap :: Key -> String -> KeyboardMap
makeKeyboardMap k [] = []
makeKeyboardMap k (x:xs) = (x, k):makeKeyboardMap k xs

readPair :: (Read a, Floating a) => String -> (a, a)
readPair s = (read first, read end)
    where first = takeWhile (/=',') s; end = drop (length first + 1) s

showFile :: Handle -> IO ()
showFile h =
    do end <- hIsEOF h;
        if end
            then return ()
            else do line <- hGetLine h
                    putStrLn line
                    showFile h