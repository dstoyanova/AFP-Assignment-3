import Data.List
import Data.List.Split
import System.IO
import Control.Monad
import Data.MemoCombinators

getTrackLength([]) = 0
getTrackLength((units, _):rest) = units + getTrackLength(rest)

splitEvenOdd [] = ([], [])
splitEvenOdd [x] = ([x], [])
splitEvenOdd (x:y:xs) = (x:xp, y:yp) 
    where (xp, yp) = splitEvenOdd xs

getAcc(0, 0) = [0]
getAcc(0, negativeSpeed) = -negativeSpeed:getAcc(0, negativeSpeed - 10)
getAcc(positiveSpeed, value) = positiveSpeed:getAcc(positiveSpeed - 10, value)

minSpeedRegion([], _, currentMin, _, _) = currentMin
minSpeedRegion((units, speed):rest, currentUnit, currentMin, minUnit, maxUnit) = 
    if and[currentUnit >= minUnit, currentUnit <= maxUnit, currentMin > speed] 
        then minSpeedRegion(rest, currentUnit + units, speed, minUnit, maxUnit)
        else minSpeedRegion(rest, currentUnit + units, currentMin, minUnit, maxUnit)

rally(_, [], currentUnit, _, moves, _, trackLength) 
    | currentUnit >= trackLength = moves
rally(_, [], currentUnit, _, _, _, trackLength) 
    | currentUnit < trackLength = -1
rally(_, accList, currentUnit, _, moves, _, trackLength) 
    | currentUnit >= trackLength = moves
rally(track, acc:rest, currentUnit, currentSpeed, moves, initialAcc, trackLength) 
    | (currentSpeed + acc) <= 0 = rally(track, rest, currentUnit, currentSpeed, moves, initialAcc, trackLength)
rally(track, acc:rest, currentUnit, currentSpeed, moves, initialAcc, trackLength) 
    | minSpeedRegion(track, 0, 10000, currentUnit + 1, currentUnit + (div (currentSpeed + acc) 10)) < (currentSpeed + acc) = 
        rally(track, rest, currentUnit, currentSpeed, moves, initialAcc, trackLength)
    | m == -1 = rally(track, rest, currentUnit, currentSpeed, moves, initialAcc, trackLength)
    | otherwise = m
        where m = rally(track, initialAcc, currentUnit + (div (currentSpeed + acc) 10), currentSpeed + acc, moves + 1, initialAcc, trackLength)

main::IO()
main = do
    hSetBuffering stdin LineBuffering
    s <- getLine
    let tests = read s :: Integer
    forM_ [1..tests] (\i -> do
        args <- getLine
        let trackSpeedData = map (\x -> read x :: Integer) (splitOn " " args)
        args <- getLine
        let trackSectionsData = map (\x -> read x :: Integer) (splitOn " " args)
        let (rodd, reven) = splitEvenOdd(trackSectionsData)
        let track = init(zip rodd reven)
        let initialAcc = sortBy (\x y -> if x > y then LT else GT) (getAcc(head(trackSpeedData), head(tail(trackSpeedData))))
        let len = getTrackLength(track)
        let res = rally(track, initialAcc, 0, 0, 0, initialAcc, len)
        print res) 