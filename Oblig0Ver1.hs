module Main where
import Oblig0Common
  ( applyFilter,
    highPassCutoff,
    hpf,
    lowPassCutoff,
    lpf,
    zeroCrossings,
  )
import System.IO


{-
first take a line and print it.
then add the line to the datapoint.
the line is then summed together.
and added to a list. the datalength is the length of the summed data. 

hvis zeroCrossings = 2, så har det gått en periode. 


loop for getting each line and then check if zeroCrossing == 2
then our first step is done, and we can print step. 


so line -> getLine. 
check if there is a period. then print step. 
runhaskell Oblig0Ver1.hs Oblig0Common.hs < testData0
-}

lines :: Int -> IO ()
lines line = do
    let datapoints = (read line) :: [(Double, Double, Double)]
    let collected = collected ++ datapoints
    let summedData = map (\(a, b, c) -> a + b + c) collected
    let dataLength = length summedData
    let processedData =
                applyFilter (hpf highPassCutoff) $
                    applyFilter (lpf lowPassCutoff) $
                        reverse summedData
    if zeroCrossing == 2 then
        putStrLn $ "Step!"
        hFlush stdout
    else do
        getLine
        lines $ line +1
        

    

main = lines 1
