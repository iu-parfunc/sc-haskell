
import GHC.Barrier
import Control.Monad

main :: IO ()
main = do forM_ [1..100] $ \_ -> storeLoadBarrier
          putStrLn "Did 100 barriers."

