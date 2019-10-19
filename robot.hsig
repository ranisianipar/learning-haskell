module Robot where
    import Data.Array
    import Data.List
    import Control.Monad
    import Control.Applicative
    import System.IO
    import SOE
    
data RobotState 
   = RobotState 
         { position  :: Position
         , facing    :: Direction
         , pen       :: Bool 
         , color     :: Color
         , treasure  :: [Position]
         , pocket    :: Int
         }
      deriving Show


spiral :: Robot ()
spiral = penDown >> loop 1 
    where loop n =
           let twice = do turnRight
                          moven n
                          turnRight
                          moven n
           in cond blocked 
                (twice >> turnRight >> moven n)
                (twice >> loop (n+1))