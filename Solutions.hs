module Solutions where

import Data.List

main0=interact$unlines.(unwords<$>).transpose.(words<$>).tail.lines