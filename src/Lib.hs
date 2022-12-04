module Lib where

for :: a -> (a -> Bool) -> (a -> a) -> (a -> IO ()) -> IO ()
for i p f job
  | p i = do
    job i
    for (f i) p f job
  | otherwise = return ()

