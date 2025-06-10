module Main (main) where

import WebChoreo

test :: Choreo v ()
test = do
  hiS <- toFree (InBrowser (\_ -> StringLit "Hello "))
  s <- toFree (OnServer (\_ -> "World!"))
  thereS <- toFree (ServerToBrowser s)
  hiThereS <- toFree (InBrowser (\unB -> StringConcat (unB hiS) (unB thereS)))
  _ <- toFree (InBrowser (\unB -> Display (unB hiThereS)))
  return ()

main :: IO ()
main = runServer test 3141
