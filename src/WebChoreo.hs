{-# LANGUAGE GADTs, RankNTypes, FlexibleInstances, OverloadedStrings #-}
module WebChoreo where

import Data.List
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai as Wai
import qualified Network.WebSockets as WS

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Free f a where
  Return :: a -> Free f a
  Do :: f a -> (a -> Free f b) -> Free f b

instance Functor (Free f) where
  fmap f (Return a) = Return (f a)
  fmap g (Do fa h) = Do fa (\a -> fmap g (h a))

instance Applicative (Free f) where
  pure a = Return a
  (Return f) <*> a = fmap f a
  (Do fa h) <*> b = Do fa (\a -> h a <*> b)

instance Monad (Free f) where
  (Return a) >>= f = f a
  (Do fa h) >>= g = Do fa (\a -> h a >>= g)

interpFree :: Monad m => (forall a. f a -> m a) -> Free f a -> m a
interpFree _ (Return a) = return a
interpFree nt (Do fa g) = nt fa >>= interpFree nt . g

toFree :: f a -> Free f a
toFree fa = Do fa Return

type Server a = Maybe a

data JS v a where
  Var :: v a -> JS v a
  StringLit :: String -> JS v String
  StringConcat :: JS v String -> JS v String -> JS v String
  Display :: JS v String -> JS v ()
  JSBind :: JS v a -> (v a -> JS v b) -> JS v b
  Recv :: JS v String
  Halt :: JS v ()

data StringVar a = SV String

instance Show (JS StringVar a) where
  show a = jsPrefix ++ "\n" ++ showJS 0 a ++ jsSuffix

jsPrefix :: String
jsPrefix =
  intercalate "\n"
    [ "<!DOCTYPE html>"
    , "<html>"
    , "<body>"
    , "<script>"
    , "const socket = new WebSocket(\"ws://\" + window.location.host);"
    , "function chan() {"
    , "  const stack = [];"
    , "  var next = false;"
    , "  async function *mkchan() {"
    , "    while (true) {"
    , "      yield new Promise((res) => {"
    , "        if (stack.length > 0) {"
    , "          res(stack.shift());"
    , "        } else {"
    , "          next = res;"
    , "        }"
    , "      });"
    , "    }"
    , "  }"
    , "  const c = mkchan();"
    , "  c.put = (v) => {"
    , "    if (next) {"
    , "      const res = next;"
    , "      next = false;"
    , "      res(v);"
    , "    } else {"
    , "      stack.push(v);"
    , "    }"
    , "  };"
    , "  return c;"
    , "}"
    , "const msgc = chan();"
    , "socket.onmessage = (ev) => { msgc.put(ev.data); };"
    , "(async function () {"
    ]

jsSuffix :: String
jsSuffix =
  intercalate "\n"
    [ "})();"
    , "</script>"
    , "</body>"
    , "</html>"
    ]

showJS :: Integer -> JS StringVar a -> String
showJS l (Var (SV x)) = x
showJS l (StringLit a) = show a
showJS l (StringConcat a b) = concat ["(", showJS l a, " + ", showJS l b, ")"]
showJS l (Display s) = concat ["console.log(", showJS l s, ")"]
showJS l (JSBind a f) = concat ["const x", show l, " = ", showJS l a, ";\n", showJS (l + 1) (f (SV ("x" ++ show l)))]
showJS l (Recv) = "(await msgc.next()).value"
showJS l (Halt) = ""

type Browser v a = Maybe (JS v a)

data ChoreoSig v a where
   OnServer :: ((forall b. Server b -> b) -> a) -> ChoreoSig v (Server a)
   InBrowser :: forall v a. ((forall b. Browser v b -> JS v b) -> JS v a) -> ChoreoSig v (Browser v a)
   ServerToBrowser :: Server String -> ChoreoSig v (Browser v String)

type Choreo v a = Free (ChoreoSig v) a

printJSAlg :: ChoreoSig StringVar a -> IO a
printJSAlg (OnServer _) = return Nothing
printJSAlg (InBrowser js) = do
  let r = js fromJust
  putStrLn (show r)
  return (Just r)
printJSAlg (ServerToBrowser _) = do
  putStrLn (show (Recv :: JS StringVar String))
  return (Just Recv)

showBrowser :: Choreo StringVar () -> String
showBrowser c = show (extractJS c)

extractJS :: Choreo StringVar a -> JS StringVar ()
extractJS (Return a) = Halt
extractJS (Do (OnServer _) f) = extractJS (f Nothing)
extractJS (Do (InBrowser f) g) = JSBind (f fromJust) (\a -> extractJS (g (Just (Var a))))
extractJS (Do (ServerToBrowser _) f) = JSBind Recv (\x -> extractJS (f (Just (Var x))))

performServer :: WS.Connection -> Choreo v () -> IO ()
performServer conn c = interpFree alg c
  where
    alg :: ChoreoSig v a -> IO a
    alg (OnServer f) = return (Just (f fromJust))
    alg (InBrowser _) = return Nothing
    alg (ServerToBrowser s) = WS.sendTextData conn (fromString (fromJust s) :: Text) >> return Nothing

runServer :: Choreo StringVar () -> Int -> IO ()
runServer c port =
  Warp.run
    port
    (WaiWS.websocketsOr
      WS.defaultConnectionOptions
      (\pending -> do
        conn <- WS.acceptRequest pending
        performServer conn c
      )
      (\req resp ->
        resp (Wai.responseLBS HTTP.status200 [] (fromString (show (extractJS c))))
      )
    )
