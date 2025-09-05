{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Scotty.Trans
import Text.Mustache
import qualified Data.Text.Lazy as L
import Data.Ix as I
import Data.Array as A

import System.Random (getStdRandom, randomR)

import Control.Concurrent.STM (TVar, newTVarIO, modifyTVar', readTVarIO, atomically)
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Control.Monad.Reader

type TileIndex = (Int, Int)
data Mark = X | O | None deriving (Eq)
instance Show Mark where
    show X = "X"
    show O = "O"
    show None = ""

swapMark :: Mark -> Mark
swapMark X = O
swapMark O = X
swapMark None = None

boardIndexes :: [TileIndex]
boardIndexes = range ((1,1),(3,3))

-- Arrays are overkill for this assignment but it's good practice
type Board = A.Array TileIndex Mark

data GameState = GameState
    { board :: Board
    , currentMark :: Mark
    , twoPlayer :: Bool
    }
    deriving (Show)

match :: Board -> [TileIndex] -> Mark
match b (ix:ixs) = foldr (\ix' m -> if m == b ! ix' then m else None) (b ! ix) ixs
match _ _ = None

winner :: Board -> Maybe Mark
winner b = 
    let v1 = match b [(1,1),(1,2),(1,3)]
        v2 = match b [(2,1),(2,2),(2,3)]
        v3 = match b [(3,1),(3,2),(3,3)]
        h1 = match b [(1,1),(2,1),(3,1)]
        h2 = match b [(1,2),(2,2),(3,2)]
        h3 = match b [(1,3),(2,3),(3,3)]
        d1 = match b [(1,1),(2,2),(3,3)]
        d2 = match b [(1,3),(2,2),(3,1)]
        w  = foldr (\line acc -> if line /= None then line else acc) None [v1,v2,v3,h1,h2,h3,d1,d2]

    in if w == None
        then 
            if (length $ openSquares b) > 0 
            then Nothing
            else Just None
        else Just w

openSquares :: Board -> [TileIndex]
openSquares b = [ fst pair | pair <- A.assocs b, snd pair == None ]

instance ToMustache GameState where
    toMustache st = object [ "mark" ~> (show $ currentMark st)
        , "tl" ~> (show $ board st ! (1,1) ) 
        , "tm" ~> (show $ board st ! (1,2) ) 
        , "tr" ~> (show $ board st ! (1,3) ) 
        , "ml" ~> (show $ board st ! (2,1) ) 
        , "mm" ~> (show $ board st ! (2,2) ) 
        , "mr" ~> (show $ board st ! (2,3) ) 
        , "bl" ~> (show $ board st ! (3,1) ) 
        , "bm" ~> (show $ board st ! (3,2) ) 
        , "br" ~> (show $ board st ! (3,3) ) 
        , "twoPlayer" ~> (twoPlayer st)
        ]

defaultGameState :: GameState
defaultGameState = GameState (A.array ((1,1), (3,3)) [(i, None) | i <- boardIndexes]) X False

newtype WebM a = WebM { runWebM :: ReaderT (TVar GameState) IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar GameState), MonadUnliftIO)

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

gets :: (GameState -> b) -> WebM b
gets f = ask >>= liftIO . readTVarIO >>= return . f

modify :: (GameState -> GameState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

render :: (ToMustache k) => String -> k -> ActionT WebM ()
render templateName k = do
    let searchSpace = [".", "./views", "./views/partials"]
    compiled <- liftIO $ automaticCompile searchSpace templateName
    case compiled of
      Left err -> text $ mconcat ["everything is awful: ", L.pack $ show err]
      Right template -> html $ L.fromStrict $ substitute template k

main :: IO ()
main = do
  sync <- newTVarIO defaultGameState
  let runActionToIO m = runReaderT (runWebM m) sync

  scottyT 8080 runActionToIO app

app :: ScottyT WebM ()
app = do
    get "/" $ do
      tv <- ask
      st <- liftIO $ readTVarIO tv
      render "index.html" st

    post "/mark" $ do
        x :: Int <- formParam "x"
        y :: Int <- formParam "y"

        tv <- ask
        state <- liftIO $ readTVarIO tv

        if ((board state) ! (x,y) /= None)
        then render "partials/board.html" state
        else do
            let currMark = currentMark state
                nextMark = swapMark currMark
                newBoard = (board state) // [((x,y), currMark)]
                newState = state { currentMark = nextMark, board = newBoard }

            webM $ modify $ \_ -> newState 

            case winner (board newState) of
                Nothing ->  if (twoPlayer newState)
                            then render "partials/board.html" newState
                            else redirect "/ai-mark"
                Just X -> redirect "/x-wins"
                Just O -> redirect "/o-wins"
                Just None -> redirect "/cats-game"

    get "/ai-mark" $ do
        tv <- ask
        state <- liftIO $ readTVarIO tv

        let spaces = openSquares (board state)
            m = currentMark state
            m' = swapMark m

        -- Random index selector, a genuinely challenging AI opponent would 
        -- use the 'match' function to determine win conditions but I don't
        -- really feel like building that today.
        ix <- getStdRandom (randomR (0, (length spaces) - 1))

        webM $ modify $ (\st -> st { board = (board st) // [((spaces !! ix), m)], currentMark = m' })

        newState <- liftIO $ readTVarIO tv

        case winner (board newState) of
            Nothing -> render "partials/board.html" newState
            Just X -> redirect "/x-wins"
            Just O -> redirect "/o-wins"
            Just None -> redirect "/cats-game"
         

    get "/toggle-ai" $ do
        webM $ modify (\st -> st { twoPlayer = (not $ twoPlayer st) })
        tv <- ask
        st <- liftIO $ readTVarIO tv
        render "partials/checkbox.html" st
          
    get "/x-wins" $ do
        tv <- ask
        oldState <- liftIO $ readTVarIO tv
        webM $ modify (\_ -> defaultGameState { twoPlayer = twoPlayer oldState })
        render "partials/boardX.html" oldState

    get "/o-wins" $ do
        tv <- ask
        oldState <- liftIO $ readTVarIO tv
        webM $ modify (\_ -> defaultGameState { twoPlayer = twoPlayer oldState })
        render "partials/boardO.html" oldState

    get "/cats-game" $ do
        tv <- ask
        oldState <- liftIO $ readTVarIO tv
        webM $ modify (\_ -> defaultGameState { twoPlayer = twoPlayer oldState })
        render "partials/boardNone.html" oldState

    get (regex "^/static/(.*)$") $ do
      filepath <- pathParam "1"
      file $ mconcat ["static/", filepath]

