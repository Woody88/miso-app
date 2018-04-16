-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String
import Miso.Dev 
import Miso.Html.Event
import Miso.Event.Types
import qualified Data.Map as M
import Data.Monoid ((<>))
-- | Type synonym for an application model
type Model = Int

-- | Sum type for application events
data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  | Dropped
  deriving (Show, Eq)

maindev = clearBody >> main

(=:) :: k -> a -> M.Map k a 
a =: b = M.singleton a b

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model  = 0                    -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = noEff (m + 1)
updateModel SubtractOne m = noEff (m - 1)
updateModel NoOp m = noEff m
updateModel SayHelloWorld m = m <# do
  putStrLn "Hello World" >> pure NoOp
updateModel Dropped m = m <# do
    putStrLn "Dropped" >> pure NoOp
  

clickHandler = on "click" emptyDecoder $ \() -> SayHelloWorld

dropHandler = onDrop (AllowDrop True) $ \() -> Dropped

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [] [
   button_ [ onClick AddOne ] [ text "+" ]
 , text (ms x)
 , button_ [ onClick SubtractOne ] [ text "-" ]
 , div_ [ style_  $ ("background" =: "red" <> "width" =: "250px" <> "height" =: "250px")] []
 , div_ [ style_  $ ("background" =: "green" <> "width" =: "250px" <> "height" =: "250px" <> "margin-top" =: "5px"), boolProp "draggable" True, clickHandler ] []
 ]

 