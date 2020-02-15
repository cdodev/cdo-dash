module Dash.Types (Slot, Query, Message, Action, State, ui) where

import Data.Function (($))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.SVG (RGB(..), Translate(..), fill, g, height, line, rect, svg, transform, width, x, y)
import Halogen.SVG as SVG
import Prelude ((<$>), (>=))

type Slot = H.Slot Query Message

data Query a = IsOn (Boolean -> a)

data Message = Toggled Boolean

data Action = SetResults Results

data Results = R

data Question

data QType c = Scale | Binary | Category c

resultToBullet :: Results -> Bullet
resultToBullet _ = Bullet $
  { caption : "Example"
  , subCap : "An example summary"
  , scale : [100, 200, 300]
  , target : 350
  , attainment : 250
  }

newtype Bullet
  = Bullet
    { caption :: String
    , subCap :: String
    , scale :: Array Int -- 0 - 1
    , target :: Int -- 0 - 1
    , attainment :: Int -- 0 -1
    }

attainmentMet :: Bullet -> Boolean
attainmentMet (Bullet b) = b.attainment >= b.target

type BulletSettings
  = { scaleHeight :: Int
    , attainmentHeight :: Int
    , targetHeight :: Int
    -- , scale :: Array Number
    }

defaultSettings :: BulletSettings
defaultSettings =
  { scaleHeight : 25
  , attainmentHeight : 10
  , targetHeight : 20
  }

newtype State
  = State
    {
      bullet :: Maybe Bullet
    , settings :: BulletSettings
    }


initialState :: forall i. i -> State
initialState _ = State { bullet : Just (resultToBullet R), settings : defaultSettings }

ui :: forall q i o m. H.Component HH.HTML q i o m
ui =
  H.mkComponent
    { initialState
    , render
    -- , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    , eval: H.mkEval H.defaultEval
    }

render :: forall m. State -> H.ComponentHTML Action () m
render (State state) = do
  HH.div_
    [
      case state.bullet of
        Nothing -> HH.h1 [] [HH.text "Loading.."]
        Just bullet@(Bullet b) ->
          let
            label = if attainmentMet bullet then "Met" else "Missed"
          in
            svg [width 500, SVG.class_ "bullet"] -- [HP.class_ $ H.ClassName "bullet"]
              [
                g [(transform  $ Translate {x: 100, y: 2}), SVG.class_ "scale"]
                  (mkRange state.settings.scaleHeight <$> b.scale)
              ]

    ]
  where
    mkRange h i = rect [height h, width i, x 0, y 0, fill (RGB {r: 0, g: 0, b: 200})] []

newtype DashState
  = DashState
  { results :: Results
  }
