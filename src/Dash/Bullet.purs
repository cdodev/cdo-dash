module Dash.Bullet (Slot, Query, Input(..), Message, Action, State, ui, Bullet, BulletSettings, Range, initialState, defaultBullet, defaultSettings) where

import Halogen.Themes.Bootstrap4
import Control.Bind (discard)
import Control.MonadZero (guard)
import Data.Array ((..))
import Data.Function (($))
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.SVG (Anchor(..), RGB(..), Translate(..), dy, fill, g, height, line, rect, rgb, stroke, strokeWidth, svg, svgText, textAnchor, transform, width, x, x1, x2, y, y1, y2)
import Halogen.SVG as SVG
import Prelude (bind, mod, negate, pure, show, (*), (+), (-), (/), (<$>), (<>), (==), (>=))

type Slot = H.Slot Query Message

data Query a = IsOn (Boolean -> a)

data Message = Toggled Boolean

data Action = SetResults Results

data Results = R

data Question

data Input = InBullet (Maybe Bullet)

data QType c = Scale | Binary | Category c

type Range = { start :: Int, end :: Int }

type Scale = { scaleLabel :: Int, x :: Number }

resultToBullet :: Results -> Bullet
resultToBullet _ = defaultBullet

defaultBullet :: Bullet
defaultBullet =
  { caption : "Purpose"
  , subCap : "What's your why?"
  , scale : [0.1, 0.65, 0.90, 1.0]
  , range : { start: 0, end: 25 }
  , tickSize : 5
  , target : 0.65
  , attainment : 0.75
  }

type Bullet
  = { caption :: String
    , subCap :: String
    , scale :: Array Number -- 0 - 1
    , range :: Range
    , tickSize :: Int
    , target :: Number -- 0 - 1
    , attainment :: Number -- 0 -1
    }

attainmentMet :: Bullet -> Boolean
attainmentMet b = b.attainment >= b.target

type BulletSettings
  = { bulletHeight :: Int
    , bulletWidth :: Int
    , bulletMargin :: Int
    , labelWidth :: Number
    , scaleHeight :: Number
    -- , scale :: Array Number
    }

mapRange :: Range -> Number -> Int
mapRange { start, end } n =
  let s = toNumber start
      e = toNumber end
  in round $ (s + (e-s) * n)

mkScale :: Bullet -> Array (Scale)
mkScale {tickSize, range } = do
  n <- range.start..range.end
  guard $ 0 == mod n tickSize
  pure { scaleLabel: n, x: toNumber n/toNumber range.end }


defaultSettings :: BulletSettings
defaultSettings =
  { bulletHeight : 50
  , bulletWidth : 500
  , bulletMargin : 5
  , scaleHeight : 0.5
  , labelWidth : 0.25
  }

type State
  = {
      bullet :: Maybe Bullet
    , settings :: BulletSettings
    }


initialState :: Input -> State
initialState (InBullet b) = { bullet : b, settings : defaultSettings }

ui :: forall q o m. H.Component HH.HTML q Input o m
ui =
  H.mkComponent
    { initialState
    , render
    -- , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    , eval: H.mkEval H.defaultEval
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  HH.div [HP.classes [HH.ClassName "bullet-container", col6]]
    [
      case state.bullet of
        Nothing -> HH.h1 [] [HH.text "Loading.."]
        Just b ->
          let
            attainmentCls = "attainment " <> if attainmentMet b then "met" else "missed"
            tgtX = scaleToBullet b.target
            renderScale { scaleLabel, x } =
              let scaleX = scaleToBullet x
              in g [transform $ Translate {x: scaleX, y: scaleHeightPx}, SVG.class_ "ticks"]
                   [ line [x1 0, x2 0, y1 0, y2 4, strokeWidth 1 ] []
                   , svgText [textAnchor Middle, dy "1em", transform $ Translate {x: 0, y: 5}] (show scaleLabel)
                   ]
          in
            svg [ width (bulletWidth+(2*bulletMargin))
                , height (bulletHeight+(2*bulletMargin))
                , SVG.class_ "bullet"] -- [HP.class_ $ H.ClassName "bullet"]
              [
                -- scale and lines
                g [(transform  $ Translate {x: startX, y: 0}), SVG.class_ "scale"]
                  (
                    (mkRange <$> b.scale)
                     <>
                    [ line [ x1 0, y1 lineY, x2 (scaleToBullet b.attainment), y2 lineY
                           , SVG.class_ attainmentCls
                           , strokeWidth sw
                           ] []
                    , line [ x1 tgtX, x2 tgtX, y1 tgtY1, y2 tgtY2
                           , SVG.class_ "target"
                           , SVG.strokeWidth 5
                           ] []
                    , g [(transform  $ Translate {x: -6, y: 12}), SVG.class_ "label"]
                        [
                          svgText [SVG.class_ "title", textAnchor End] b.caption
                        , svgText [SVG.class_ "subtitle", dy "1.5em", textAnchor End] b.subCap
                        ]
                    ]
                    <>
                    (renderScale <$> mkScale b )
                  )
              ]

  ]
  where
    { bulletWidth, bulletHeight, bulletMargin, labelWidth, scaleHeight } = state.settings
    targetHeight = scaleHeight * 0.75
    scaleHeightPx = round (scaleHeight * toNumber bulletHeight)
    attainmentHeight = scaleHeight * 0.33
    startX = round $ labelWidth * (toNumber bulletWidth)
    bulletCanvasWidth = toNumber $ bulletWidth - startX - bulletMargin
    tgtY1 = round $ toNumber bulletHeight * ((scaleHeight - targetHeight)/2.0)
    tgtY2 = tgtY1 + (round $ toNumber bulletHeight * targetHeight)
    lineY = round $ toNumber bulletHeight * (scaleHeight/2.0)
    scaleToBullet i = (round (i * bulletCanvasWidth))
    sw = round $ attainmentHeight * (toNumber bulletHeight)
    mkRange i =
      let w = (round (i * bulletCanvasWidth))
      in rect [ height scaleHeightPx
              , width w
              , x 0
              , y 0
              ] []


newtype DashState
  = DashState
  { results :: Results
  }

scaleGray = rgb 131 133 144

cdoDevGreen :: RGB
cdoDevGreen = rgb 8 123 56

darkGreen :: RGB
darkGreen = rgb 3 10  7
