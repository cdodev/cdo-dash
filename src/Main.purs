module Main where

import Halogen.Themes.Bootstrap4
import Prelude

import Dash.Bullet as Bullet
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)


type State =
  { purpose :: Bullet.State
  , strategy :: Bullet.State
  }

type ChildSlots =
  ( purpose :: Bullet.Slot Unit
  , strategy :: Bullet.Slot Unit
  )

_purpose :: SProxy "purpose"
_purpose = SProxy


_strategy :: SProxy "strategy"
_strategy = SProxy


component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
    }

initialState :: forall i. i -> State
initialState i =
  let stratBullet =
        Bullet.defaultBullet
            { caption = "Strategy"
            , subCap = "How will you get there?"
            , attainment = 0.55
            , scale = [0.25, 0.5, 0.75, 1.0]
            }
  in
    { purpose: Bullet.initialState (Bullet.InBullet $ Just Bullet.defaultBullet)
    , strategy: { bullet: Just stratBullet, settings: Bullet.defaultSettings }
    }


render :: forall m. State -> H.ComponentHTML Unit ChildSlots m
render state =
  HH.div [HP.class_ container]
    [HH.div [HP.class_ row]
      [ HH.slot _purpose unit Bullet.ui (Bullet.InBullet state.purpose.bullet) (const Nothing)
      , HH.slot _strategy unit Bullet.ui (Bullet.InBullet state.strategy.bullet) (const Nothing)
      ]

    ]

main :: Effect Unit
main = do
  log "Main"
  runHalogenAff do
    body <- awaitBody
    log "after body"
    runUI component {} body
    -- forkAff $ runProcess watchLocalStorageProcess
  -- log "🍝"

onLoad :: Unit
onLoad = unsafePerformEffect main
