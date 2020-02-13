module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Halogen.Chartist
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Halogen (Component, ComponentHTML)
import Halogen.HTML (ClassName(ClassName), HTML, a, button, code_, div, div_, h1, pre, slot, strong_, text)

import Dash.Types
import Dash.View

mkMainFrame ::
  forall m n.
  MonadAff m =>
  MonadEffect n =>
  n (Component HTML HQuery Unit Message m)
mkMainFrame = do
  let
    initialState = mkInitialState editorPreferences
  pure
    $ H.mkComponent
        { initialState: const initialState
        , render
        , eval:
          H.mkEval
            { handleQuery
            , handleAction: handleActionWithAnalyticsTracking
            , receive: const Nothing
            , initialize: Just $ CheckAuthStatus
            , finalize: Nothing
            }
        }

main :: Effect Unit
main = do
  mainFrame <- mkMainFrame
  runHalogenAff do
    body <- awaitBody
    driver <- runUI mainFrame Mounted body
    forkAff $ runProcess watchLocalStorageProcess
  log "🍝"
