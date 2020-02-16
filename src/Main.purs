module Main where

import Prelude

import Dash.Bullet (ui)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

-- import Dash.View

-- mkMainFrame ::
--   forall m n.
--   MonadAff m =>
--   MonadEffect n =>
--   n (Component HTML HQuery Unit Message m)
-- mkMainFrame = do
--   pure
--     $ H.mkComponent
--         { initialState: const initialState
--         , render
--         , eval:
--           H.mkEval
--             { handleQuery
--             , handleAction: handleActionWithAnalyticsTracking
--             , receive: const Nothing
--             , initialize: Just $ CheckAuthStatus
--             , finalize: Nothing
--             }
--         }

main :: Effect Unit
main = do
  log "Main"
  runHalogenAff do
    body <- awaitBody
    log "after body"
    runUI ui {} body
    -- forkAff $ runProcess watchLocalStorageProcess
  -- log "🍝"

onLoad :: Unit
onLoad = unsafePerformEffect main
