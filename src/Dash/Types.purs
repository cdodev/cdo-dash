module Dash.Types (Slot, Query, Message, Action, State) where

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Slot = H.Slot Query Message

data Query a = IsOn (Boolean -> a)

data Message = Toggled Boolean

data Action = SetResults Results

type State = { enabled :: Boolean }

data Results

data Question

data QType = Scale | Binary | Category c

data DashQuery a
  = ReceiveResults Results a

newtype DashState
  = DashState
  { results :: Results
  }
