import Task exposing (Task)
import Html exposing (..)
import Json.Decode
import Json.Encode
import Dict exposing (Dict)
import ElmFire
import ElmFire.Dict
import ElmFire.Op

-- NOTE: The value at this path or subpath must be a dict of string -> string
url = "https://elmfire-extra-hello-world.firebaseio-demo.com"
-- url = "https://elmfire-extra-hello-world.firebaseio.com/my/sub/path"

config = {
    location = ElmFire.fromUrl url,
    orderOptions = ElmFire.noOrder,
    encoder = Json.Encode.string,
    decoder = Json.Decode.string
  }

-- mirror the firebase data through a task and signal of a dict
(task, serverValue) = ElmFire.Dict.mirror config

-- you have to run the task so that messages from firebase come through the serverValue signal
port runTask : Task ElmFire.Error (Task ElmFire.Error ())
port runTask = task

type alias Model = String
type alias FirebaseModel = Dict String String

startModel = "Waiting..."

model : Signal Model
model =
  -- in a real app this foldp would likely be more complicated, using Signal.merge
  -- to combine signals such as user actions.
  -- Signal.map converts a Signal FirebaseModel -> Signal Action
  Signal.foldp update startModel (Signal.map ServerUpdate serverValue)

-- in a real app, all actions including user actions could be modeled by this Action
-- type that gets passed to the update function
type Action =
  NoOp
  | ServerUpdate FirebaseModel

update : Action -> Model -> Model
update action model =
  let
    -- dictionary reducer function; concatenate key-value list
    reduceKeyValue key value prev =
      case prev of
        "" -> key ++ ": " ++ value
        _ -> prev ++ ", " ++ key ++ ": " ++ value
  in
    case action of
      NoOp -> model
      ServerUpdate dict -> Dict.foldl reduceKeyValue "" dict

view : Model -> Html
view value =
  h1 [] [ text value ]

main : Signal Html
main =
  Signal.map view model
