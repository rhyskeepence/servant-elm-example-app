module Events exposing (onChange, onEnter, onSubmitPreventDefault)

{-| Extensions to the Html.Events library.

@docs onEnter
-}

import Html exposing (Attribute)
import Html.Events exposing (..)
import Json.Decode as Json


onEnter : (() -> a) -> Attribute a
onEnter tagger =
  on
    "keydown"
    (Json.map tagger (Json.customDecoder keyCode is13))


is13 : Int -> Result String ()
is13 code =
  if code == 13 then
    Ok ()
  else
    Err "not the right key code"


onChange : (String -> a) -> Html.Attribute a
onChange f =
  on "input" (Json.map f targetValue)


onSubmitPreventDefault message =
  onWithOptions
    "submit"
    { defaultOptions | preventDefault = True }
    (Json.succeed message)
