module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, textarea, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

type alias Status =
  { horizontalPosition : Int
  , depth : Int
  , aim: Int
  }

type Command = Forward Int | Down Int | Up Int

toCommand : String -> Command
toCommand str =
  case (String.split " " str) of
    "forward" :: v :: [] -> Forward (Maybe.withDefault 0 (String.toInt v))
    "down" :: v :: []    -> Down (Maybe.withDefault 0 (String.toInt v))
    "up" :: v :: []      -> Up (Maybe.withDefault 0 (String.toInt v))
    _                    -> Down 0

toCommands : String -> List Command
toCommands str =
  List.map toCommand (String.split "\n" str)

apply1 : Status -> List Command -> Status
apply1 status commands =
  case commands of
    []                -> status
    Forward x :: rest -> apply1 (Status (status.horizontalPosition + x) status.depth status.aim) rest
    Down x :: rest    -> apply1 (Status status.horizontalPosition (status.depth + x) status.aim) rest
    Up x :: rest      -> apply1 (Status status.horizontalPosition (status.depth - x) status.aim) rest

apply2 : Status -> List Command -> Status
apply2 status commands =
  case commands of
    []                -> status
    Forward x :: rest -> apply2 (Status (status.horizontalPosition + x) (status.depth + status.aim * x) status.aim) rest
    Down x :: rest    -> apply2 (Status status.horizontalPosition status.depth (status.aim + x)) rest
    Up x :: rest      -> apply2 (Status status.horizontalPosition status.depth (status.aim - x)) rest

mult : Status -> Int
mult status = status.horizontalPosition * status.depth

initialStatus : Status
initialStatus = Status 0 0 0

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model =
  { content : String
  }

init : Model
init =
  { content = "" }

type Msg
  = Change String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }

view : Model -> Html Msg
view model =
  div []
    [ textarea [ id "input", placeholder "input", value model.content, onInput Change ] []
    , div [ id "part1" ] [ text ("Part 1: " ++ (String.fromInt (mult (apply1 initialStatus (toCommands model.content))))) ]
    , div [ id "part2" ] [ text ("Part 2: " ++ (String.fromInt (mult (apply2 initialStatus (toCommands model.content))))) ]
    ]
