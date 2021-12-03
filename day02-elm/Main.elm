port module Main exposing (main)

import Platform exposing (Program)

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

type alias InputType = String
type alias OutputType = String

port get : (InputType -> msg) -> Sub msg
port put : OutputType -> Cmd msg

main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }

type alias Model = ()
type Msg = Input String
type alias Flags = ()


init : Flags -> ( Model, Cmd Msg )
init _ = ( (), Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input -> ( model, put (run input))

subscriptions : Model -> Sub Msg
subscriptions _ =
    get Input

run : InputType -> OutputType
run input =
  let
    commands = toCommands input
    part1 = (mult (apply1 initialStatus commands))
    part2 = (mult (apply2 initialStatus commands))
  in
    "Part 1: " ++ (String.fromInt part1) ++ "\nPart 2: " ++ (String.fromInt part2)
