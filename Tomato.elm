module Tomato where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Signal exposing (Signal, Address)
import Time
import String

import TomatoUtils as Utils

-- MODEL

type alias Model = 
  {
    tasks: List Task,
    taskNameInput: String,
    nextID: Int,
    timer: Timer,
    history: List String
  }

type alias Timer = 
  {
    timerType: String,
    started: Bool,
    secondsLeft: Int
  }

type alias Task = 
  { 
    id: Int,
    isActive: Bool,
    name: String,
    finishedTomatoes: List Tomato
  }


type alias Tomato =
  {
  }

initialModel : Model
initialModel =
  let
    emptyModel =
      {
        tasks = [],
        taskNameInput = "",
        nextID = 1,
        timer = newTimer "Work",
        history = []
      }
  in Maybe.withDefault emptyModel getStoredModel

timerTypeSecondsMap : String -> Int
timerTypeSecondsMap timerType =
    case timerType of
      "Work" -> 60 * 25
      "ShortBreak" -> 60 * 5
      "LongBreak" -> 60 * 20
      _ -> 0

newTimer : String -> Timer 
newTimer timerType = 
  { timerType = timerType, 
    started = False, 
    secondsLeft = timerTypeSecondsMap timerType }


newTask : String -> Int -> Task
newTask name id =
  {
    id = id,
    isActive = False,
    name = name,
    finishedTomatoes = []
  }

newTomato : Tomato 
newTomato = 
  {
  }

-- UPDATE

type Action 
  = NoOp
  | Start Int Bool
  | TaskNameInputUpdate String
  | AddTask
  | Delete Int
  | Tick
  | Break String
  | Stop


-- How do we update our model for the give action
update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model

    AddTask -> 
      { model |
        nextID = model.nextID + 1,
        taskNameInput = "",
        tasks = 
          if String.isEmpty model.taskNameInput 
            then model.tasks
            else (newTask model.taskNameInput model.nextID) :: model.tasks
      }

    TaskNameInputUpdate name ->
      { model | taskNameInput = name }

    Delete id -> 
      { model | tasks = List.filter (\t -> t.id /= id) model.tasks}

    Tick ->
      model
        |> timerUpdate
        |> tomatoUpdate

    Start id isActive -> 
      let 
        updateTask t = 
          if t.id == id 
            then { t | isActive = isActive } 
            else { t | isActive = False }

        newWorkTimer =
          newTimer "Work"

        updateTimer timer =
          if isActive
            then 
              { newWorkTimer | started = isActive }
            else
              newWorkTimer

        historyMessage isActive = 
          if isActive then "Started Task" else "Stopped Task"
      in
        { model | 
          tasks = List.map updateTask model.tasks, 
          timer = updateTimer model.timer,
          history = historyMessage isActive :: model.history }

    Break breakType ->
      let 
        updateTasks t = { t | isActive = False }
        newShortBreakTimer = newTimer breakType
      in
        { model | 
          tasks = List.map updateTasks model.tasks, 
          timer = { newShortBreakTimer | started = True }}

    Stop ->
      let
        updateTasks t = { t | isActive = False }
        stopTimer timer =
          { timer | started = False }
      in
        { model | tasks = List.map updateTasks model.tasks, timer = stopTimer model.timer }    


timerUpdate : Model -> Model
timerUpdate model =
  let
    updateTimer timer =
      if timer.started == False
        then timer
        else 
          if timer.secondsLeft > 0
            then { timer | secondsLeft = timer.secondsLeft - 1 }
            else { timer | started = False }
  in 
    { model | timer = updateTimer model.timer }

tomatoUpdate : Model -> Model
tomatoUpdate model =
  let 
    isTimerFinished timer = 
      timer.started == True && timer.secondsLeft <= 0 && timer.timerType == "Work"

    updateTask timer task =
      if task.isActive == True && isTimerFinished timer
        then { task | finishedTomatoes = newTomato :: task.finishedTomatoes, isActive = False }
        else task
  in 
    { model | tasks = List.map (updateTask model.timer) model.tasks  }



-- VIEW

view : Address Action -> Model -> Html
view address model =
  div 
    [ class "content" ]
    [
      section
      [ id "promatoapp" ]
      [
        lazy2 taskEntry address model
        , lazy2 taskList address model.tasks
      ]
    ]


taskEntry : Address Action -> Model -> Html
taskEntry address model =
  header
    [ id "header" ]
    [ h1 [] [ text "Promato" ]
    , div 
      [ id "timer" ]
      [ text (prettySeconds model.timer.secondsLeft) ]
    , div
      [ id "controls" ]
      [ button
        [ class "btn",
          onClick address (Break "ShortBreak")
        ]
        [ text "Short Break" ]
      , button
        [ class "btn",
          onClick address (Break "LongBreak")
        ]
        [ text "Long Break" ]
      , button
        [ class "btn" ,
          onClick address Stop
        ]
        [ text "Stop"]
      ]
    , input
      [ id "new-task"
      , placeholder "What's your task name?"
      , autofocus True
      , value model.taskNameInput
      , name "newTask"
      , on "input" targetValue (Signal.message address << TaskNameInputUpdate)
      , Utils.onEnter address AddTask
      ]
      []
    ]

taskList : Address Action -> List Task -> Html
taskList address tasks =
  section
    [ id "main" ]
    [ ul 
      [ id "task-list" ]
      (List.map (taskItem address) tasks)
    ]


taskItem : Address Action -> Task -> Html
taskItem address task =
  li
    [ classList [ ("active", task.isActive) ]
    ]
    [ div
        [ class "view" ]
        [ input
              [ class "start-task"
              , type' "checkbox"
              , checked task.isActive
              , onClick address (Start task.id (not task.isActive))
              ]
              []
          , label 
          [ ]
          [ text task.name ]
          , div
            [ class "tomatos" ]
            (List.map tomatoItem task.finishedTomatoes)
          , button 
            [ class "destroy"
            , onClick address (Delete task.id)
            ]
            []
        ] 
    ]

tomatoItem : Tomato -> Html
tomatoItem _ =
  img
    [ src "Tomato-icon.png", width 35 ]
    []

prettySeconds : Int -> String
prettySeconds seconds = 
  let
    totalMinutes = truncate ((toFloat seconds) / 60)
    secondsRemainder = seconds - (totalMinutes * 60)
    secondsString = 
        if secondsRemainder < 10
          then "0" ++ (toString secondsRemainder)
          else (toString secondsRemainder)
  in
    (toString totalMinutes) ++ ":" ++ secondsString

-- PORTS

port modelChanges : Signal Model
port modelChanges =
  model

port getStoredModel : Maybe Model

port setStoredModel : Signal Model
port setStoredModel =
  model

-- SIGNALS

ticker : Signal Action
ticker =
  Signal.map (always Tick) (Time.every Time.second)

inbox : Signal.Mailbox Action
inbox =
  Signal.mailbox NoOp
        
signals : Signal Action
signals =
  Signal.mergeMany [inbox.signal, ticker]

model : Signal Model
model =
  Signal.foldp update initialModel signals

main : Signal Html
main =
  Signal.map (view inbox.address) model

