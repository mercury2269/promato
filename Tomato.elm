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
    nextID: Int
  }

type alias Timer = 
  {
    timerType: TimerType,
    started: Bool,
    secondsLeft: Int
  }

type alias Task = 
  { 
    id: Int,
    isActive: Bool,
    name: String,
    finishedTomatoes: Int,
    timer: Timer
  }


type TimerType = Work | Break | LongBreak

initialModel : Model
initialModel =
  {
    tasks = [],
    taskNameInput = "",
    nextID = 1
  }

newTask : String -> Int -> Task
newTask name id =
  {
    id = id,
    isActive = True,
    name = name,
    finishedTomatoes = 0,
    timer = 
      { 
        timerType = Work,
        started = False,
        secondsLeft = 60 * 25
      }
  }

-- UPDATE

type Action 
  = NoOp
  | Start
  | Stop
  | Reset
  | TaskNameInputUpdate String
  | AddTask
  | ToggleActive Int
  | Delete Int
  | Tick


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

    ToggleActive id -> 
      let
        updateTask e = 
          if e.id == id then { e | isActive = (not e.isActive) } else e
      in
      { model | tasks = List.map updateTask model.tasks }

    Tick ->
      model
        |> timerUpdate
        |> tomatoUpdate

    Start -> 
      model

    Stop -> 
      model

    Reset -> 
      model


timerUpdate : Model -> Model
timerUpdate model =
  let
    tickDown timer =
      { timer | secondsLeft = timer.secondsLeft - 1 }
    updateTask task =
      if task.isActive == True && task.timer.started == True && task.timer.secondsLeft > 0
        then { task | timer = tickDown task.timer }
        else task 
  in 
    { model | tasks = List.map updateTask model.tasks }

tomatoUpdate : Model -> Model
tomatoUpdate model =
  let 
    stop timer =
      { timer | started = False}
    updateTask t =
      if t.isActive == True && t.timer.started == True && t.timer.secondsLeft <= 0
        then 
          if t.timer.timerType == Work
            then { t | timer = stop t.timer, finishedTomatoes = t.finishedTomatoes + 1 }
            else { t | timer = stop t.timer }
        else t
  in 
    { model | tasks = List.map updateTask model.tasks }




-- VIEW

view : Address Action -> Model -> Html
view address model =
  div 
    [ class "content" ]
    [
      section
      [ id "promatoapp" ]
      [
        lazy2 taskEntry address model.taskNameInput
        , lazy2 taskList address model.tasks
      ]
    ]


taskEntry : Address Action -> String -> Html
taskEntry address taskName =
  header
    [ id "header" ]
    [ h1 [] [ text "Promato" ]
    , input
      [ id "new-task"
      , placeholder "What's your task name?"
      , autofocus True
      , value taskName
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
    []
    [ div
        [ class "view" ]
        [ label 
          [ ]
          [ text task.name ]
          , button 
            [ class "destroy"
            , onClick address (Delete task.id)
            ]
            []
        ] 
    ]


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

