module Main where

import Json.Decode as D exposing (..)

import Signal exposing (Signal, mailbox, message, (<~), foldp)
import String exposing (contains, isEmpty)
import Html exposing (..)
import Html.Attributes as Html exposing (value, selected, type')
import Html.Events exposing (on, targetValue, onClick, onBlur)
import List exposing (map, filter)
import Date exposing (Date, fromString)
import Debug exposing (log, watch, watchSummary)

main : Signal Html
main = (view updates.address) <~ model

model : Signal Model
model = foldp update model0 updates.signal

model0 : Model
model0 =
  let getDate str = case Date.fromString str of
    Ok date -> date
    Err err -> Date.fromTime 0.0
  in
    { q = ""
    , list = []
    , list0 =
        [ Person 1 "aaa" Male   (getDate "2000/5/1")
        , Person 2 "bbb" Female (getDate "1988/2/5")
        , Person 3 "ccc" Male   (getDate "1992/12/30")
        ]
    }

update : Action -> Model -> Model
update action m =
  case action of
    Search n ->
      { m | q <- n
          , list0 <- if n == ""
                     then m.list0
                     else filter (\p -> contains n p.name) m.list0 }

    Del n ->
      { m | list0 <- filter (\p -> p.id /= n) m.list0 }

    UpdateGendar n v ->
      let dum = watch "(n,v) = " (n, v)
          updateGendar i = if i.id == n
                         then { i | gendar <- if v == "Male" then Male else Female }
                         else i
      in
         { m | list0 <- List.map updateGendar m.list0 }

    UpdateName n v ->
      let dum = watch "(n, v) = " (n, v)
          updateName i = if i.id == n
                       then { i | name <- v }
                       else i
      in
         { m | list0 <- List.map updateName m.list0 }

    otherwise ->
      { m | q <- toString action }

{-- testing code...  --}
type alias XX =
  { name : String
  , age : Int
  , g : Gendar
  }

parseGendar : String -> Gendar
parseGendar str =
  case str of
    "Male" -> Male
    "Female" -> Female
{-- --}

view : Signal.Address Action -> Model -> Html
view addr model =
  let dum = Debug.watch "model: " model
      json = Debug.log "xx:" <| decodeString
        (object3 XX
          ("name" := string)
          ("age" := int)
          ("g" := object1 parseGendar string))
          "{ \"name\": \"nnn\", \"age\": 33, \"g\": \"Male\" }"
  in
    div [] 
      [ label []
          [ text "search: "
          , input
            [ Html.value model.q
            , on "input" targetValue (\v -> message addr (Search v))
            ] []
          ]
      , renderList addr model.list0
      ]

renderList : Signal.Address Action -> List Person -> Html
renderList addr list = 
  table []
    [ thead []
        [ tr []
            [ th [] [text "name"]
            , th [] [text "gendar"]
            , th [] [text "BD"]
            ]
        ]
    , tbody [] (List.map (renderPerson addr) list)
    ]

renderPerson : Signal.Address Action -> Person -> Html
renderPerson addr p =
  tr []
    [ td []
        [ input
            [ Html.value p.name
            , on "input" targetValue (\v -> message addr (UpdateName p.id v))
            ] []
        ]
    , td []
        [ select
            [ Html.value <| toString p.gendar
            , on "change" targetValue (\v -> message addr (UpdateGendar p.id v))
            ]
            (List.map (\v -> option [ selected <| p.gendar == v ] [ text <| toString v ]) [Male, Female])
            -- [ option [ selected <| p.gendar == Male   ] [ text "Male"]
            -- , option [ selected <| p.gendar == Female ] [ text "Female"]
            -- ]
        ]
    , td []
        [ input
            [ Html.value (date2string p.birdthday)
            , type' "date"
            , on "input" targetValue (\v -> message addr (UpdateName p.id v))
            ] []
        ]
    , button
        [ onClick addr (Del p.id) ]
        [ text "x" ]
    ]

date2string : Date -> String
date2string date =
  let y = toString <| Date.year date
      m = toString <| Date.month date
      d = toString <| Date.day date
  in
     y ++ " " ++ m ++ " " ++ d

type Action
  = Nop
  | Up
  | Down
  | Del Id
  | UpdateName Id String
  | UpdateGendar Id String
  | Search String

updates : Signal.Mailbox Action
updates = Signal.mailbox Nop


-- Data Model

type alias Id = Int

type Gendar
  = Male
  | Female

type alias Person =
  { id : Id
  , name : String
  , gendar : Gendar
  , birdthday : Date
  }

type alias Model =
  { q : String
  , list : List Person
  , list0 : List Person
  }

