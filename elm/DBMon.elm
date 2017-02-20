module DBMon exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy)
import Random exposing (Generator)
import Dict exposing (Dict)
import Time exposing (Time)
import Task
import Process


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , init = ( initialModel, Task.perform LoadSamples Time.now )
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { databases : Dict String Database }


type alias Database =
    { samples : List Sample
    , name : String
    }


type alias Sample =
    { queries : List Query
    , time : Time
    }


type alias Query =
    { elapsed : Float
    , query : String
    , canvas_action : Maybe ()
    , canvas_context_id : Maybe ()
    , canvas_controller : Maybe ()
    , canvas_hostname : Maybe ()
    , canvas_job_tag : Maybe ()
    , canvas_pid : Maybe ()
    , elapsed : Float
    , query : String
    , waiting : Bool
    }


type Msg
    = LoadSamples Time


initialModel : Model
initialModel =
    { databases = Dict.empty }


update : Msg -> Model -> ( Model, Cmd Msg )
update (LoadSamples time) model =
    ( { model | databases = loadSamples time model.databases }
    , Process.sleep timeout
        |> Task.andThen (\_ -> Time.now)
        |> Task.perform LoadSamples
    )


viewQuery : String -> Float -> Html msg
viewQuery query elapsed =
    let
        className =
            if elapsed >= 10 then
                "elapsed warn_long"
            else if elapsed >= 1 then
                "elapsed warn"
            else
                "elapsed short"

        caption =
            if elapsed > 0 then
                formatElapsed elapsed
            else
                ""
    in
        td [ class ("Query " ++ className) ]
            [ text caption
            , div [ class "popover left" ]
                [ div [ class "popover-content" ] [ text query ]
                , div [ class "arrow" ] []
                ]
            ]


formatElapsed : Float -> String
formatElapsed value =
    if value > 60 then
        let
            minutes =
                value / 60

            comps =
                (toFloat ((floor value) % 60) / 100)
                    |> toString
                    |> String.split "."

            { seconds, ms } =
                case comps of
                    sec :: msec :: _ ->
                        { seconds = String.padLeft 2 '0' sec
                        , ms = msec
                        }

                    _ ->
                        { seconds = "", ms = "" }
        in
            String.join ""
                [ toString minutes, ":", toString seconds, ".", toString ms ]
    else
        toString (toFloat (floor value) / 100)


sample : List Query -> Html msg
sample queries =
    let
        queryCount =
            List.length queries

        countClassName =
            if queryCount >= 20 then
                "label label-important"
            else if queryCount >= 10 then
                "label label-warning"
            else
                "label label-success"
    in
        td [ class "query-count" ]
            (span [ class countClassName ] [ text (toString (List.length queries)) ]
                :: sampleHelp 5 queries []
            )


sampleHelp :
    Int
    -> List Query
    -> List (Html msg)
    -> List (Html msg)
sampleHelp remaining queries results =
    if remaining > 0 then
        case queries of
            [] ->
                (viewQuery "" 0 :: results)
                    |> sampleHelp (remaining - 1) []

            query :: rest ->
                viewQuery query.query query.elapsed
                    :: results
                    |> sampleHelp (remaining - 1) rest
    else
        results


getLastSample : List a -> Maybe a
getLastSample samples =
    case samples of
        [] ->
            Nothing

        last :: [] ->
            Just last

        _ :: rest ->
            getLastSample rest


viewDatabase : Database -> Html msg
viewDatabase database =
    case getLastSample database.samples of
        Just lastSample ->
            tr []
                [ td [ class "dbname" ] [ text database.name ]
                , lazy sample lastSample.queries
                ]

        Nothing ->
            Html.text ""


view : Model -> Html msg
view model =
    div []
        [ table [ class "table table-striped latest-data" ]
            [ tbody [] (List.map viewDatabase (Dict.values model.databases)) ]
        ]


rows : Int
rows =
    100


timeout : Time
timeout =
    0


queryGenerator : Generator Query
queryGenerator =
    Random.map3 makeQuery
        (Random.float 0 15)
        (Random.map ((==) 0) (Random.int 0 1))
        queryStringGenerator


queryStringGenerator : Generator String
queryStringGenerator =
    Random.int 0 100
        |> Random.map
            (\int ->
                if int < 10 then
                    "vacuum"
                else if int < 22 then
                    "<IDLE> in transaction"
                else
                    "SELECT blah FROM something"
            )


makeQuery : Float -> Bool -> String -> Query
makeQuery elapsed isWaiting queryString =
    { canvas_action = Nothing
    , canvas_context_id = Nothing
    , canvas_controller = Nothing
    , canvas_hostname = Nothing
    , canvas_job_tag = Nothing
    , canvas_pid = Nothing
    , elapsed = elapsed
    , query = queryString
    , waiting = isWaiting
    }


getRandomQueries : Generator (List Query)
getRandomQueries =
    Random.int 1 11
        |> Random.andThen generateQueryList


generateQueryList : Int -> Generator (List Query)
generateQueryList length =
    queryGenerator
        |> Random.list length
        |> Random.map (List.sortBy .elapsed)


getData : Time -> Dict String Sample
getData time =
    let
        emptySample =
            { queries = []
            , time = time
            }

        addSamples index samples =
            if index > 0 then
                samples
                    |> Dict.insert ("cluster" ++ toString index) emptySample
                    |> Dict.insert ("cluster" ++ toString index ++ "slave") emptySample
                    |> addSamples (index - 1)
            else
                samples
    in
        -- TODO incorporate randomness
        addSamples 100 Dict.empty


loadSamples : Time -> Dict String Database -> Dict String Database
loadSamples time databases =
    getData time
        |> Dict.map (thingdo databases time)


thingdo :
    Dict String Database
    -> Time
    -> String
    -> Sample
    -> Database
thingdo databases startAt dbname sampleInfo =
    let
        samples =
            databases
                |> Dict.get dbname
                |> Maybe.map .samples
                |> Maybe.withDefault []

        newSamples =
            (samples ++ [ { time = startAt, queries = sampleInfo.queries } ])
                |> List.take 5
    in
        { samples = newSamples
        , name = dbname
        }
