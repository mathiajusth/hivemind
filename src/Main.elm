module Main exposing (..)

import Browser as Browser
import Data.Product as Product
import Data.Review as Review exposing (Review)
import Date exposing (Date)
import Element as E
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import GenericDict as Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as Decode
import Library.Id as Id
import Library.Time.Extra as Time
import Library.Validate as Validate exposing (Validation)
import List.Extra as List
import Maybe.Extra as Maybe
import Result.Extra as Result
import Return exposing (Return)
import Task
import Time
import Time.Extra as Time
import Ui.DatePicker as DatePicker
import Ui.Input as Input



-- MODEL


type alias Model =
    { reviewsFile :
        Maybe
            { name : String
            , content : Result Decode.Error (Dict Product.Id (List Review))
            }
    , zone : Maybe Time.Zone
    , query : QueryForm
    }


init : () -> Return Msg Model
init _ =
    Return.return
        { reviewsFile = Nothing
        , zone = Nothing
        , query =
            { start = DatePicker.initWithoutToday
            , end = DatePicker.initWithoutToday
            , limit = ""
            , minNumberReviews = ""
            }
        }
        (Task.perform ZoneRecieved Time.here)
        |> Return.command (Task.perform TodayRecieved Date.today)



---- Query validation


type alias QueryForm =
    { start : DatePicker.Model
    , end : DatePicker.Model
    , limit : String
    , minNumberReviews : String
    }


type alias Query =
    { start : Time.Posix
    , end : Time.Posix
    , limit : Int
    , minNumberReviews : Int
    }


validateQuery : Time.Zone -> Validate.RecordValidation String QueryForm Query
validateQuery usersZone =
    Validate.record Query
        |> Validate.field .start
            (Validate.lift DatePicker.getDate
                |> Validate.compose Validate.fromMaybe
                |> Validate.compose
                    (Validate.lift (Time.fromDate Time.StartOfDay usersZone))
            )
        |> Validate.field .end
            (Validate.lift DatePicker.getDate
                |> Validate.compose Validate.fromMaybe
                |> Validate.compose
                    (Validate.lift (Time.fromDate Time.EndOfDay usersZone))
            )
        |> Validate.field .limit Validate.toInt
        |> Validate.field .minNumberReviews Validate.toInt
        |> Validate.endRecord



-- UPDATE


type Msg
    = UploadFileClicked
    | FileUploaded File
    | FileExtracted String String
    | RemoveFileClicked
    | ZoneRecieved Time.Zone
    | TodayRecieved Date
    | FormMsg FormMsg


type FormMsg
    = StartDatePickerMsg DatePicker.Msg
    | EndDatePickerMsg DatePicker.Msg
    | LimitChanged String
    | MinNumberReviewsChanged String


update : Msg -> Model -> Return Msg Model
update msg ({ query } as model) =
    case msg of
        ZoneRecieved zone ->
            { model | zone = Just zone }
                |> Return.singleton

        TodayRecieved today ->
            { model
                | query =
                    { query
                        | start =
                            query.start
                                |> DatePicker.setToday today
                        , end =
                            query.end
                                |> DatePicker.setToday today
                    }
            }
                |> Return.singleton

        UploadFileClicked ->
            Return.return model
                (Select.file [ "application/x-ndjson" ] FileUploaded)

        FileUploaded file ->
            Return.return model
                (Task.perform (FileExtracted <| File.name file) (File.toString file))

        FileExtracted fileName ndJsonString ->
            { model
                | reviewsFile =
                    Just
                        { name = fileName
                        , content =
                            ndJsonString
                                |> String.lines
                                |> List.map (Decode.decodeString Review.decoder)
                                |> Result.combine
                                |> Result.map
                                    (List.gatherEqualsBy .productID
                                        >> List.map
                                            (\( first, others ) ->
                                                ( first.productID, first :: others )
                                            )
                                        >> Dict.fromList Id.toString
                                    )
                        }
            }
                |> Return.singleton

        RemoveFileClicked ->
            { model | reviewsFile = Nothing }
                |> Return.singleton

        FormMsg formMsg ->
            case formMsg of
                StartDatePickerMsg datePickerMsg ->
                    DatePicker.update datePickerMsg query.start
                        |> Return.mapBoth (StartDatePickerMsg >> FormMsg)
                            (\newState -> { model | query = { query | start = newState } })

                EndDatePickerMsg datePickerMsg ->
                    DatePicker.update datePickerMsg query.end
                        |> Return.mapBoth (EndDatePickerMsg >> FormMsg)
                            (\newState -> { model | query = { query | end = newState } })

                LimitChanged string ->
                    { model | query = { query | limit = string } }
                        |> Return.singleton

                MinNumberReviewsChanged string ->
                    { model | query = { query | minNumberReviews = string } }
                        |> Return.singleton


view : Model -> Html Msg
view model =
    E.layout []
        (E.column [ E.centerX, E.spacing 10, E.padding 40 ]
            [ Maybe.unwrap
                -- TODO make a wrapper for button
                (Input.button []
                    { onPress = Just UploadFileClicked
                    , label = E.text "Upload Reviews"
                    }
                )
                (\{ name } ->
                    E.column [ E.spacing 30 ]
                        [ E.row [ E.spacing 10 ]
                            [ E.text name, Input.button [] { onPress = Just RemoveFileClicked, label = E.text "X" } ]
                        , DatePicker.view
                            (DatePicker.default
                                { label = "Pick a starting date" }
                            )
                            model.query.start
                            |> E.map (StartDatePickerMsg >> FormMsg)
                        , DatePicker.view
                            (DatePicker.default
                                { label = "Pick an ending date" }
                            )
                            model.query.end
                            |> E.map (EndDatePickerMsg >> FormMsg)
                        , Input.view
                            (Input.default
                                { onChange = LimitChanged
                                , text = model.query.limit
                                , label = "Number of results to show"
                                }
                            )
                            |> E.map FormMsg
                        , Input.view
                            (Input.default
                                { onChange = MinNumberReviewsChanged
                                , text = model.query.limit
                                , label = "Minimun number of reviews"
                                }
                            )
                            |> E.map FormMsg
                        ]
                )
                model.reviewsFile
            ]
        )


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
