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
import Html.Attributes as HtmlAttributes
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
import Ui.Button as Button
import Ui.DatePicker as DatePicker
import Ui.Input as Input
import Ui.WithError as WithError



-- MODEL


type alias Model =
    { reviewsFile :
        Maybe
            { name : String
            , content : Result Decode.Error (Dict Product.Id (List Review))
            }
    , zone : Maybe Time.Zone
    , queryForm : QueryForm
    , showValidationErrors : Bool
    }


init : () -> Return Msg Model
init _ =
    Return.return
        { reviewsFile = Nothing
        , zone = Nothing
        , queryForm =
            { start = DatePicker.initWithoutToday
            , end = DatePicker.initWithoutToday
            , limit = ""
            , minNumberReviews = ""
            }
        , showValidationErrors = False
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
    let
        correctDateNotProvidedErrorText : String
        correctDateNotProvidedErrorText =
            "Required in ISO format: YYYY-MM-DD"
    in
    Validate.record Query
        |> Validate.field .start
            (Validate.lift DatePicker.getDate
                |> Validate.compose (Validate.fromMaybe_ correctDateNotProvidedErrorText)
                |> Validate.compose
                    (Validate.lift (Time.fromDate Time.StartOfDay usersZone))
            )
        |> Validate.field .end
            (Validate.lift DatePicker.getDate
                |> Validate.compose (Validate.fromMaybe_ correctDateNotProvidedErrorText)
                |> Validate.compose
                    (Validate.lift (Time.fromDate Time.EndOfDay usersZone))
            )
        |> Validate.field .limit
            (Validate.isNotEmptyString
                |> Validate.compose Validate.toInt
            )
        |> Validate.field .minNumberReviews
            (Validate.isNotEmptyString
                |> Validate.compose Validate.toInt
            )
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
    | RunQueryClicked (Validate.ValidatedRecord String Query)


update : Msg -> Model -> Return Msg Model
update msg ({ queryForm } as model) =
    case msg of
        ZoneRecieved zone ->
            { model | zone = Just zone }
                |> Return.singleton

        TodayRecieved today ->
            { model
                | queryForm =
                    { queryForm
                        | start =
                            queryForm.start
                                |> DatePicker.setToday today
                        , end =
                            queryForm.end
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
                    DatePicker.update datePickerMsg queryForm.start
                        |> Return.mapBoth (StartDatePickerMsg >> FormMsg)
                            (\newState -> { model | queryForm = { queryForm | start = newState } })

                EndDatePickerMsg datePickerMsg ->
                    DatePicker.update datePickerMsg queryForm.end
                        |> Return.mapBoth (EndDatePickerMsg >> FormMsg)
                            (\newState -> { model | queryForm = { queryForm | end = newState } })

                LimitChanged string ->
                    { model | queryForm = { queryForm | limit = string } }
                        |> Return.singleton

                MinNumberReviewsChanged string ->
                    { model | queryForm = { queryForm | minNumberReviews = string } }
                        |> Return.singleton

                RunQueryClicked validatedQuery ->
                    Result.unwrap
                        { model | showValidationErrors = True }
                        (\query ->
                            -- TODO
                            model
                        )
                        validatedQuery
                        |> Return.singleton


view : Model -> Html Msg
view model =
    E.layout []
        (E.column [ E.centerX, E.spacing 10, E.padding 40 ]
            [ Maybe.unwrap
                -- TODO make a wrapper for button
                (Button.view
                    (Button.default "Upload Reviews"
                        |> Button.onClick UploadFileClicked
                    )
                )
                (\{ name } ->
                    Maybe.unwrap E.none
                        (\zone ->
                            let
                                validatedQuery : Validate.ValidatedRecord String Query
                                validatedQuery =
                                    validateQuery zone model.queryForm
                            in
                            E.column [ E.spacing 42 ]
                                [ E.row [ E.spacing 10 ]
                                    [ E.text name
                                    , Button.view
                                        (Button.default "X"
                                            |> Button.onClick RemoveFileClicked
                                        )
                                        |> E.el [ E.htmlAttribute <| HtmlAttributes.title "Remove file" ]
                                    ]
                                , DatePicker.view
                                    (DatePicker.default
                                        { label = "Pick a starting date" }
                                    )
                                    model.queryForm.start
                                    |> E.map (StartDatePickerMsg >> FormMsg)
                                    |> WithError.error (Validate.getFieldErrorIf model.showValidationErrors 0 validatedQuery)
                                , DatePicker.view
                                    (DatePicker.default
                                        { label = "Pick an ending date" }
                                    )
                                    model.queryForm.end
                                    |> E.map (EndDatePickerMsg >> FormMsg)
                                    |> WithError.error (Validate.getFieldErrorIf model.showValidationErrors 1 validatedQuery)
                                , Input.view
                                    (Input.default
                                        { onChange = LimitChanged
                                        , text = model.queryForm.limit
                                        , label = "Number of results to show"
                                        }
                                    )
                                    |> E.map FormMsg
                                    |> WithError.error (Validate.getFieldErrorIf model.showValidationErrors 2 validatedQuery)
                                , Input.view
                                    (Input.default
                                        { onChange = MinNumberReviewsChanged
                                        , text = model.queryForm.minNumberReviews
                                        , label = "Minimun number of reviews"
                                        }
                                    )
                                    |> E.map FormMsg
                                    |> WithError.error (Validate.getFieldErrorIf model.showValidationErrors 3 validatedQuery)
                                , Button.view
                                    (Button.default "Run Query"
                                        |> Button.onClick (RunQueryClicked validatedQuery)
                                    )
                                    |> E.map FormMsg
                                    |> E.el [ E.alignRight ]
                                ]
                        )
                        model.zone
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
