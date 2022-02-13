module Main exposing (..)

import Browser as Browser
import Data.Product as Product
import Data.Rating as Rating
import Data.Review as Review exposing (Review)
import Date exposing (Date)
import Element as E exposing (Element)
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import GenericDict as Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HtmlAttributes
import Json.Decode as Decode
import Library.Id as Id
import Library.List.Extra as List
import Library.Validate as Validate exposing (Validation)
import List.Extra as List
import List.Nonempty as NonemptyList
import Maybe.Extra as Maybe
import Result.Extra as Result
import Return exposing (Return)
import Round
import Task
import Time
import Time.Extra as Time
import Ui.Button as Button
import Ui.DatePicker as DatePicker
import Ui.Input as Input
import Ui.WithError as WithError



-- MODEL


type Model
    = LoadingGlobalData
    | GlobalDataLoadedModel GlobalData GlobalDataLoaded


type alias GlobalData =
    { zone : Time.Zone
    , today : Date
    }


type
    GlobalDataLoaded
    -- This sum type model architecture enrures that only possible states are possible
    -- while the model is not deeply nested and wrapped in multiple Maybes and Results
    -- (as it would be if the fileName that is in two branches was not distributed - like * over + but product type over sum type)
    = FileNotUploaded
    | FileParsingInProgress { fileName : String }
    | FileParsingSuccess
        { fileName : String
        , groupedReviews : List.Grouped Review
        , queryForm : QueryForm
        , showResponseOrErrors : Bool
        }
    | FileParsingFailed Decode.Error


init : () -> Return Msg Model
init _ =
    Return.return LoadingGlobalData
        (Task.map2 GlobalData Time.here Date.today
            |> Task.perform GlobalDataLoaded
        )


type alias QueryForm =
    { start : DatePicker.Model
    , end : DatePicker.Model
    , limit : String
    , minNumberReviews : String
    }


type alias Query =
    { start : Date
    , end : Date
    , limit : Int
    , minNumberReviews : Int
    }


validateQuery : Validate.RecordValidation String QueryForm Query
validateQuery =
    let
        correctDateNotProvidedErrorText : String
        correctDateNotProvidedErrorText =
            "Required in ISO format: YYYY-MM-DD"
    in
    Validate.record Query
        |> Validate.field .start
            (Validate.lift DatePicker.getDate
                |> Validate.compose (Validate.fromMaybe_ correctDateNotProvidedErrorText)
            )
        |> Validate.field .end
            (Validate.lift DatePicker.getDate
                |> Validate.compose (Validate.fromMaybe_ correctDateNotProvidedErrorText)
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


{-| Messages are divided based on which model's branches' view function
they can be fired from
-}
type Msg
    = GlobalDataLoaded GlobalData
    | GlobalDataLoadedMsg GlobalDataLoadedMsg


type GlobalDataLoadedMsg
    = FileNotUploadedMsg FileNotUploadedMsg
    | FileParsingInProgressMsg FileParsingInProgressMsg
    | FileParsingSuccessMsg FileParsingSuccessMsg
      -- when () replaced by sum type the pattern match in the update function will throw error
      -- so you have to fill in the implementation
    | FileParsingFailedMsg ()


type FileNotUploadedMsg
    = UploadFileClicked
    | FileUploaded File


type FileParsingInProgressMsg
    = FileParsed (Result Decode.Error (List.Grouped Review))


type FileParsingSuccessMsg
    = StartDatePickerMsg DatePicker.Msg
    | EndDatePickerMsg DatePicker.Msg
    | LimitChanged String
    | MinNumberReviewsChanged String
    | ShowClicked


parseFileContent : String -> Result Decode.Error (List.Grouped Review)
parseFileContent ndJsonString =
    ndJsonString
        -- when loading the file "\n"
        -- was always inserted at the end of the file
        -- don't know why but hence the trim
        |> String.trim
        |> String.lines
        |> List.map (Decode.decodeString Review.decoder)
        |> Result.combine
        |> Result.map (List.groupEqualsBy .productID)


{-| Update function is then constructed so that only exactly adimissible
model-msg combinations are handled (exactly because type error will be thrown if
a msg is added to the correspoonding msg branch due to the usages of "\_"s i.e. non used parameters)
-}
update : Msg -> Model -> Return Msg Model
update beforeInitMsg beforeInitModel =
    case ( beforeInitModel, beforeInitMsg ) of
        ( LoadingGlobalData, GlobalDataLoaded initData ) ->
            GlobalDataLoadedModel initData FileNotUploaded
                |> Return.singleton

        ( LoadingGlobalData, _ ) ->
            Return.singleton beforeInitModel

        ( GlobalDataLoadedModel initData model, GlobalDataLoadedMsg msg ) ->
            (case ( model, msg ) of
                ( FileNotUploaded, FileNotUploadedMsg fileNotUploadedMsg ) ->
                    case fileNotUploadedMsg of
                        UploadFileClicked ->
                            Return.return model
                                (Select.file [ "application/x-ndjson" ] (FileUploaded >> FileNotUploadedMsg))

                        FileUploaded file ->
                            Return.return (FileParsingInProgress { fileName = File.name file })
                                (Task.perform
                                    (FileParsed >> FileParsingInProgressMsg)
                                    (File.toString file
                                        |> Task.map parseFileContent
                                    )
                                )

                ( FileNotUploaded, _ ) ->
                    Return.singleton model

                ( FileParsingInProgress { fileName }, FileParsingInProgressMsg fileParsingInProgressMsg ) ->
                    case fileParsingInProgressMsg of
                        FileParsed parsedFileResult ->
                            Result.unpack FileParsingFailed
                                (\parsedFile ->
                                    FileParsingSuccess
                                        { fileName = fileName
                                        , groupedReviews = parsedFile
                                        , queryForm =
                                            { start =
                                                DatePicker.initWithoutToday
                                                    |> DatePicker.setToday initData.today
                                            , end =
                                                DatePicker.initWithoutToday
                                                    |> DatePicker.setToday initData.today
                                            , limit = ""
                                            , minNumberReviews = ""
                                            }
                                        , showResponseOrErrors = False
                                        }
                                )
                                parsedFileResult
                                |> Return.singleton

                ( FileParsingInProgress _, _ ) ->
                    Return.singleton model

                ( FileParsingSuccess ({ queryForm } as fileParsedModel), FileParsingSuccessMsg formMsg ) ->
                    (case formMsg of
                        StartDatePickerMsg datePickerMsg ->
                            DatePicker.update datePickerMsg queryForm.start
                                |> Return.mapBoth (StartDatePickerMsg >> FileParsingSuccessMsg)
                                    (\newState -> { fileParsedModel | queryForm = { queryForm | start = newState } })

                        EndDatePickerMsg datePickerMsg ->
                            DatePicker.update datePickerMsg queryForm.end
                                |> Return.mapBoth (EndDatePickerMsg >> FileParsingSuccessMsg)
                                    (\newState -> { fileParsedModel | queryForm = { queryForm | end = newState } })

                        LimitChanged string ->
                            { fileParsedModel | queryForm = { queryForm | limit = string } }
                                |> Return.singleton

                        MinNumberReviewsChanged string ->
                            { fileParsedModel | queryForm = { queryForm | minNumberReviews = string } }
                                |> Return.singleton

                        ShowClicked ->
                            { fileParsedModel | showResponseOrErrors = True }
                                |> Return.singleton
                    )
                        |> Return.map FileParsingSuccess

                ( FileParsingSuccess _, _ ) ->
                    Return.singleton model

                ( FileParsingFailed _, FileParsingFailedMsg () ) ->
                    Return.singleton model

                ( FileParsingFailed _, _ ) ->
                    Return.singleton model
            )
                |> Return.mapBoth GlobalDataLoadedMsg (GlobalDataLoadedModel initData)

        ( GlobalDataLoadedModel initData model, _ ) ->
            Return.singleton beforeInitModel


view : Model -> Html Msg
view model =
    E.layout []
        ((case model of
            LoadingGlobalData ->
                -- should be instant
                E.none

            GlobalDataLoadedModel { zone } subModel ->
                E.el [ E.centerX, E.spacing 10, E.padding 40 ]
                    (case subModel of
                        FileNotUploaded ->
                            Button.view
                                (Button.default "Upload Reviews"
                                    |> Button.onClick UploadFileClicked
                                )
                                |> E.map FileNotUploadedMsg

                        FileParsingInProgress { fileName } ->
                            E.text <| ".. Parsing " ++ fileName ++ " .."

                        FileParsingSuccess { groupedReviews, queryForm, showResponseOrErrors } ->
                            let
                                validatedQuery : Validate.ValidatedRecord String Query
                                validatedQuery =
                                    validateQuery queryForm
                            in
                            E.column [ E.spacing 42 ]
                                [ DatePicker.view
                                    (DatePicker.default
                                        { label = "Pick a starting date" }
                                    )
                                    queryForm.start
                                    |> E.map StartDatePickerMsg
                                    |> WithError.error (Validate.getFieldErrorIf showResponseOrErrors 0 validatedQuery)
                                , DatePicker.view
                                    (DatePicker.default
                                        { label = "Pick an ending date" }
                                    )
                                    queryForm.end
                                    |> E.map EndDatePickerMsg
                                    |> WithError.error (Validate.getFieldErrorIf showResponseOrErrors 1 validatedQuery)
                                , Input.view
                                    (Input.default
                                        { onChange = LimitChanged
                                        , text = queryForm.limit
                                        , label = "Number of results to show"
                                        }
                                    )
                                    |> WithError.error (Validate.getFieldErrorIf showResponseOrErrors 2 validatedQuery)
                                , Input.view
                                    (Input.default
                                        { onChange = MinNumberReviewsChanged
                                        , text = queryForm.minNumberReviews
                                        , label = "Minimun number of reviews"
                                        }
                                    )
                                    |> WithError.error (Validate.getFieldErrorIf showResponseOrErrors 3 validatedQuery)
                                , Button.view
                                    (Button.default "Show"
                                        |> Button.onClick ShowClicked
                                    )
                                    |> E.el [ E.alignRight ]
                                , if showResponseOrErrors then
                                    Result.unwrap E.none
                                        (\query ->
                                            groupedReviews
                                                |> List.filterGroupMembers
                                                    (\review ->
                                                        Date.isBetween
                                                            query.start
                                                            query.end
                                                            (Date.fromPosix zone review.time)
                                                    )
                                                |> List.filter
                                                    (\group ->
                                                        NonemptyList.length group >= query.minNumberReviews
                                                    )
                                                |> List.map
                                                    (\group ->
                                                        let
                                                            ratingAverage : Float
                                                            ratingAverage =
                                                                group
                                                                    |> NonemptyList.map .rating
                                                                    |> Rating.average
                                                        in
                                                        { ratingAverage = ratingAverage, productID = (NonemptyList.head group).productID }
                                                    )
                                                -- not sure which sorting algorithm
                                                -- elm uses by default
                                                -- but we probably could
                                                -- make this more efficient for small limits
                                                -- by folding the list and keeping
                                                -- track only of the highes N rating averages in the accumulator
                                                -- (N = limit)
                                                |> List.sortBy .ratingAverage
                                                |> List.take query.limit
                                                |> viewResults
                                        )
                                        validatedQuery

                                  else
                                    E.none
                                ]
                                |> E.map FileParsingSuccessMsg

                        FileParsingFailed decodeError ->
                            E.text <| "Error parsing file: " ++ Decode.errorToString decodeError
                    )
         )
            |> E.map GlobalDataLoadedMsg
        )



--     (E.column [ E.centerX, E.spacing 10, E.padding 40 ]
--         [ Maybe.unwrap
--             (Button.view
--                 (Button.default "Upload Reviews"
--                     |> Button.onClick UploadFileClicked
--                 )
--             )
--             (\{ name, content } ->
--                 Result.unpack (\parsingError -> E.text <| "Error parsing file: " ++ Decode.errorToString parsingError)
--                     (\groupedReviews ->
--                         let
--                             validatedQuery : Validate.ValidatedRecord String Query
--                             validatedQuery =
--                                 validateQuery model.queryForm
--                         in
--                         E.column [ E.spacing 42 ]
--                             [
--                                  Button.view
--                                     (Button.default "X"
--                                         |> Button.onClick RemoveFileClicked
--                                     )
--                                     |> E.el [ E.htmlAttribute <| HtmlAttributes.title "Remove file" ]
--                             , DatePicker.view
--                                 (DatePicker.default
--                                     { label = "Pick a starting date" }
--                                 )
--                                 model.queryForm.start
--                                 |> E.map (StartDatePickerMsg >> FormMsg)
--                                 |> WithError.error (Validate.getFieldErrorIf model.showResponseOrErrors 0 validatedQuery)
--                             , DatePicker.view
--                                 (DatePicker.default
--                                     { label = "Pick an ending date" }
--                                 )
--                                 model.queryForm.end
--                                 |> E.map (EndDatePickerMsg >> FormMsg)
--                                 |> WithError.error (Validate.getFieldErrorIf model.showResponseOrErrors 1 validatedQuery)
--                             , Input.view
--                                 (Input.default
--                                     { onChange = LimitChanged
--                                     , text = model.queryForm.limit
--                                     , label = "Number of results to show"
--                                     }
--                                 )
--                                 |> E.map FormMsg
--                                 |> WithError.error (Validate.getFieldErrorIf model.showResponseOrErrors 2 validatedQuery)
--                             , Input.view
--                                 (Input.default
--                                     { onChange = MinNumberReviewsChanged
--                                     , text = model.queryForm.minNumberReviews
--                                     , label = "Minimun number of reviews"
--                                     }
--                                 )
--                                 |> E.map FormMsg
--                                 |> WithError.error (Validate.getFieldErrorIf model.showResponseOrErrors 3 validatedQuery)
--                             , Button.view
--                                 (Button.default "Show"
--                                     |> Button.onClick ShowClicked
--                                 )
--                                 |> E.map FormMsg
--                                 |> E.el [ E.alignRight ]
--                             , if model.showResponseOrErrors then
--                                 Result.unwrap E.none
--                                     (\query ->
--                                         -- task to get zone never fails so we can show E.none on fail as it never happens
--                                         Maybe.unwrap E.none
--                                             (\zone ->
--                                                 groupedReviews
--                                                     |> List.filterGroupMembers
--                                                         (\review ->
--                                                             Date.isBetween
--                                                                 query.start
--                                                                 query.end
--                                                                 (Date.fromPosix zone review.time)
--                                                         )
--                                                     |> List.filter
--                                                         (\group ->
--                                                             NonemptyList.length group >= query.minNumberReviews
--                                                         )
--                                                     |> List.map
--                                                         (\group ->
--                                                             let
--                                                                 ratingAverage : Float
--                                                                 ratingAverage =
--                                                                     group
--                                                                         |> NonemptyList.map .rating
--                                                                         |> Rating.average
--                                                             in
--                                                             { ratingAverage = ratingAverage, productID = (NonemptyList.head group).productID }
--                                                         )
--                                                     -- not sure which sorting algorithm
--                                                     -- elm uses by default
--                                                     -- but we probably could
--                                                     -- make this more efficient for small limits
--                                                     -- by folding the list and keeping
--                                                     -- track only of the highes N rating averages in the accumulator
--                                                     -- (N = limit)
--                                                     |> List.sortBy .ratingAverage
--                                                     |> List.take query.limit
--                                                     |> viewResults
--                                             )
--                                             model.zone
--                                     )
--                                     validatedQuery
--                               else
--                                 E.none
--                             ]
--                     )
--                     content
--             )
--             model.reviewsFile
--         ]
--     )


viewResults : List { ratingAverage : Float, productID : Product.Id } -> Element msg
viewResults results =
    E.column [ E.spacing 20 ]
        (List.map
            (\{ ratingAverage, productID } ->
                E.column []
                    [ E.text <| "Product ID :" ++ Id.toString productID
                    , E.text <| "Average rating: " ++ Round.round 2 ratingAverage
                    ]
            )
            results
        )


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
