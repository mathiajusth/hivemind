module Main exposing (..)

import Browser as Browser
import Data.Product as Product
import Data.Review as Review exposing (Review)
import Element as E
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import GenericDict as Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as Decode
import Library.DatePicker as DatePicker
import Library.Id as Id
import List.Extra as List
import Maybe.Extra as Maybe
import Result.Extra as Result
import Return exposing (Return)
import Task


type alias Model =
    { reviewsFile :
        Maybe
            { name : String
            , content : Result Decode.Error (Dict Product.Id (List Review))
            }
    }


init : () -> Return Msg Model
init _ =
    { reviewsFile = Nothing
    }
        |> Return.singleton


type Msg
    = UploadFileClicked
    | FileUploaded File
    | FileExtracted String String
    | RemoveFileClicked


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
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


view : Model -> Html Msg
view model =
    E.layout []
        (E.column [ E.centerX, E.spacing 10, E.padding 40 ]
            [ Maybe.unwrap
                (Input.button []
                    { onPress = Just UploadFileClicked
                    , label = E.text "Upload Reviews"
                    }
                )
                (\{ name } ->
                    E.row [ E.spacing 10 ]
                        [ E.text name
                        , Input.button [] { onPress = Just RemoveFileClicked, label = E.text "X" }
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
