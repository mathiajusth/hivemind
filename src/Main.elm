module Main exposing (..)

import Browser as Browser
import Data.Product as Product
import Data.Review as Review exposing (Review)
import Element as E
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import GenericDict exposing (Dict)
import Html exposing (Html)
import Return exposing (Return)


type alias Model =
    { reviewsFile :
        Maybe
            { name : String
            , content : Dict Product.Id Review
            }
    }


type alias Product =
    String


type alias Flags =
    ()


init : Flags -> Return Msg Model
init _ =
    Return.singleton { reviewsFile = Nothing }


type Msg
    = UploadReviewsClicked
    | ReviewsUploaded File


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        UploadReviewsClicked ->
            Return.return model
                (Select.file [ "application/x-ndjson" ] ReviewsUploaded)

        ReviewsUploaded file ->
            Debug.todo "process and save the file"


view : Model -> Html Msg
view model =
    E.layout []
        (E.column [ E.centerX, E.spacing 10, E.padding 40 ]
            [ Input.button []
                { onPress = Just UploadReviewsClicked
                , label = E.text "Upload Reviews"
                }
            , E.el [ E.centerX ] (E.text "TODO")
            ]
        )


main : Platform.Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
