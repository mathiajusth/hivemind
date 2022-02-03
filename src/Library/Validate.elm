-- I have written this some time ago but haven't published it yet


module Library.Validate exposing (..)

import Library.Dict.Nonempty as NonemptyDict exposing (NonemptyDict)
import List.Extra as List
import Maybe.Extra as Maybe
import RemoteData exposing (RemoteData)
import Result.Extra as Result
import String.Extra as String
import String.Nonempty as NonemptyString exposing (NonemptyString)



--- VALIDATION  ---


type alias Validation error input output =
    {-
       Validation is a function from user input to the data model of the entity the user is defining
       with possible failure
    -}
    input -> Result error output


compose : Validation e b c -> Validation e a b -> Validation e a c
compose fromB fromA =
    {- This is just a kleisi composition for Result monad -}
    fromA >> Result.andThen fromB


lift : (a -> b) -> Validation e a b
lift f =
    {- Mapping functions to the Result monad's Kleisi category -}
    Ok << f



-- Validation trifunctor (co, contra, co)-variant


mapError : (e -> f) -> Validation e a b -> Validation f a b
mapError mapper validation =
    validation >> Result.mapError mapper


mapInput : (j -> i) -> Validation e i b -> Validation e j b
mapInput mapper validation =
    {- notice that this is contravariant -}
    mapper >> validation


mapOutput : (o -> p) -> Validation e i o -> Validation e i p
mapOutput mapper validation =
    validation >> Result.map mapper


trimap : (e -> f) -> (j -> i) -> (o -> p) -> Validation e i o -> Validation f j p
trimap errorPush inputPull outputPush =
    mapError errorPush >> mapInput inputPull >> mapOutput outputPush


succeed : output -> Validation error input output
succeed =
    {- Validation that always succeeds with the given output regardless of input -}
    always << Ok


fail : error -> Validation error input output
fail =
    {- Validation that always fails with the given error regardless of input -}
    always << Err


accept : Validation error input input
accept =
    Ok


andMap :
    (newError -> otherErrors)
    -> (newError -> otherErrors -> otherErrors)
    -> Validation newError i a
    -> Validation otherErrors i (a -> b)
    -> Validation otherErrors i b
andMap wrapError addError validateA validateAtoB =
    {- If wish to keep all the validation errors when the validation fails,
       as opposed to just the first one which would be the behavior with Result monad's andMap,
       we have to define a new function application that passes the errors along.
    -}
    \i ->
        case ( validateA i, validateAtoB i ) of
            ( Ok b, Ok aToB ) ->
                Ok <| aToB b

            ( Err newError, Ok aToB ) ->
                Err <| wrapError newError

            ( Ok b, Err otherErrors ) ->
                Err otherErrors

            ( Err newError, Err otherErrors ) ->
                Err <| addError newError otherErrors



--- RECORD VALIDATION  ---


type alias RecordValidation error input record =
    Validation (RecordError error) input record


type alias RecordError error =
    NonemptyDict Int error


type alias ValidatedRecord error output =
    {- We keep all the field errors in a dict where indexes are field indexes -}
    Result (RecordError error) output


type alias RecordValidationWithAcc error input record =
    {- Int is used as accumulator to know which field is being validated. -}
    ( Int, RecordValidation error input record )


record : recordConstructor -> RecordValidationWithAcc error input recordConstructor
record recordConstructor =
    ( 0, succeed recordConstructor )


getFieldError : Int -> ValidatedRecord error output -> Maybe error
getFieldError fieldIndex validatedRecord =
    case validatedRecord of
        Ok _ ->
            Nothing

        Err errorDict ->
            NonemptyDict.get fieldIndex errorDict


getFieldErrorIf : Bool -> Int -> ValidatedRecord error output -> Maybe error
getFieldErrorIf shouldShowError fieldIndex validatedRecord =
    if shouldShowError then
        case validatedRecord of
            Ok _ ->
                Nothing

            Err errorDict ->
                NonemptyDict.get fieldIndex errorDict

    else
        Nothing


endRecord : RecordValidationWithAcc error input record -> RecordValidation error input record
endRecord =
    Tuple.second


field :
    (input -> contextForField)
    -> Validation error contextForField field
    -> RecordValidationWithAcc error input (field -> rest)
    -> RecordValidationWithAcc error input rest
field getContext validateField ( fieldIndex, validateFieldToRecord ) =
    {- A concrete implementation of andMapStateful for Records -}
    ( fieldIndex + 1
    , andMap
        (NonemptyDict.singleton fieldIndex)
        (NonemptyDict.insert fieldIndex)
        (validateField << getContext)
        validateFieldToRecord
    )


simplifyRecordError : RecordValidation e i r -> Validation e i r
simplifyRecordError =
    mapError NonemptyDict.head



---- MAYBE VALIDATION ----


maybe : Validation e i o -> Validation e (Maybe i) (Maybe o)
maybe justValidate =
    Maybe.unwrap
        (accept Nothing)
        (justValidate
            |> compose (lift Just)
        )



---- TUPLE VALIDATION ----


tuple : ( Validation error a b, Validation error c d ) -> Validation error ( a, c ) ( b, d )
tuple ( validationA, validationC ) ( a, c ) =
    let
        validatedA =
            validationA a

        validatedC =
            validationC c
    in
    case ( validatedA, validatedC ) of
        ( Ok x, Ok y ) ->
            Ok ( x, y )

        ( Ok x, Err error ) ->
            Err error

        ( Err error, _ ) ->
            Err error



---- LIST VALIDATION ----


list : Validation e a b -> Validation (List (Maybe e)) (List a) (List b)
list aToB =
    \listOfA ->
        let
            listOfResults : List (Result e b)
            listOfResults =
                List.map aToB listOfA

            partition : ( List b, List e )
            partition =
                Result.partition listOfResults
        in
        case partition of
            ( bs, [] ) ->
                Ok bs

            _ ->
                Err <| List.map Result.error listOfResults


listWithSimpleError : Validation e a b -> Validation e (List a) (List b)
listWithSimpleError aToB =
    List.map aToB >> Result.combine


getListError : Int -> Result (List (Maybe e)) (List b) -> Maybe e
getListError index =
    Result.error
        >> Maybe.andThen (List.getAt index)
        >> Maybe.join


getRecordInsideListError : Int -> Int -> Result (List (Maybe (RecordError e))) (List b) -> Maybe e
getRecordInsideListError listIndex recordFieldIndex =
    Result.error
        >> Maybe.andThen (List.getAt listIndex)
        >> Maybe.join
        >> Maybe.andThen (NonemptyDict.get recordFieldIndex)



---- VALIDATION construcotrs ----
-- high level


with : (a -> Result error b) -> Validation error a b
with =
    identity


ifTrue : (a -> Bool) -> error -> Validation error a a
ifTrue condition error =
    \a ->
        if condition a then
            Ok a

        else
            Err error


ifJust : (a -> Maybe b) -> error -> Validation error a b
ifJust toMaybeB errorMsg =
    toMaybeB >> Maybe.unwrap (Err errorMsg) Ok



-- lower level


fromMaybe_ : error -> Validation error (Maybe a) a
fromMaybe_ errorMsg =
    ifJust identity errorMsg


fromRemoteData_ : error -> error -> (remoteError -> error) -> Validation error (RemoteData remoteError a) a
fromRemoteData_ notAskedError loadingError fromRemoteError =
    \remoteData ->
        case remoteData of
            RemoteData.Success data ->
                Ok data

            RemoteData.Failure remoteError ->
                Err <| fromRemoteError remoteError

            RemoteData.Loading ->
                Err loadingError

            RemoteData.NotAsked ->
                Err notAskedError


toInt_ : error -> Validation error String Int
toInt_ error =
    ifJust String.toInt error


toFloat_ : error -> Validation error String Float
toFloat_ error =
    ifJust String.toFloat error


isNonNegativeNumber_ : error -> Validation error number number
isNonNegativeNumber_ errorMsg =
    ifTrue ((<=) 0) errorMsg


isLowerThan_ : number -> error -> Validation error number number
isLowerThan_ ceiling errorMsg =
    ifTrue ((>=) ceiling) errorMsg


isNotEmptyString_ : error -> Validation error String String
isNotEmptyString_ errorMsg =
    ifTrue (not << String.isEmpty) errorMsg


toNonEmptyString : Validation error String (Maybe NonemptyString)
toNonEmptyString =
    NonemptyString.fromString >> Ok



-- with prefilled error messages (this is prone to being changed based on UX requirements)


fromMaybe : Validation String (Maybe a) a
fromMaybe =
    fromMaybe_ "Required"


fromRemoteData : (remoteError -> String) -> Validation String (RemoteData remoteError a) a
fromRemoteData =
    fromRemoteData_ "Data needs to be loaded" "Loading"


toInt : Validation String String Int
toInt =
    toInt_ "Has to be a number"


toFloat : Validation String String Float
toFloat =
    toFloat_ "Has to be a number"


isNonNegativeNumber : Validation String number number
isNonNegativeNumber =
    isNonNegativeNumber_ "Has to be 0 or above"


intIsLowerThan : Int -> Validation String Int Int
intIsLowerThan ceiling =
    isLowerThan_ ceiling ("Has to be lower than " ++ String.fromInt ceiling)


floatIsLowerThan : Float -> Validation String Float Float
floatIsLowerThan ceiling =
    isLowerThan_ ceiling ("Has to be lower than " ++ String.fromFloat ceiling)


isNotEmptyString : Validation String String String
isNotEmptyString =
    isNotEmptyString_ "Can't be empty."
