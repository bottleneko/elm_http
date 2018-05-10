import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode

main =
    Html.program
        { init = init "cats"
        , update = update
        , view = view
        , subscriptions = subscriptions
         }

-- MODEL

type alias Model =
    { topic  : String
    , gifUrl : String
    , error  : String
    }

init : String -> (Model, Cmd Msg)
init topic = (Model topic "priv/waiting.gif" "", Cmd.none)

-- UPDATE

type Msg
    = MorePlease
    | NewGif (Result Http.Error String)
    | ChangeTopic String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MorePlease ->
            (model, getRandomGif model.topic)
        NewGif (Ok newUrl) ->
            ({ model | gifUrl = newUrl }, Cmd.none)
        NewGif (Err errorMessage) ->
            ({ model | error = (toString errorMessage) }, Cmd.none)
        ChangeTopic newTopic ->
            ({ model | topic = newTopic }, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Type topic", onInput ChangeTopic, defaultValue model.topic ] []
        , button [ onClick MorePlease ] [ text "More Please!" ]
        , br [] []
        , showImg model
        ]

showImg : Model -> Html Msg
showImg model =
    case model.error of
        "" ->
            img [ src model.gifUrl ] []
        error ->
            div []
                [ h1 [] [ text "Error" ]
                , p [] [ text error ]
                ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- HTTP

getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let
        url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
    in
        Http.send NewGif (Http.get url decodeGifUrl)

decodeGifUrl : Decode.Decoder String
decodeGifUrl = Decode.at ["data", "image_url"] Decode.string
