module Frontend exposing (Model, app, app_)

import Browser
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Subscription as Subscription
import Effect.Browser.Navigation as Navigation
import Html exposing (Html, text)
import Html.Attributes exposing (id, style, value, placeholder)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Lamdera
import Types exposing (..)
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)


type alias Model =
    FrontendModel


{-| Lamdera applications define 'app' instead of 'main'.

Lamdera.frontend is the same as Browser.application with the
additional update function; updateFromBackend.

-}
app =
    Effect.Lamdera.frontend
        Lamdera.sendToBackend
        app_


app_ =
    { init = init
    , update = update
    , updateFromBackend = updateFromBackend
    , view =
        \model ->
            { title = "Humanity Against Cards"
            , body = [ view model ]
            }
    , subscriptions = \_ -> Subscription.none
    , onUrlChange = UrlChanged
    , onUrlRequest = UrlRequested
    }


-- ROUTING


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map PlayerRoute Parser.top
        , Parser.map AdminRoute (Parser.s "admin")
        ]


urlToRoute : Url -> Route
urlToRoute url =
    Parser.parse routeParser url
        |> Maybe.withDefault PlayerRoute


-- INIT


init : Url -> Navigation.Key -> ( Model, Command FrontendOnly ToBackend FrontendMsg )
init url key =
    ( { counter = 0
      , clientId = Nothing
      , route = urlToRoute url
      , key = key
      , deckJsonInput = ""
      , loadedDeck = Nothing
      }
    , Command.none
    )


-- JSON DECODERS


deckDecoder : Decoder Deck
deckDecoder =
    Decode.map2 Deck
        (Decode.field "prompts" (Decode.list Decode.string))
        (Decode.field "answers" (Decode.list Decode.string))


-- UPDATE


update : FrontendMsg -> Model -> ( Model, Command FrontendOnly ToBackend FrontendMsg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, Effect.Lamdera.sendToBackend CounterIncremented )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Effect.Lamdera.sendToBackend CounterDecremented )

        FNoop ->
            ( model, Command.none )

        UrlChanged url ->
            ( { model | route = urlToRoute url }, Command.none )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.key (Url.toString url) )

                Browser.External url ->
                    ( model, Navigation.load url )

        DeckJsonInputChanged input ->
            ( { model | deckJsonInput = input }, Command.none )

        LoadDeckClicked ->
            case Decode.decodeString deckDecoder model.deckJsonInput of
                Ok deck ->
                    ( { model | loadedDeck = Just deck }
                    , Effect.Lamdera.sendToBackend (LoadDeck deck)
                    )

                Err _ ->
                    -- For now, just do nothing on parse error
                    -- We could add error display later
                    ( model, Command.none )


-- UPDATE FROM BACKEND


updateFromBackend : ToFrontend -> Model -> ( Model, Command FrontendOnly ToBackend FrontendMsg )
updateFromBackend msg model =
    case msg of
        CounterNewValue newValue clientId ->
            ( { model | counter = newValue, clientId = Just clientId }, Command.none )

        DeckLoaded deck ->
            ( { model | loadedDeck = Just deck }, Command.none )


-- VIEW


view : Model -> Html FrontendMsg
view model =
    case model.route of
        PlayerRoute ->
            viewPlayer model

        AdminRoute ->
            viewAdmin model


viewPlayer : Model -> Html FrontendMsg
viewPlayer model =
    Html.div [ style "padding" "30px" ]
        [ Html.h1 [] [ text "Humanity Against Cards" ]
        , Html.p [] [ text "Player view - coming soon!" ]
        , Html.button [ onClick Increment, id "plusOne" ] [ text "+" ]
        , Html.text (String.fromInt model.counter)
        , Html.button [ onClick Decrement, id "minusOne" ] [ text "-" ]
        ]


viewAdmin : Model -> Html FrontendMsg
viewAdmin model =
    Html.div [ style "padding" "30px" ]
        [ Html.h1 [] [ text "Admin Console" ]
        , Html.h2 [] [ text "Load Deck" ]
        , Html.textarea
            [ placeholder "Paste deck JSON here..."
            , value model.deckJsonInput
            , onInput DeckJsonInputChanged
            , style "width" "100%"
            , style "min-height" "200px"
            , style "font-family" "monospace"
            , id "deck-json-input"
            ]
            []
        , Html.div []
            [ Html.button
                [ onClick LoadDeckClicked
                , id "load-deck-button"
                , style "margin-top" "10px"
                ]
                [ text "Load Deck" ]
            ]
        , case model.loadedDeck of
            Nothing ->
                Html.div [] [ text "No deck loaded yet" ]

            Just deck ->
                viewLoadedDeck deck
        ]


viewLoadedDeck : Deck -> Html FrontendMsg
viewLoadedDeck deck =
    Html.div [ style "margin-top" "30px" ]
        [ Html.h3 [] [ text "Loaded Deck" ]
        , Html.h4 [] [ text "Prompts:" ]
        , Html.ul [ id "prompts-list" ]
            (List.map (\prompt -> Html.li [] [ text prompt ]) deck.prompts)
        , Html.h4 [] [ text "Answers:" ]
        , Html.ul [ id "answers-list" ]
            (List.map (\answer -> Html.li [] [ text answer ]) deck.answers)
        ]
