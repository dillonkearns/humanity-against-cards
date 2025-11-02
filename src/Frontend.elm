module Frontend exposing (Model, app, app_)

import Browser
import Dict
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Subscription as Subscription
import Effect.Browser.Navigation as Navigation
import Effect.Task
import Effect.Time
import Html exposing (Html, text)
import Html.Attributes exposing (id, style, value, placeholder, disabled)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Lamdera
import Time
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
      , playerToken = Nothing
      , playerNameInput = ""
      , joinedPlayer = Nothing
      , playersList = []
      , gameState = Lobby
      , myHand = []
      , currentJudge = Nothing
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

        PlayerNameInputChanged input ->
            ( { model | playerNameInput = input }, Command.none )

        JoinGameClicked ->
            -- Generate a token using current time
            ( model
            , Effect.Task.perform
                (\time -> GotPlayerToken (PlayerToken (String.fromInt (Time.posixToMillis time))))
                Effect.Time.now
            )

        GotPlayerToken token ->
            let
                trimmedName =
                    String.trim model.playerNameInput
            in
            if String.isEmpty trimmedName then
                ( model, Command.none )

            else
                ( { model | playerToken = Just token }
                , Effect.Lamdera.sendToBackend (JoinGame token trimmedName)
                )

        StartGameClicked ->
            ( model, Effect.Lamdera.sendToBackend StartGame )


-- UPDATE FROM BACKEND


updateFromBackend : ToFrontend -> Model -> ( Model, Command FrontendOnly ToBackend FrontendMsg )
updateFromBackend msg model =
    case msg of
        CounterNewValue newValue clientId ->
            ( { model | counter = newValue, clientId = Just clientId }, Command.none )

        DeckLoaded deck ->
            ( { model | loadedDeck = Just deck }, Command.none )

        PlayerJoined player ->
            ( { model | joinedPlayer = Just player }, Command.none )

        PlayersListUpdated players ->
            ( { model | playersList = players }, Command.none )

        GameStarted { yourHand, currentJudge } ->
            ( { model
                | myHand = yourHand
                , currentJudge = Just currentJudge
                , gameState =
                    Playing
                        { currentJudge = currentJudge
                        , remainingAnswers = []  -- We don't track this on frontend
                        , remainingPrompts = []
                        }
              }
            , Command.none
            )

        GameStateUpdated gameState ->
            ( { model | gameState = gameState }, Command.none )


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
        , case model.joinedPlayer of
            Nothing ->
                viewJoinForm model

            Just player ->
                case model.gameState of
                    Lobby ->
                        viewPlayerLobby player

                    Playing _ ->
                        viewPlayerHand player model.myHand model.currentJudge

                    Ended ->
                        Html.div [] [ text "Game ended!" ]
        ]


viewJoinForm : Model -> Html FrontendMsg
viewJoinForm model =
    Html.div []
        [ Html.h2 [] [ text "Join Game" ]
        , Html.input
            [ placeholder "Enter your name..."
            , value model.playerNameInput
            , onInput PlayerNameInputChanged
            , id "player-name-input"
            , style "padding" "10px"
            , style "font-size" "16px"
            , style "margin-bottom" "10px"
            , style "width" "300px"
            ]
            []
        , Html.div []
            [ Html.button
                [ onClick JoinGameClicked
                , id "join-game-button"
                , style "padding" "10px 20px"
                , style "font-size" "16px"
                , disabled (String.isEmpty (String.trim model.playerNameInput))
                ]
                [ text "Join Game" ]
            ]
        ]


viewPlayerLobby : Player -> Html FrontendMsg
viewPlayerLobby player =
    Html.div []
        [ Html.h2 [] [ text ("Welcome, " ++ player.name ++ "!") ]
        , Html.p [ id "player-joined-message" ] [ text "Waiting for game to start..." ]
        ]


viewPlayerHand : Player -> List String -> Maybe PlayerToken -> Html FrontendMsg
viewPlayerHand player hand maybeJudge =
    let
        isJudge =
            maybeJudge == Just player.token
    in
    Html.div []
        [ Html.h2 [] [ text ("Welcome, " ++ player.name ++ "!") ]
        , Html.p []
            [ text
                (if isJudge then
                    "You are the judge this round!"

                 else
                    "Your hand:"
                )
            ]
        , if isJudge then
            Html.div [] [ text "Judge view coming soon..." ]

          else
            Html.ul [ id "player-hand" ]
                (List.map
                    (\card ->
                        Html.li [] [ text card ]
                    )
                    hand
                )
        ]


viewAdmin : Model -> Html FrontendMsg
viewAdmin model =
    Html.div [ style "padding" "30px" ]
        [ Html.h1 [] [ text "Admin Console" ]
        , viewPlayersList model.playersList
        , viewGameControls model
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


viewGameControls : Model -> Html FrontendMsg
viewGameControls model =
    case model.gameState of
        Lobby ->
            let
                canStart =
                    not (List.isEmpty model.playersList) && model.loadedDeck /= Nothing
            in
            Html.div [ style "margin-bottom" "30px" ]
                [ Html.h2 [] [ text "Game Controls" ]
                , Html.button
                    [ onClick StartGameClicked
                    , id "start-game-button"
                    , style "padding" "10px 20px"
                    , style "font-size" "16px"
                    , disabled (not canStart)
                    ]
                    [ text "Start Game" ]
                , if not canStart then
                    Html.p [ style "color" "#666" ]
                        [ text "Need at least one player and a loaded deck to start" ]

                  else
                    Html.text ""
                ]

        Playing _ ->
            Html.div [ style "margin-bottom" "30px" ]
                [ Html.h2 [] [ text "Game Controls" ]
                , Html.p [] [ text "Game in progress..." ]
                ]

        Ended ->
            Html.div [ style "margin-bottom" "30px" ]
                [ Html.h2 [] [ text "Game Controls" ]
                , Html.p [] [ text "Game ended" ]
                ]


viewPlayersList : List Player -> Html FrontendMsg
viewPlayersList players =
    Html.div [ style "margin-bottom" "30px" ]
        [ Html.h2 [] [ text "Joined Players" ]
        , if List.isEmpty players then
            Html.p [] [ text "No players yet" ]

          else
            Html.ul [ id "players-list" ]
                (List.map
                    (\player ->
                        Html.li [] [ text player.name ]
                    )
                    players
                )
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
