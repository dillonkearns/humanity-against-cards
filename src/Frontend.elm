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
      , selectedCard = Nothing
      , hasSubmitted = False
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

        CardSelected card ->
            ( { model | selectedCard = Just card }, Command.none )

        SubmitCardClicked ->
            case ( model.playerToken, model.selectedCard ) of
                ( Just token, Just card ) ->
                    ( { model | hasSubmitted = True }
                    , Effect.Lamdera.sendToBackend (SubmitCard token card)
                    )

                _ ->
                    ( model, Command.none )

        RevealNextCardClicked ->
            ( model, Effect.Lamdera.sendToBackend RevealNextCard )

        SelectWinnerClicked winnerToken ->
            ( model, Effect.Lamdera.sendToBackend (SelectWinner winnerToken) )


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

        GameStarted { yourHand, currentJudge, initialPrompt } ->
            let
                -- Create initial round phase with the prompt
                initialRoundPhase =
                    SubmissionPhase
                        { prompt = initialPrompt
                        , submissions = []
                        }

                updatedGameState =
                    Playing
                        { currentJudge = currentJudge
                        , remainingAnswers = []
                        , remainingPrompts = []
                        , roundPhase = Just initialRoundPhase
                        }
            in
            ( { model
                | myHand = yourHand
                , currentJudge = Just currentJudge
                , gameState = updatedGameState
                , hasSubmitted = False  -- Reset submission status for new round
              }
            , Command.none
            )

        GameStateUpdated gameState ->
            ( { model | gameState = gameState }, Command.none )

        RoundPhaseUpdated roundPhase ->
            let
                updatedGameState =
                    case model.gameState of
                        Playing playingState ->
                            Playing { playingState | roundPhase = Just roundPhase }

                        other ->
                            other
            in
            ( { model | gameState = updatedGameState }, Command.none )

        CardSubmitted ->
            -- Confirmation message, already handled locally
            ( model, Command.none )

        WinnerSelected { winner, winningCard } ->
            -- TODO: Handle winner announcement
            ( model, Command.none )


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
                        viewPlayerGame player model

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


viewPlayerGame : Player -> Model -> Html FrontendMsg
viewPlayerGame player model =
    let
        isJudge =
            model.currentJudge == Just player.token

        roundPhase =
            case model.gameState of
                Playing playingState ->
                    playingState.roundPhase

                _ ->
                    Nothing
    in
    Html.div []
        [ Html.h2 [] [ text ("Welcome, " ++ player.name ++ "!") ]
        , case roundPhase of
            Nothing ->
                Html.p [] [ text "Round starting..." ]

            Just phase ->
                viewRoundPhase player model isJudge phase
        ]


viewRoundPhase : Player -> Model -> Bool -> RoundPhase -> Html FrontendMsg
viewRoundPhase player model isJudge roundPhase =
    case roundPhase of
        SubmissionPhase { prompt, submissions } ->
            Html.div []
                [ Html.div
                    [ style "margin-bottom" "20px"
                    , style "padding" "20px"
                    , style "background-color" "#f0f0f0"
                    , style "border-radius" "8px"
                    ]
                    [ Html.h3 [] [ text "Current Prompt:" ]
                    , Html.p
                        [ style "font-size" "18px"
                        , style "font-weight" "bold"
                        , id "current-prompt"
                        ]
                        [ text prompt ]
                    ]
                , if isJudge then
                    Html.div []
                        [ Html.p [] [ text "You are the judge this round!" ]
                        , Html.p [] [ text ("Waiting for " ++ String.fromInt (List.length submissions) ++ " submissions...") ]
                        ]

                  else
                    viewCardSelection model
                ]

        RevealPhase { prompt, submissions, revealedCount } ->
            Html.div []
                [ Html.div
                    [ style "margin-bottom" "20px"
                    , style "padding" "20px"
                    , style "background-color" "#f0f0f0"
                    , style "border-radius" "8px"
                    ]
                    [ Html.h3 [] [ text "Current Prompt:" ]
                    , Html.p
                        [ style "font-size" "18px"
                        , style "font-weight" "bold"
                        , id "current-prompt"
                        ]
                        [ text prompt ]
                    ]
                , if isJudge then
                    viewJudgeReveal submissions revealedCount

                  else
                    Html.div []
                        [ Html.h3 [] [ text "Revealing cards..." ]
                        , Html.p [ id "reveal-status" ]
                            [ text (String.fromInt revealedCount ++ " of " ++ String.fromInt (List.length submissions) ++ " cards revealed") ]
                        ]
                ]

        JudgingPhase { prompt, submissions } ->
            Html.div []
                [ Html.div
                    [ style "margin-bottom" "20px"
                    , style "padding" "20px"
                    , style "background-color" "#f0f0f0"
                    , style "border-radius" "8px"
                    ]
                    [ Html.h3 [] [ text "Current Prompt:" ]
                    , Html.p
                        [ style "font-size" "18px"
                        , style "font-weight" "bold"
                        , id "current-prompt"
                        ]
                        [ text prompt ]
                    ]
                , if isJudge then
                    viewJudgeSelection prompt submissions

                  else
                    Html.div []
                        [ Html.h3 [] [ text "All cards revealed!" ]
                        , Html.p [] [ text "Waiting for judge to select winner..." ]
                        , viewSubmittedCards submissions
                        ]
                ]


viewCardSelection : Model -> Html FrontendMsg
viewCardSelection model =
    Html.div []
        [ Html.h3 [] [ text "Your Hand:" ]
        , if model.hasSubmitted then
            Html.p [ style "color" "green", id "submission-status" ]
                [ text "✓ Card submitted! Waiting for other players..." ]

          else
            Html.div []
                [ Html.ul [ id "player-hand", style "list-style" "none", style "padding" "0" ]
                    (List.indexedMap (viewCardButton model.selectedCard) model.myHand)
                , case model.selectedCard of
                    Nothing ->
                        Html.p [ style "color" "#666" ] [ text "Select a card to submit" ]

                    Just _ ->
                        Html.button
                            [ onClick SubmitCardClicked
                            , id "submit-card-button"
                            , style "padding" "10px 20px"
                            , style "font-size" "16px"
                            , style "background-color" "#4CAF50"
                            , style "color" "white"
                            , style "border" "none"
                            , style "border-radius" "4px"
                            , style "cursor" "pointer"
                            ]
                            [ text "Submit Card" ]
                ]
        ]


viewJudgeReveal : List Submission -> Int -> Html FrontendMsg
viewJudgeReveal submissions revealedCount =
    let
        revealedSubmissions =
            List.take revealedCount submissions

        hasMore =
            revealedCount < List.length submissions
    in
    Html.div []
        [ Html.h3 [] [ text "Reveal Cards One-by-One" ]
        , Html.div [ id "revealed-cards", style "margin-bottom" "20px" ]
            (List.indexedMap viewRevealedCard revealedSubmissions)
        , if hasMore then
            Html.button
                [ onClick RevealNextCardClicked
                , id "reveal-next-button"
                , style "padding" "15px 30px"
                , style "font-size" "18px"
                , style "background-color" "#2196F3"
                , style "color" "white"
                , style "border" "none"
                , style "border-radius" "4px"
                , style "cursor" "pointer"
                ]
                [ text ("Reveal Card " ++ String.fromInt (revealedCount + 1)) ]

          else
            Html.p [ style "color" "green", style "font-weight" "bold" ]
                [ text "All cards revealed! Now select the winner." ]
        ]


viewRevealedCard : Int -> Submission -> Html FrontendMsg
viewRevealedCard index submission =
    Html.div
        [ style "padding" "15px"
        , style "margin-bottom" "10px"
        , style "background-color" "white"
        , style "border" "2px solid #ddd"
        , style "border-radius" "8px"
        ]
        [ Html.p [ style "font-size" "16px", style "margin" "0" ] [ text submission.card ] ]


viewJudgeSelection : String -> List Submission -> Html FrontendMsg
viewJudgeSelection prompt submissions =
    Html.div []
        [ Html.h3 [] [ text "Select the Winner!" ]
        , Html.div [ id "judging-cards" ]
            (List.indexedMap viewJudgingCard submissions)
        ]


viewJudgingCard : Int -> Submission -> Html FrontendMsg
viewJudgingCard index submission =
    Html.div
        [ style "padding" "15px"
        , style "margin-bottom" "10px"
        , style "background-color" "white"
        , style "border" "2px solid #ddd"
        , style "border-radius" "8px"
        ]
        [ Html.p [ style "font-size" "16px", style "margin-bottom" "10px" ] [ text submission.card ]
        , Html.button
            [ onClick (SelectWinnerClicked submission.playerToken)
            , id ("select-winner-" ++ String.fromInt index)
            , style "padding" "10px 20px"
            , style "font-size" "14px"
            , style "background-color" "#4CAF50"
            , style "color" "white"
            , style "border" "none"
            , style "border-radius" "4px"
            , style "cursor" "pointer"
            ]
            [ text "Select Winner" ]
        ]


viewSubmittedCards : List Submission -> Html FrontendMsg
viewSubmittedCards submissions =
    Html.div [ id "submitted-cards" ]
        (List.map
            (\submission ->
                Html.div
                    [ style "padding" "15px"
                    , style "margin-bottom" "10px"
                    , style "background-color" "white"
                    , style "border" "1px solid #ddd"
                    , style "border-radius" "8px"
                    ]
                    [ Html.p [ style "font-size" "16px", style "margin" "0" ] [ text submission.card ] ]
            )
            submissions
        )


viewCardButton : Maybe String -> Int -> String -> Html FrontendMsg
viewCardButton selectedCard index card =
    let
        isSelected =
            selectedCard == Just card
    in
    Html.li
        [ style "margin-bottom" "10px" ]
        [ Html.button
            [ onClick (CardSelected card)
            , id ("card-" ++ String.fromInt index)
            , style "padding" "15px"
            , style "width" "100%"
            , style "text-align" "left"
            , style "border" (if isSelected then "2px solid #4CAF50" else "1px solid #ccc")
            , style "background-color" (if isSelected then "#e8f5e9" else "white")
            , style "border-radius" "4px"
            , style "cursor" "pointer"
            , style "font-size" "14px"
            ]
            [ text card ]
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
