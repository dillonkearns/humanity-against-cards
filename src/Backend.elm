module Backend exposing (app, app_, init)

import Dict
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Subscription as Subscription
import Lamdera
import Set exposing (Set)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Effect.Lamdera.backend
        Lamdera.broadcast
        Lamdera.sendToFrontend
        app_


app_ =
    { init = init
    , update = update
    , updateFromFrontend = updateFromFrontend
    , subscriptions = subscriptions
    }


init : ( Model, Command BackendOnly ToFrontend BackendMsg )
init =
    ( { counter = 0
      , deck = Nothing
      , players = Dict.empty
      , gameState = Lobby
      }
    , Command.none
    )


update : BackendMsg -> Model -> ( Model, Command BackendOnly ToFrontend BackendMsg )
update msg model =
    case msg of
        ClientConnected sessionId clientId ->
            let
                playersList =
                    Dict.values model.players
            in
            ( model
            , Command.batch
                [ Effect.Lamdera.sendToFrontend clientId <| CounterNewValue model.counter clientId
                , Effect.Lamdera.sendToFrontend clientId <| PlayersListUpdated playersList
                , Effect.Lamdera.sendToFrontend clientId <| GameStateUpdated model.gameState
                , case model.deck of
                    Just deck ->
                        Effect.Lamdera.sendToFrontend clientId <| DeckLoaded deck

                    Nothing ->
                        Command.none
                ]
            )

        Noop ->
            ( model, Command.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        CounterIncremented ->
            let
                newCounter =
                    model.counter + 1
            in
            ( { model | counter = newCounter }
            , Effect.Lamdera.broadcast (CounterNewValue newCounter clientId)
            )

        CounterDecremented ->
            let
                newCounter =
                    model.counter - 1
            in
            ( { model | counter = newCounter }
            , Effect.Lamdera.broadcast (CounterNewValue newCounter clientId)
            )

        LoadDeck deck ->
            ( { model | deck = Just deck }
            , Effect.Lamdera.broadcast (DeckLoaded deck)
            )

        JoinGame (PlayerToken tokenStr) name ->
            let
                player =
                    { token = PlayerToken tokenStr
                    , name = name
                    , hand = []
                    , score = 0
                    , clientId = Just clientId
                    }

                newPlayers =
                    Dict.insert tokenStr player model.players

                playersList =
                    Dict.values newPlayers
            in
            ( { model | players = newPlayers }
            , Command.batch
                [ Effect.Lamdera.sendToFrontend clientId (PlayerJoined player)
                , Effect.Lamdera.broadcast (PlayersListUpdated playersList)
                ]
            )

        StartGame ->
            case model.deck of
                Nothing ->
                    -- Can't start without a deck
                    ( model, Command.none )

                Just deck ->
                    if Dict.isEmpty model.players then
                        -- Can't start without players
                        ( model, Command.none )

                    else
                        let
                            -- Get first player as judge
                            firstJudge =
                                Dict.values model.players
                                    |> List.head
                                    |> Maybe.map .token
                                    |> Maybe.withDefault (PlayerToken "")

                            -- Deal 10 cards to each player
                            ( playersWithHands, remainingAnswers ) =
                                dealCardsToPlayers (Dict.values model.players) deck.answers []

                            newPlayers =
                                playersWithHands
                                    |> List.map (\p -> ( tokenToString p.token, p ))
                                    |> Dict.fromList

                            newGameState =
                                Playing
                                    { currentJudge = firstJudge
                                    , remainingAnswers = remainingAnswers
                                    , remainingPrompts = deck.prompts
                                    }

                            -- Send each player their hand
                            playerCommands =
                                Dict.values newPlayers
                                    |> List.filterMap
                                        (\player ->
                                            case player.clientId of
                                                Just cid ->
                                                    Just
                                                        (Effect.Lamdera.sendToFrontend cid
                                                            (GameStarted
                                                                { yourHand = player.hand
                                                                , currentJudge = firstJudge
                                                                }
                                                            )
                                                        )

                                                Nothing ->
                                                    Nothing
                                        )

                            commands =
                                Command.batch
                                    (Effect.Lamdera.broadcast (GameStateUpdated newGameState) :: playerCommands)
                        in
                        ( { model
                            | players = newPlayers
                            , gameState = newGameState
                          }
                        , commands
                        )


-- HELPER FUNCTIONS


tokenToString : PlayerToken -> String
tokenToString (PlayerToken str) =
    str


dealCardsToPlayers : List Player -> List String -> List Player -> ( List Player, List String )
dealCardsToPlayers players availableCards accum =
    case players of
        [] ->
            ( List.reverse accum, availableCards )

        player :: rest ->
            let
                ( hand, remaining ) =
                    ( List.take 10 availableCards, List.drop 10 availableCards )

                updatedPlayer =
                    { player | hand = hand }
            in
            dealCardsToPlayers rest remaining (updatedPlayer :: accum)


subscriptions model =
    Subscription.batch
        [ Effect.Lamdera.onConnect ClientConnected
        ]
