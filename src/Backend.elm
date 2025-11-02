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


subscriptions model =
    Subscription.batch
        [ Effect.Lamdera.onConnect ClientConnected
        ]
