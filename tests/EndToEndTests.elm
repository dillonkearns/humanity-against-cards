module EndToEndTests exposing (main, tests)

import Backend
import Effect.Browser.Dom as Dom
import Effect.Command as Command exposing (BackendOnly, Command, FrontendOnly)
import Effect.Http exposing (Response(..))
import Effect.Lamdera
import Effect.Subscription as Subscription
import Effect.Test exposing (FileUpload(..), HttpResponse(..), MultipleFilesUpload(..))
import Effect.Time
import Expect
import Frontend
import Html
import Json.Encode as Encode
import Test.Html.Query
import Test.Html.Selector
import Types exposing (BackendModel, BackendMsg, FrontendModel, FrontendMsg, ToBackend, ToFrontend)
import Url exposing (Url)


unsafeUrl : Url
unsafeUrl =
    case Url.fromString "https://chat-app.lamdera.app" of
        Just url ->
            url

        Nothing ->
            Debug.todo "Invalid url"


config : Effect.Test.Config ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
config =
    { frontendApp = Frontend.app_
    , backendApp = Backend.app_
    , handleHttpRequest = always NetworkErrorResponse
    , handlePortToJs = always Nothing
    , handleFileUpload = always UnhandledFileUpload
    , handleMultipleFilesUpload = always UnhandledMultiFileUpload
    , domain = unsafeUrl
    }


sampleDeckJson : String
sampleDeckJson =
    """{"prompts":["J.K Rowling: Harry Potter and the Chamber of _.","Moms love _."],"answers":["A wildly misguided plan with strong confidence.","Free samples.","Unresolved childhood trauma.","A concerning amount of confidence.","Aggressive finger guns.","The exact wrong energy for this situation.","A solution that creates three new problems.","Getting so angry that you pop a boner.","Oral sex that tastes like urine.","$1.25 per day","$10 bills defaced with anarchist propaganda.","$10,000 worth of silica gel.","$13 worth of Taco Bell","$15 minimum wage.","$16,000 in cash, a kilo of hashish, and eight belly dancers.","$2 beer pitcher night at the base bowling alley.","$20 Tinnies","$20 tinnies","$200/hour","$300 worth of vanilla yogurt."]}"""


tests : List (Effect.Test.EndToEndTest ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
tests =
    [ Effect.Test.start
        "Admin can start game and players receive hands"
        (Effect.Time.millisToPosix 0)
        config
        [ Effect.Test.connectFrontend
            100
            (Effect.Lamdera.sessionIdFromString "adminSession")
            "/admin"
            { width = 800, height = 600 }
            (\admin ->
                [ admin.input 100 (Dom.id "deck-json-input") sampleDeckJson
                , admin.click 100 (Dom.id "load-deck-button")
                , Effect.Test.connectFrontend
                    100
                    (Effect.Lamdera.sessionIdFromString "player1Session")
                    "/"
                    { width = 800, height = 600 }
                    (\player1 ->
                        [ player1.input 100 (Dom.id "player-name-input") "Alice"
                        , player1.click 100 (Dom.id "join-game-button")
                        , Effect.Test.connectFrontend
                            100
                            (Effect.Lamdera.sessionIdFromString "player2Session")
                            "/"
                            { width = 800, height = 600 }
                            (\player2 ->
                                [ player2.input 100 (Dom.id "player-name-input") "Bob"
                                , player2.click 100 (Dom.id "join-game-button")
                                , admin.click 100 (Dom.id "start-game-button")
                                , player1.checkView 100
                                    (Test.Html.Query.has [ Test.Html.Selector.text "You are the judge this round!" ])
                                , player2.checkView 100
                                    (Test.Html.Query.find [ Test.Html.Selector.id "player-hand" ]
                                        >> Test.Html.Query.findAll [ Test.Html.Selector.tag "li" ]
                                        >> Test.Html.Query.count (Expect.equal 10)
                                    )
                                ]
                            )
                        ]
                    )
                ]
            )
        ]
    , Effect.Test.start
        "Players can join game and appear in admin console"
        (Effect.Time.millisToPosix 0)
        config
        [ Effect.Test.connectFrontend
            100
            (Effect.Lamdera.sessionIdFromString "player1Session")
            "/"
            { width = 800, height = 600 }
            (\player1 ->
                [ player1.checkView 100
                    (Test.Html.Query.has [ Test.Html.Selector.text "Join Game" ])
                , player1.input 100 (Dom.id "player-name-input") "Alice"
                , player1.click 100 (Dom.id "join-game-button")
                , player1.checkView 100
                    (Test.Html.Query.has [ Test.Html.Selector.text "Welcome, Alice!" ])
                , player1.checkView 100
                    (Test.Html.Query.find [ Test.Html.Selector.id "player-joined-message" ]
                        >> Test.Html.Query.has [ Test.Html.Selector.text "Waiting for game to start..." ]
                    )
                , Effect.Test.connectFrontend
                    100
                    (Effect.Lamdera.sessionIdFromString "player2Session")
                    "/"
                    { width = 800, height = 600 }
                    (\player2 ->
                        [ player2.input 100 (Dom.id "player-name-input") "Bob"
                        , player2.click 100 (Dom.id "join-game-button")
                        , player2.checkView 100
                            (Test.Html.Query.has [ Test.Html.Selector.text "Welcome, Bob!" ])
                        , Effect.Test.connectFrontend
                            100
                            (Effect.Lamdera.sessionIdFromString "adminSession")
                            "/admin"
                            { width = 800, height = 600 }
                            (\admin ->
                                [ admin.checkView 100
                                    (Test.Html.Query.has [ Test.Html.Selector.text "Joined Players" ])
                                , admin.checkView 100
                                    (Test.Html.Query.find [ Test.Html.Selector.id "players-list" ]
                                        >> Test.Html.Query.has [ Test.Html.Selector.text "Alice" ]
                                    )
                                , admin.checkView 100
                                    (Test.Html.Query.find [ Test.Html.Selector.id "players-list" ]
                                        >> Test.Html.Query.has [ Test.Html.Selector.text "Bob" ]
                                    )
                                ]
                            )
                        ]
                    )
                ]
            )
        ]
    , Effect.Test.start
        "Admin can load deck and it broadcasts to all clients"
        (Effect.Time.millisToPosix 0)
        config
        [ Effect.Test.connectFrontend
            100
            (Effect.Lamdera.sessionIdFromString "adminSession")
            "/admin"
            { width = 800, height = 600 }
            (\admin ->
                [ admin.checkView 100
                    (Test.Html.Query.has [ Test.Html.Selector.text "Admin Console" ])
                , admin.input 100 (Dom.id "deck-json-input") sampleDeckJson
                , admin.click 100 (Dom.id "load-deck-button")
                , admin.checkView 100
                    (Test.Html.Query.find [ Test.Html.Selector.id "prompts-list" ]
                        >> Test.Html.Query.has [ Test.Html.Selector.text "J.K Rowling: Harry Potter and the Chamber of _." ]
                    )
                , admin.checkView 100
                    (Test.Html.Query.find [ Test.Html.Selector.id "answers-list" ]
                        >> Test.Html.Query.has [ Test.Html.Selector.text "Free samples." ]
                    )
                , Effect.Test.connectFrontend
                    100
                    (Effect.Lamdera.sessionIdFromString "playerSession")
                    "/"
                    { width = 800, height = 600 }
                    (\player ->
                        [ player.checkView 100 (Test.Html.Query.has [ Test.Html.Selector.text "Humanity Against Cards" ])
                        ]
                    )
                ]
            )
        ]
    ]


main : Program () (Effect.Test.Model ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel) (Effect.Test.Msg ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
main =
    Effect.Test.viewer tests
