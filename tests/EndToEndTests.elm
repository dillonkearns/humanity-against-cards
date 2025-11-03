module EndToEndTests exposing (main, suite, tests)

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
import Test exposing (Test)
import Test.Html.Query
import Test.Html.Selector
import Types exposing (BackendModel, BackendMsg, FrontendModel, FrontendMsg, ToBackend, ToFrontend)
import Url exposing (Url)


suite : Test
suite =
    tests
        |> List.map Effect.Test.toTest
        |> Test.describe "suite"


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
    """{"prompts":["J.K Rowling: Harry Potter and the Chamber of _.","Moms love _."],"answers":["A wildly misguided plan with strong confidence.","Free samples.","Unresolved childhood trauma.","A concerning amount of confidence.","Aggressive finger guns.","The exact wrong energy for this situation.","A solution that creates three new problems.","Getting so angry that you pop a boner.","Oral sex that tastes like urine.","$1.25 per day","$10 bills defaced with anarchist propaganda.","$10,000 worth of silica gel.","$13 worth of Taco Bell","$15 minimum wage.","$16,000 in cash, a kilo of hashish, and eight belly dancers.","$2 beer pitcher night at the base bowling alley.","$20 Tinnies","$20 tinnies","$200/hour","$300 worth of vanilla yogurt.","An unstoppable freight train of poor decisions.","The forbidden snack.","Emotional damage.","A concerning lack of adult supervision.","That one friend who ruins everything.","The audacity.","Chaos incarnate.","A spectacularly bad idea.","Unfiltered confidence.","The consequences of my actions."]}"""


tests : List (Effect.Test.EndToEndTest ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
tests =
    [ Effect.Test.start
        "Complete round flow: submit, reveal, judge, score"
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
                                , Effect.Test.connectFrontend
                                    100
                                    (Effect.Lamdera.sessionIdFromString "player3Session")
                                    "/"
                                    { width = 800, height = 600 }
                                    (\player3 ->
                                        [ player3.input 100 (Dom.id "player-name-input") "Charlie"
                                        , player3.click 100 (Dom.id "join-game-button")
                                        -- Verify Charlie joined successfully
                                        , player3.checkView 100
                                            (Test.Html.Query.has [ Test.Html.Selector.text "Welcome, Charlie!" ])
                                        -- Verify admin sees all 3 players before starting
                                        , admin.checkView 100
                                            (Test.Html.Query.has [ Test.Html.Selector.text "Charlie" ])
                                        , admin.click 100 (Dom.id "start-game-button")

                                        -- SUBMISSION PHASE
                                        -- Alice is the judge (with clientId tokens, first connection wins)
                                        , player1.checkView 100
                                            (Test.Html.Query.has [ Test.Html.Selector.text "You are the judge this round!" ])

                                        -- Bob and Charlie submit cards
                                        , player2.click 100 (Dom.id "card-0")
                                        , player2.click 100 (Dom.id "submit-card-button")
                                        , player2.checkView 100
                                            (Test.Html.Query.has [ Test.Html.Selector.text "✓ Card submitted!" ])

                                        , player3.click 100 (Dom.id "card-0")
                                        , player3.click 100 (Dom.id "submit-card-button")
                                        -- After Charlie submits, all non-judges have submitted, so auto-transition to reveal

                                        -- REVEAL PHASE (auto-transition after all submit)
                                        -- Alice (judge) sees reveal controls
                                        , player1.checkView 100
                                            (Test.Html.Query.has [ Test.Html.Selector.text "Reveal Card 1" ])

                                        -- Alice (judge) reveals first card
                                        , player1.click 100 (Dom.id "reveal-next-button")
                                        , player2.checkView 100
                                            (Test.Html.Query.has [ Test.Html.Selector.text "1 of 2 cards revealed" ])

                                        -- Alice (judge) reveals second card
                                        , player1.click 100 (Dom.id "reveal-next-button")

                                        -- JUDGING PHASE (auto-transition after all revealed)
                                        -- Alice (judge) sees winner selection
                                        , player1.checkView 100
                                            (Test.Html.Query.has [ Test.Html.Selector.text "Select the Winner!" ])

                                        -- Verify non-judges see all cards
                                        , player2.checkView 100
                                            (Test.Html.Query.has [ Test.Html.Selector.text "All cards revealed!" ])

                                        -- Alice (judge) selects winner
                                        , player1.click 100 (Dom.id "select-winner-0")

                                        -- WINNER ANNOUNCEMENT
                                        -- All players see the winner announcement
                                        , player1.checkView 100
                                            (Test.Html.Query.find [ Test.Html.Selector.id "winner-announcement" ]
                                                >> Test.Html.Query.has [ Test.Html.Selector.text "wins this round!" ]
                                            )
                                        , player2.checkView 100
                                            (Test.Html.Query.has [ Test.Html.Selector.text "Round Complete!" ])
                                        , player3.checkView 100
                                            (Test.Html.Query.has [ Test.Html.Selector.text "Round Complete!" ])

                                        -- Verify scoreboard shows updated scores
                                        , player1.checkView 100
                                            (Test.Html.Query.find [ Test.Html.Selector.id "scoreboard" ]
                                                >> Test.Html.Query.has [ Test.Html.Selector.text "pts" ]
                                            )
                                        ]
                                    )
                                ]
                            )
                        ]
                    )
                ]
            )
        ]
    , Effect.Test.start
        "Players can submit cards after game starts"
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

                                -- Verify judge sees the prompt
                                , player1.checkView 100
                                    (Test.Html.Query.find [ Test.Html.Selector.id "current-prompt" ]
                                        >> Test.Html.Query.has [ Test.Html.Selector.text "J.K Rowling: Harry Potter and the Chamber of _." ]
                                    )
                                , player1.checkView 100
                                    (Test.Html.Query.has [ Test.Html.Selector.text "You are the judge this round!" ])

                                -- Verify non-judge sees prompt and cards
                                , player2.checkView 100
                                    (Test.Html.Query.find [ Test.Html.Selector.id "current-prompt" ]
                                        >> Test.Html.Query.has [ Test.Html.Selector.text "J.K Rowling: Harry Potter and the Chamber of _." ]
                                    )
                                , player2.checkView 100
                                    (Test.Html.Query.find [ Test.Html.Selector.id "player-hand" ]
                                        >> Test.Html.Query.findAll [ Test.Html.Selector.tag "button" ]
                                        >> Test.Html.Query.count (Expect.equal 10)
                                    )

                                -- Player2 selects and submits a card
                                , player2.click 100 (Dom.id "card-0")
                                , player2.click 100 (Dom.id "submit-card-button")

                                -- With only 2 players (1 judge, 1 non-judge), submission auto-transitions to reveal
                                -- Verify we're now in reveal phase
                                , player2.checkView 100
                                    (Test.Html.Query.has [ Test.Html.Selector.text "Revealing cards..." ])

                                -- Verify judge sees reveal button
                                , player1.checkView 100
                                    (Test.Html.Query.find [ Test.Html.Selector.id "reveal-next-button" ]
                                        >> Test.Html.Query.has [ Test.Html.Selector.text "Reveal Card 1" ]
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
    , Effect.Test.start
        "Admin can remove players before game starts"
        (Effect.Time.millisToPosix 0)
        config
        [ Effect.Test.connectFrontend
            100
            (Effect.Lamdera.sessionIdFromString "adminSession")
            "/admin"
            { width = 800, height = 600 }
            (\admin ->
                [ Effect.Test.connectFrontend
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

                                -- Admin should see both players
                                , admin.checkView 100
                                    (Test.Html.Query.find [ Test.Html.Selector.id "players-list" ]
                                        >> Test.Html.Query.has [ Test.Html.Selector.text "Alice" ]
                                    )
                                , admin.checkView 100
                                    (Test.Html.Query.find [ Test.Html.Selector.id "players-list" ]
                                        >> Test.Html.Query.has [ Test.Html.Selector.text "Bob" ]
                                    )

                                -- Admin removes Alice
                                , admin.click 100 (Dom.id "remove-player-player1Session")

                                -- Admin should only see Bob now
                                , admin.checkView 100
                                    (Test.Html.Query.find [ Test.Html.Selector.id "players-list" ]
                                        >> Test.Html.Query.has [ Test.Html.Selector.text "Bob" ]
                                    )

                                -- Alice should no longer be in the list (only 1 player visible)
                                , admin.checkView 100
                                    (Test.Html.Query.find [ Test.Html.Selector.id "players-list" ]
                                        >> Test.Html.Query.children []
                                        >> Test.Html.Query.count (Expect.equal 1)
                                    )
                                ]
                            )
                        ]
                    )
                ]
            )
        ]
    ]


main : Program () (Effect.Test.Model ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel) (Effect.Test.Msg ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
main =
    Effect.Test.viewer tests
