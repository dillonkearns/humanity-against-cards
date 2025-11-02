module EndToEndTests exposing (main, tests)

import Backend
import Effect.Browser.Dom as Dom
import Effect.Command as Command exposing (BackendOnly, Command, FrontendOnly)
import Effect.Http exposing (Response(..))
import Effect.Lamdera
import Effect.Subscription as Subscription
import Effect.Test exposing (FileUpload(..), HttpResponse(..), MultipleFilesUpload(..))
import Effect.Time
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
    """{"prompts":["J.K Rowling: Harry Potter and the Chamber of ____.","Moms love ____."],"answers":["A wildly misguided plan with strong confidence.","Free samples."]}"""


tests : List (Effect.Test.EndToEndTest ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
tests =
    [ Effect.Test.start
        "Clients stay in sync"
        (Effect.Time.millisToPosix 0)
        config
        [ Effect.Test.connectFrontend
            100
            (Effect.Lamdera.sessionIdFromString "sessionId0")
            "/"
            { width = 800, height = 600 }
            (\client1 ->
                [ client1.click 100 (Dom.id "plusOne")
                , client1.click 100 (Dom.id "plusOne")
                , client1.click 100 (Dom.id "plusOne")
                , client1.checkView 100 (Test.Html.Query.has [ Test.Html.Selector.exactText "3" ])
                , Effect.Test.connectFrontend
                    100
                    (Effect.Lamdera.sessionIdFromString "sessionId1")
                    "/"
                    { width = 800, height = 600 }
                    (\client2 ->
                        [ client2.checkView 100 (Test.Html.Query.has [ Test.Html.Selector.exactText "3" ])
                        , client2.click 100 (Dom.id "minusOne")
                        , client1.checkView 100 (Test.Html.Query.has [ Test.Html.Selector.exactText "2" ])
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
                        >> Test.Html.Query.has [ Test.Html.Selector.text "J.K Rowling: Harry Potter and the Chamber of ____." ]
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
