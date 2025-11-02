module DeckBuilder exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.File
import BackendTask.Random
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import FatalError exposing (FatalError)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)
import Random
import Random.List


run : Script
run =
    Script.withCliOptions program
        (\{ promptCount, answerCount } ->
            loadCahCards
                |> BackendTask.andThen
                    (\{ whites, blacks } ->
                        let
                            singleBlankPrompts : List String
                            singleBlankPrompts =
                                filterSingleBlankPrompts blacks
                        in
                        BackendTask.map2 buildDeckJson
                            (selectRandomCards promptCount singleBlankPrompts)
                            (selectRandomCards answerCount whites)
                            |> BackendTask.andThen
                                (\outputJson ->
                                    Script.log (Encode.encode 2 outputJson)
                                )
                    )
        )


{-| Load the Cards Against Humanity JSON file from the local repo
-}
loadCahCards : BackendTask FatalError { whites : List String, blacks : List BlackCard }
loadCahCards =
    BackendTask.File.jsonFile
        (Decode.field "white" (Decode.list Decode.string)
            |> Decode.andThen
                (\whites ->
                    Decode.field "black" (Decode.list blackCardDecoder)
                        |> Decode.map (\blacks -> { whites = whites, blacks = blacks })
                )
        )
        "../../../crhallberg/json-against-humanity/cah-all-compact.json"
        |> BackendTask.allowFatal


{-| Filter black cards to only include single-blank prompts (pick=1)
-}
filterSingleBlankPrompts : List BlackCard -> List String
filterSingleBlankPrompts blacks =
    blacks
        |> List.filter (\card -> card.pick == 1)
        |> List.map .text


{-| Shuffle cards and select a random subset
-}
selectRandomCards : Int -> List String -> BackendTask error (List String)
selectRandomCards count cards =
    Random.List.shuffle cards
        |> Random.map (List.take count)
        |> BackendTask.Random.generate


{-| Build the final JSON output in our deck format
-}
buildDeckJson : List String -> List String -> Encode.Value
buildDeckJson prompts answers =
    Encode.object
        [ ( "prompts", Encode.list Encode.string prompts )
        , ( "answers", Encode.list Encode.string answers )
        ]


type alias BlackCard =
    { text : String
    , pick : Int
    }


blackCardDecoder : Decoder BlackCard
blackCardDecoder =
    Decode.map2 BlackCard
        (Decode.field "text" Decode.string)
        (Decode.field "pick" Decode.int)


type alias CliOptions =
    { promptCount : Int
    , answerCount : Int
    }


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.optionalKeywordArg "prompts"
                        |> Option.validateMap
                            (\maybeStr ->
                                case maybeStr of
                                    Nothing ->
                                        Ok 30

                                    Just str ->
                                        String.toInt str
                                            |> Result.fromMaybe "Must be an integer"
                            )
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "answers"
                        |> Option.validateMap
                            (\maybeStr ->
                                case maybeStr of
                                    Nothing ->
                                        Ok 150

                                    Just str ->
                                        String.toInt str
                                            |> Result.fromMaybe "Must be an integer"
                            )
                    )
            )
