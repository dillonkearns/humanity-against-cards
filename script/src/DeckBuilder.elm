module DeckBuilder exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.File
import BackendTask.Random
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)
import Random
import Random.List


run : Script
run =
    Script.withCliOptions program
        (\{ promptCount, answerCount } ->
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
                |> BackendTask.andThen
                    (\{ whites, blacks } ->
                        let
                            -- Filter to only pick=1 prompts
                            singlePickBlacks =
                                blacks
                                    |> List.filter (\card -> card.pick == 1)
                                    |> List.map .text

                            -- Convert _ to ____
                            normalizedPrompts =
                                singlePickBlacks
                                    |> List.map (String.replace "_" "____")

                            -- Shuffle and take
                            promptsGenerator =
                                Random.List.shuffle normalizedPrompts
                                    |> Random.map (List.take promptCount)

                            answersGenerator =
                                Random.List.shuffle whites
                                    |> Random.map (List.take answerCount)
                        in
                        BackendTask.map2
                            (\selectedPrompts selectedAnswers ->
                                Encode.object
                                    [ ( "prompts", Encode.list Encode.string selectedPrompts )
                                    , ( "answers", Encode.list Encode.string selectedAnswers )
                                    ]
                            )
                            (BackendTask.Random.generate promptsGenerator)
                            (BackendTask.Random.generate answersGenerator)
                            |> BackendTask.andThen
                                (\outputJson ->
                                    Script.log (Encode.encode 2 outputJson)
                                )
                    )
        )


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
