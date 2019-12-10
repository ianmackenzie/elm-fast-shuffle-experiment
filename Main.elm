module Main exposing (main)

import Benchmark
import Benchmark.Runner exposing (BenchmarkProgram)
import Random
import Random.List


randomInt : Random.Generator Int
randomInt =
    Random.int Random.minInt Random.maxInt


fastShuffle : List a -> Random.Generator (List a)
fastShuffle items =
    Random.independentSeed
        |> Random.map
            (\seed ->
                let
                    taggedItems =
                        decorate items seed []

                    shuffled =
                        List.sortBy Tuple.first taggedItems
                in
                undecorate shuffled []
            )


decorate : List a -> Random.Seed -> List ( Int, a ) -> List ( Int, a )
decorate items currentSeed accumulatedTaggedItems =
    case items of
        firstItem :: remainingItems ->
            let
                ( tag, updatedSeed ) =
                    Random.step randomInt currentSeed

                taggedItem =
                    ( tag, firstItem )
            in
            decorate remainingItems
                updatedSeed
                (taggedItem :: accumulatedTaggedItems)

        [] ->
            accumulatedTaggedItems


undecorate : List ( Int, a ) -> List a -> List a
undecorate taggedItems accumulatedItems =
    case taggedItems of
        ( tag, firstItem ) :: remainingTaggedItems ->
            undecorate remainingTaggedItems (firstItem :: accumulatedItems)

        [] ->
            accumulatedItems


main : BenchmarkProgram
main =
    let
        initialList =
            List.range 1 100
    in
    Benchmark.Runner.program <|
        Benchmark.compare "List shuffling"
            "Random.List.shuffle"
            (\() -> Random.step (Random.List.shuffle initialList) (Random.initialSeed 1))
            "fastShuffle"
            (\() -> Random.step (fastShuffle initialList) (Random.initialSeed 1))
