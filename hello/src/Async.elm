module Async
    exposing
        ( External(..)
        )


type External a
    = Loaded a
    | Loading
    | LoadError String
    | NotLoaded
