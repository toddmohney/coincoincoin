module Forms.Model
    exposing
        ( Form
        , errorsLens
        )

import Monocle.Lens exposing (..)


type alias Form a =
    { model : a
    , errors : List String
    }


errorsLens : Lens (Form a) (List String)
errorsLens =
    Lens errorsGetter errorsSetter


errorsGetter : Form a -> List String
errorsGetter f =
    f.errors


errorsSetter : List String -> Form a -> Form a
errorsSetter errs f =
    let
        es =
            List.append f.errors errs
    in
        { f | errors = es }
