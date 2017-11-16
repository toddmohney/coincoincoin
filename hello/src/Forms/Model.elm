module Forms.Model
    exposing
        ( Form
        , modelLens
        , errorsLens
        )

import Monocle.Lens exposing (..)


type alias Form a =
    { model : a
    , errors : List String
    }


modelLens : Lens (Form a) a
modelLens =
    Lens modelGetter modelSetter


modelGetter : Form a -> a
modelGetter f =
    f.model


modelSetter : a -> Form a -> Form a
modelSetter m f =
    { f | model = m }


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
