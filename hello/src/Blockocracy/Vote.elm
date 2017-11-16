module Blockocracy.Vote
    exposing
        ( Vote
        , proposalNumberLens
        , proposalSupportLens
        , supportJustificationLens
        , defForm
        )

import Forms.Model exposing (..)
import Monocle.Lens exposing (..)


type alias Vote =
    { proposalNumber : Int
    , proposalSupport : Bool
    , supportJustification : String
    }


defForm : Form Vote
defForm =
    Form defVote []


defVote : Vote
defVote =
    Vote 0 False ""


proposalNumberLens : Lens (Form Vote) Int
proposalNumberLens =
    Lens proposalNumberGetter proposalNumberSetter


proposalNumberGetter : Form Vote -> Int
proposalNumberGetter f =
    f.model.proposalNumber


proposalNumberSetter : Int -> Form Vote -> Form Vote
proposalNumberSetter val f =
    let
        formModel =
            f.model
    in
        { f | model = { formModel | proposalNumber = val } }


proposalSupportLens : Lens (Form Vote) Bool
proposalSupportLens =
    Lens proposalSupportGetter proposalSupportSetter


proposalSupportGetter : Form Vote -> Bool
proposalSupportGetter f =
    f.model.proposalSupport


proposalSupportSetter : Bool -> Form Vote -> Form Vote
proposalSupportSetter val f =
    let
        formModel =
            f.model
    in
        { f | model = { formModel | proposalSupport = val } }


supportJustificationLens : Lens (Form Vote) String
supportJustificationLens =
    Lens supportJustificationGetter supportJustificationSetter


supportJustificationGetter : Form Vote -> String
supportJustificationGetter f =
    f.model.supportJustification


supportJustificationSetter : String -> Form Vote -> Form Vote
supportJustificationSetter val f =
    let
        formModel =
            f.model
    in
        { f | model = { formModel | supportJustification = val } }
