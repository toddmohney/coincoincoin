module Blockocracy.Votes.VotingRulesForms
    exposing
        ( defForm
        , minimumQuorumLens
        , debatingPeriodInMinutesLens
        , majorityMarginLens
        )

import Forms.Model exposing (..)
import Monocle.Lens exposing (..)
import Blockocracy.Vote exposing (VotingRules)


defForm : Form VotingRules
defForm =
    Form defVotingRules []


defVotingRules : VotingRules
defVotingRules =
    VotingRules 0 0 0


minimumQuorumLens : Lens (Form VotingRules) Int
minimumQuorumLens =
    Lens minimumQuorumGetter minimumQuorumSetter


minimumQuorumGetter : Form VotingRules -> Int
minimumQuorumGetter f =
    f.model.minimumQuorum


minimumQuorumSetter : Int -> Form VotingRules -> Form VotingRules
minimumQuorumSetter val f =
    let
        formModel =
            f.model
    in
        { f | model = { formModel | minimumQuorum = val } }


debatingPeriodInMinutesLens : Lens (Form VotingRules) Int
debatingPeriodInMinutesLens =
    Lens debatingPeriodInMinutesGetter debatingPeriodInMinutesSetter


debatingPeriodInMinutesGetter : Form VotingRules -> Int
debatingPeriodInMinutesGetter f =
    f.model.debatingPeriodInMinutes


debatingPeriodInMinutesSetter : Int -> Form VotingRules -> Form VotingRules
debatingPeriodInMinutesSetter val f =
    let
        formModel =
            f.model
    in
        { f | model = { formModel | debatingPeriodInMinutes = val } }


majorityMarginLens : Lens (Form VotingRules) Int
majorityMarginLens =
    Lens majorityMarginGetter majorityMarginSetter


majorityMarginGetter : Form VotingRules -> Int
majorityMarginGetter f =
    f.model.majorityMargin


majorityMarginSetter : Int -> Form VotingRules -> Form VotingRules
majorityMarginSetter val f =
    let
        formModel =
            f.model
    in
        { f | model = { formModel | majorityMargin = val } }
