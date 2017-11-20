port module Blockocracy.Ports
    exposing
        ( addMember
        , removeMember
        , submitProposal
        , submitVote
        )

import Blockocracy.Members.Model exposing (MemberRequest)
import Blockocracy.Proposal exposing (ProposalRequest)
import Blockocracy.Vote exposing (VoteRequest)


port submitProposal : ProposalRequest -> Cmd msg


port submitVote : VoteRequest -> Cmd msg


port addMember : MemberRequest -> Cmd msg


port removeMember : MemberRequest -> Cmd msg
