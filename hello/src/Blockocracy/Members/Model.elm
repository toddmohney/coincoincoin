module Blockocracy.Members.Model
    exposing
        ( Member
        , MemberRequest
        , accountLens
        , nameLens
        , toMemberRequest
        )

import Monocle.Lens exposing (..)
import Forms.Model as Form exposing (Form)
import Views.TxForm exposing (Tx)
import Web3.Web3 as Web3 exposing (AccountAddress(..))


type alias Member =
    { account : AccountAddress
    , name : String
    }


type alias MemberRequest =
    { senderAddress : String
    , gasPrice : Int
    , memberAddress : String
    , memberName : String
    }


toMemberRequest : Tx -> Member -> MemberRequest
toMemberRequest tx member =
    MemberRequest
        (Web3.getAccountAddress tx.senderAddress)
        tx.gasPrice
        (Web3.getAccountAddress member.account)
        member.name


accountLens : Lens (Form Member) AccountAddress
accountLens =
    Lens accountGetter accountSetter


accountGetter : Form Member -> AccountAddress
accountGetter f =
    f.model.account


accountSetter : AccountAddress -> Form Member -> Form Member
accountSetter addr f =
    let
        member =
            f.model
    in
        { f | model = { member | account = addr } }


nameLens : Lens (Form Member) String
nameLens =
    Lens nameGetter nameSetter


nameGetter : Form Member -> String
nameGetter f =
    f.model.name


nameSetter : String -> Form Member -> Form Member
nameSetter name f =
    let
        member =
            f.model
    in
        { f | model = { member | name = name } }
