module Blockocracy.Members.Model
    exposing
        ( Member
        , accountLens
        , nameLens
        )

import Monocle.Lens exposing (..)
import Forms.Model as Form exposing (Form)
import Web3.Web3 as Web3 exposing (AccountAddress(..))


type alias Member =
    { account : AccountAddress
    , name : String
    }


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
