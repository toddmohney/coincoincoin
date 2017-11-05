port module Ports exposing (getHelloCount, sayHello)

port getHelloCount : String -> Cmd msg

port sayHello : String -> Cmd msg
