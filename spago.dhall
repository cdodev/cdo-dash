{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "cdo-dash"
, dependencies =
    [ "prelude"
    , "console"
    , "effect"
    , "variant"
    , "nonempty"
    , "aff"
    , "graphql"
    , "halogen"
    , "halogen-formless"
    , "remotedata"
    , "routing"
    , "formatters"
    , "routing-duplex"
    , "now"
    , "affjax"
    , "slug"
    , "precise-datetime"
    , "typelevel-prelude"
    , "argonaut-core"
    , "argonaut-codecs"
    , "aff-bus"
    , "struct"
    , "tolerant-argonaut"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}