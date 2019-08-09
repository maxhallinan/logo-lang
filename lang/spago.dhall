{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "aff"
    , "console"
    , "control"
    , "effect"
    , "foldable-traversable"
    , "newtype"
    , "ordered-collections"
    , "parsing"
    , "psci-support"
    , "quickcheck"
    , "spec"
    , "strings"
    , "tuples"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
