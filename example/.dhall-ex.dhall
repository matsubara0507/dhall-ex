{ GH_TOKEN =
    [ "matsubara0507:XXX" ] : Optional Text
, root =
    "configs"
, exports =
    [ { name =
          "example"
      , repo =
          [] : Optional Text
      , paths =
          [ "example.yaml" ]
      }
--    , { name =
--          "gh-example"
--      , repo =
--          [ "matsubara0507/dhall-ex-example" ] : Optional Text
--      , paths =
--          [ "example.yaml" ]
--      }
    ]
}
