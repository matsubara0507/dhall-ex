{ GH_TOKEN =
    [] : Optional Text
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
    ]
}
