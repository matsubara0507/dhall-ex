{ apiVersion =
    "v1"
, kind =
    "Service"
, spec =
    { selector =
        { app = "MyApp" }
    , ports =
        [ { targetPort = 80.0, protocol = "TCP", port = 80.0 } ]
    }
, metadata =
    { name = "my-service" }
}
