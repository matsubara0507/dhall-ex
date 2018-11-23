# dhall-ex

Dhall + exports

dhall-ex is cli tool to support dhall.

```
$ dhall-ex --help
Usage: dhall-ex [-v|--verbose] [-c|--config PATH] COMMAND [--version]

Available options:
  -v,--verbose             Enable verbose mode: verbosity level "debug"
  -c,--config PATH         Configuration file
  --version                Show version
  -h,--help                Show this help text

Available commands:
  sort                     Sort record keys in dhall file
  echo                     Echo TEXT
  init                     Init dhall-ex work directory
  build                    Build Dhall file to YAML or JSON
  deploy                   Deploy builded config file to remote repository
```

## Requirement

- [stack](https://docs.haskellstack.org/)
- [dhall](https://github.com/dhall-lang/dhall-lang) (optional)

## Install

```
$ stack install
```

## Usage
