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
    - dhall-ex use dhall-1.5.1

## Install

Use Haskell Stack:

```
$ stack install
```

## Usage

Write dhall-ex's config file `.dhall-ex.dhall` typed by [`dhall/dhall-ex-type.dhall`](dhall/dhall-ex-type.dhall).
Exec echo subcommand to type check dhall's config file:

```
$ dhall-ex echo hoge
hoge
```

Initialize dhall workspace with dhall's config file:

```
$ dhall-ex init
```

Build all config files that be wanted to export with dhall's config file:

```
$ dhall-ex build
```

Commit and Push builded config files if builded config file is in GitHub repository:

```
$ dhall-ex deploy -b BRANCH
```
