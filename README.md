# dhall-ex

Dhall + exports

dhall-ex is cli tool to support dhall.

```
$ dhall-ex --help
dhall-ex - cli tool to support dhall

Usage: dhall-ex [-v|--verbose] [-c|--config PATH] [--only NAME] COMMAND
                [--version]

Available options:
  -v,--verbose             Enable verbose mode: verbosity level "debug"
  -c,--config PATH         Configuration file
  --only NAME              Exec subcommand only NAME in config
  --version                Show version
  -h,--help                Show this help text

Available commands:
  sort                     Sort record keys in dhall file
  echo                     Echo TEXT
  init                     Init dhall-ex work directory
  build                    Build Dhall file to YAML or JSON
  deploy                   Deploy builded config file to remote repository
  checkout                 Checkout repository in dhall workspace
  pull                     Pull repository in dhall workspace
```

## Requirement

- [Haskell Stack](https://docs.haskellstack.org/) or Docker
- [dhall](https://github.com/dhall-lang/dhall-lang) (optional)
    - dhall-ex use dhall-1.5.1

## Install

Use Haskell Stack:

```
$ stack install
```

or use docker:

```
$ docker pull matsubara0507/dhall-ex
```

## Usage

Write dhall-ex's config file `.dhall-ex.dhall` typed by [`dhall/dhall-ex-type.dhall`](dhall/dhall-ex-type.dhall).
Exec echo subcommand to type check dhall's config file:

```
$ dhall-ex echo hoge
hoge
```

If use docker:

```
$ docker run --rm -v `pwd`:/work matsubara0507/dhall-ex /bin/bash -c "cd work && dhall-ex echo 'hello docker'"
hello docker
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
