![CI](https://github.com/tweag/servant-template/actions/workflows/ci.yaml/badge.svg)

# servant-template

A modern template for a [Servant](https://haskell-servant.github.io/) application.

## Scope

The project aims to provide a template for a Servant project featuring:

- `nix` support via flakes.
- database interaction with [rel8](https://hackage.haskell.org/package/rel8);
- JWT authentication with [servant-auth](https://hackage.haskell.org/package/servant-auth);
- logging with [co-log-core](https://hackage.haskell.org/package/co-log-core);
- TOML configuration using [tomland](https://hackage.haskell.org/package/tomland);
- first class records in Servant API using [NamedRecords](https://hackage.haskell.org/package/servant-0.19/changelog).

## The application

The application allows users to categorify contents by tags. Any content can have many tags and any tag could be used for several contents.

It allows also to retrieve contents by a set of tags.

### Architecture

A more in depth description of the architecture of the application can be found in [ARCHITECTURE.md](./ARCHITECTURE.md).

### Configuration

Configuration of the application is managed using [TOML](https://toml.io). The application requires a configuration file with the following format:

```toml
[database]
  host     = "localhost"
  port     = 5432
  dbname   = "tagger-db"
  user     = "tagger-user"
  password = "tagger-pwd"

[api]
  port = 8080
```

By default, the file is located in `config.toml`, but the path is actually configurable with the `config` option.

### Authentication

The main endpoints of the application are protected by JWT authentication. To access them you first need to get an authorization token for a user.

To get it you first need to register a user by calling the `register` endpoint.

Next, you can obtain a token by calling the `login` endpoint with the same data provided to the register endpoint.

Eventually, you should pass your token in the `Authorization` header for the relevant endpoints to access them.

## Development

The project is setup to be built with [Cabal](https://cabal.readthedocs.io/en/latest/cabal-commands.html), though with dependencies provided via [Nix](https://nixos.org/).
Tasks are provided using a GNU Make-like tool called [Task](https://taskfile.dev/) and are supposed to be run inside a Nix shell.

Available tasks can be seen with `task --list`:

```sh
❯ task --list
task: Available tasks for this project:
* api:build:        Build the API
* api:dev:          Typecheck the API in a loop
* api:docs:         Build Haddock docs
* api:repl:         Start a cabal REPL
* api:serve:        Serve the API
* api:test:         Run API tests
* db:destroy:       Destroy the database
* db:setup:         Setup a postgres database using the config file
* fe:build:         Build the frontend app
* fe:serve:         Serve the frontend app
```

The usage of tasks is completely optional, and direct invocations of `cabal` (e.g. `cabal build`, `cabal test`, etc.) and `elm` commands are also valid.

> :information_source: If `direnv` is installed, it is also possible to use it with `nix`. The provided `.envrc` file is already configured to use `nix` and only needs to be enabled by issuing `direnv allow` in the project root once.

### Setup

Setting up the runtime dependencies for development, such as the database, is taken care of by `task setup`. This calls individual components' setup scripts such as `task db:setup` under the hood, if you prefer to call it directly.

This task (and others in general) assumes the existence of a few packages (`toml2json`, `jq`, `postgres`, etc.) provided by the nix shell.

#### Alternative: docker

In the root of the project you can find a `docker-compose.yml` file which provides a Postgresql database and a web interface to it, exposed on port `8081`.
You can initialise the schema of the database by running the `schema.sql` which is also provided.

### Building the API

To build the API, run

```sh
# With task
task api:build

# With cabal
cabal build
```

> :warning: Note for non-nix users: the build requires the presence of the `pg_config` executable which is made available by installing Postgresql. Nix takes care of this automatically.

### Serving the API for development

You can launch the web server using

```sh
# With task
task api:serve

### Running the API tests

To run the tests, run

```sh
# With task
task api:test

# With Cabal
cabal test
```

which will expose the service on port defined in configuration.

> :warning: Note for non-nix users: serving the API with hot-reloading requires the presence of the `watchexec` utility which is made available by the nix shell. Install it manually if you wish to use this script.

The executable accepts two options:

- `--config`, which allows to customize the path of the configuration file
- `--jwk`, which allows to customize the path of the file where the JWK is stored

### Formatting
The Haskell files are formatted using `ormolu`. The Elm source code is formatted using `elm-format`. The executables are provided in the nix shell.

### Pre-commit hooks
Git commit hooks are installed by the nix shell and run checks before a commit. These include linting, formatting, etc.

These checks can also be run manually with `pre-commit run`.

## Documentation

You can generate the documentation of the project using

```sh
# With task
task api:docs

# With Cabal
cabal haddock
```

### OpenApi documentation

You can access the OpenAPI documentation just by visiting the `docs` endpoint (by default http://localhost:8080/docs)

## Frontend

This repository contains also a client [Elm](https://elm-lang.org/) application to interact in a human-friendly way with the Tagger api.

You can find more details in [elm/README.md](elm/README.md), but there are convenience commands:

### Building the project
```sh
# With task
task fe:build

# With nix
nix-shell --run 'cd elm; elm make src/Main.elm'

# With npm
cd elm; npx elm make src/Main.elm
```

### Serve project for development
```sh
# With task
task fe:serve

# With nix
nix-shell --run 'cd elm; elm-live src/Main.elm'

# With npm
cd elm; npx elm-live src/Main.elm
```
