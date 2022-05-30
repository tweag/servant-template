# servant-template

A modern template for a [Servant](https://haskell-servant.github.io/).

## Scope

The projects aims to provide a template for a Servant project featuring:

- database interaction with [rel8](https://hackage.haskell.org/package/rel8);
- JWT authentication with [servant-auth](https://hackage.haskell.org/package/servant-auth);
- loggin with [co-log-core](https://hackage.haskell.org/package/co-log-core);
- TOML configuration using [tomland](https://hackage.haskell.org/package/tomland);
- first class records in Servant API using [NamedRecords](https://hackage.haskell.org/package/servant-0.19/changelog).

## The application

The application allows users to categorify contents by tags. Any content can have many tags and any tag could be used for several contents.

It allows also to retrieve contents by a set of tags.

### Architecture

A more in depth description of the architecture of the application can be found in [ARCHITECTURE.md].

### Configuration

Configuration of the application is managed using [TOML](https://toml.io). The application requires a configuration file with the following format:

```toml
[database]
  host     = "localhost"
  port     = 5432
  dbname   = "tagger"
  user     = "tagger"
  password = "password"

[api]
  port = 8080
```

By default the file is located in `config.toml`, but the path is actually configurable with the `config` option.

### Authentication

The main endpoints of the application are protected by JWT authentication. To access them you first need to get an authorization token for a user.

To get it you first need to register a user by calling the `register` endpoint.

Next, you can obtain a token by calling the `login` endpoint with the same data provided to the register endpoint.

Eventually, you should pass your token in the `Authorization` header for the relevant endpoints to access them.

## Development

The project provides scripts to manage the lifecycle of the application. If `nix` is available the scripts will attempt to use it, otherwise they will fall back to the "raw" commands (i.e., using `stack` directly from the user's environment.)

The project uses [Stack](https://docs.haskellstack.org/en/stable/README/). The convenience scripts are completely optional and the user is free to use the stack commands directly (e.g. `stack build`, `stack test`, etc.)

To build the API, run

```
bin/api/build # or directly `stack build`
```

> :information_source: This command forwards options/flags to `stack build`, so it could be run as `bin/api/build --file-watch`

> :warning: Note for non-nix users: the build requires the presence of the `pg_config` executable which is made available by installing Postgresql. Nix would take care of this automatically.

To run the tests, run

```
bin/api/test # or directly `stack test`
```

You can launch the web server using

```
bin/api/serve # or directly (without hot-reloading) `stack exec servant-template-exe`
```

> :warning: Note for non-nix users: serving the API with hot-reloading requires the presence of the `watchexec` utility which is made available by the nix shell. Install it manually if you wish to use this script.

which will expose the service on port defined in configuration.

The executable accepts two options:

- `--config`, which allows to customize the path of the configuration file
- `--jwk`, which allows to customize the path of the file where the JWK is stored

### Database

In the root of the project you can find a `docker-compose.yml` file which provides a Postgresql database and a web interface to it, exposed on port `8081`.

You can initialise the schema of the database by running the `schema.sql` which is also provided.

Alternatively, there is a setup script in `bin/db/setup` that will create a database, user and load the schema required for a working development environment. This script assumes the existence of few packages (`toml2json`, `jq`, `postgres`, etc.) made available by the nix-shell but should be usable if they are installed manually.

## Documentation

You can generate the documentation of the project using

```
bin/api/docs
# or directly:
stack haddock
```

### OpenApi documentation

You can access the OpenAPI documentation just by visiting the `docs` endpoint

## Frontend

This repository contains also a client [Elm](https://elm-lang.org/) application to interact in a human-friendly way with the Tagger api.

You can find more details in [elm/README.md](elm/README.md), but there are convenience commands:

```
# builds the elm project
bin/frontend/build

# serves, reloading on file change
bin/frontend/serve
```
