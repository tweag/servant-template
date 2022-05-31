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
  dbname   = "db-name"
  user     = "db-user"
  password = "db-password"

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
The project is setup to be built either with [Stack](https://docs.haskellstack.org/en/stable/README/) or [Nix](https://nixos.org/). Convenience scripts are also provided (under the `bin/` folder) to manage the lifecycle of the application which try to use Nix, if available, and fall back to using Stack directly otherwise. The scripts are completely optional and the user is free to use the stack commands directly (e.g. `stack build`, `stack test`, etc.)

In summary, there are three equally valid ways of interacting with the project depending on your setup:
- Using the higher-level convenience scripts (`bin/api/serve`, `bin/frontend/build`)
- Through the nix-shell (using `nix-shell --run 'stack'` and `nix-shell --run 'elm make ...'` and letting `nix` take care of making the dependencies available)
- Directly (using your environment's `stack` and `elm` commands)

### Setup
Setting up the runtime dependencies for development, such as the database, is taken care of by the `bin/setup` script. This calls individual components' setup scripts such as `bin/db/setup` under the hood, if you prefer to call it directly without using `nix`.

### Building the API

To build the API, run

```sh
# With convenience script
bin/api/build

# With nix
nix-shell --run 'stack build'

# With stack
stack build
```

> :information_source: The convenience script forwards options/flags to `stack build`, so it could be run as `bin/api/build --file-watch`

> :warning: Note for non-nix users: the build requires the presence of the `pg_config` executable which is made available by installing Postgresql. Nix would take care of this automatically.

### Serving the API for development
You can launch the web server using

```sh
# With convenience script (with hot-reloading enabled)
bin/api/serve

# With nix
nix-shell --run 'stack exec servant-template-exe'

# With stack (without hot-reloading)
stack exec servant-template-exe
```

### Running the API tests
To run the tests, run

```sh
# With convenience script
bin/api/test

# With nix
nix-shell --run 'stack test'

# With Stack
stack test
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

```sh
# With convenience script
bin/api/docs

# With Stack
stack haddock
```

### OpenApi documentation

You can access the OpenAPI documentation just by visiting the `docs` endpoint (by default http://localhost:8080/docs)

## Frontend

This repository contains also a client [Elm](https://elm-lang.org/) application to interact in a human-friendly way with the Tagger api.

You can find more details in [elm/README.md](elm/README.md), but there are convenience commands:

```
# builds the elm project
bin/frontend/build

# serves, reloading on file change
bin/frontend/serve
```
