# servant-template

A modern template for [Servant](https://haskell-servant.github.io/).

## Scope

The projects aims to provide a template for a Servant project featuring:

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
  dbname   = "tagger"
  user     = "user"
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

*Note*: If you're using `nix` you can run `nix develop` to get a shell with all
required dependencies pre-installed. You should then build with `stack` from
within this shell as normal.

The project is using [Stack](https://docs.haskellstack.org/en/stable/README/).

To build the project, run

```
stack build
```

The build requires the presence of the `pg_config` executable.

To run the tests, run

```
stack test
```

You can launch the web server using

```
stack exec servant-template-exe
```

which will expose the service on port defined in configuration.

The executable accepts two options:

- `--config`, which allows to customize the path of the configuration file
- `--jwk`, which allows to customize the path of the file where the JWK is stored

### Database

In the root of the project you can find a `docker-compose.yml` file which provides a Postgresql database and a web interface to it, exposed on port `8081`.

You can initialise the schema of the database by running the `schema.sql` which is also provided.

## Documentation

You can generate the documentation of the project using

```
stack haddock
```

### OpenApi documentation

You can access the OpenAPI documentation just by visiting the `docs` endpoint

## Frontend

This repository contains also a client [Elm](https://elm-lang.org/) application to interact in a human-friendly way with the Tagger api.

You can find more details in [elm/README.md](elm/README.md).
