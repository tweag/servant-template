# servant-template

A modern template for a [Servant](https://haskell-servant.github.io/).

## Scope

The projects aims to provide a template for a Servant project featuring:

- database interaction with [rel8](https://hackage.haskell.org/package/rel8);
- authentication with [servant-auth](https://hackage.haskell.org/package/servant-auth);
- logging on an external server such as [Sentry](https://hackage.haskell.org/package/servant-auth);
- first class records in Servant API using [NamedRecords](https://hackage.haskell.org/package/servant-0.19/changelog).

## The application

The application allows users to categorify contents by tags. Any content can have many tags and any tag could be used for several contents.

It allows also to retrieve contents by a set of tags.

## Development

The project is using [Stack](https://docs.haskellstack.org/en/stable/README/).

To build the project, run

```
stack build
```

To run the tests, run

```
stack test
```

You can launch the web server using

```
stack exec servant-template-exe
```

which will expose the service on port 8080

## Openapi documentation

You can access the OpenAPI documentation just by visiting

```
localhost:8080/docs
```
