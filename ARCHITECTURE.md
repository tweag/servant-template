# Architectural documentation

`Tagger` is a simple application created to showcase `Servant` and how to integrate it into a working application.

The project is structured in three layers:
- the `Tagger` folder contains the business domain of the application;
- the `Infrastructure` folder contains the implementation of the services needed by the application;
- the `API` folder contains the application layer which connects the external world and the previous two layers.

There is a dependency between the layers which works as follows

```mermaid
graph TD;
  Infrastructure --> Domain;
  Application --> Infrastructure;
  Application --> Domain;
```

where each arrow denotes an allowed dependency.

In other words, the `Domain` of the application can never access either the `Infrastructure` or the `Application` layer. The `Infrastructure` layer can only access the `Domain` layer and the `Application` layer has actually access to everything.

## Domain

The domain of the application contains the relevant domain entities, as `Content`, `Tag` and `User`.

Identifiers of domain entities are not included in the entities themselves and are instead managed using an `Id` data type, which has a phantom type used to specify which entity it is actually identifying.

The domain layer contains also the [repositories](https://www.martinfowler.com/eaaCatalog/repository.html), defined as records of functions, to interact with collections of domain objects. Those records are parameterized by a monadic context `m` in order to define the interface of repositories without committing to a particular implementation.

### A concrete example

For example, to interact with `User`s, we have a `UserRepository` which abstracts the operations concerning users

```haskell
data UserRepository m = UserRepository
  { getUserByName :: Text -> m (Id User, User)
  , addUser       :: User -> m (Id User)
  }
```

These operations define an interface which allows us to interact with an abstract model of a collection of `User`s.
In practice, if we wanted to combine these operation, we would need to impose restrictions on the context `m` in exchange for a more powerful api. For example, to allow sequential composition of such operations, we would need to restrict `m` to be a `Monad`. Similarly, if we wanted to grant to these operations the possiblity of failure, we could add a `MonadError` contraint. Broadly speaking, we could use `mtl`-style typeclasses to restrict the allowed contexts in exchange for more computational expressivity.

## Infrastructure

The infrastructure layer provides the implementations needed by the application. Specifically it provides access to the database, to error logging and user authentication.

### Persistence

Contains the implementation, based on `PostgreSQL`, of the repositories defined in the domain layer.

In the concrete implementations we specialize the context `m` of the repositories to `ExceptT e IO` so that we can work in a concrete monad.

The repositories are combining the queries defined at the database level and are using the serialization/unserialization mechanism to convert between the domain entities and their representation at the database level.

#### A concrete example

Continuing the example introducted above, in the infrastructure layer we choose a concrete monad stack where we want to work. Specifically, for our repositories, we need to perform `IO` to interact with the database and we need to allow the possibility of failures, since we might not find the records we are looking for in the database. Therefore we specialize our generic context `m` to `ExceptT e IO`, where the `e` error type depends on the repository itself.

This leads us to define our `postgresUserRepository` as

```haskell
postgresUserRepository :: Connection -> UserRepository (ExceptT UserRepositoryError IO)
postgresUserRepository connection = UserRepository
  { getUserByName = postgresGetUserByName connection
  , addUser       = postgresAddUser connection
  }
```

where `postgresGetUserByName` and `postgresAddUser` contain the actual logic to run the correct queries in [PostgreSQL](https://www.postgresql.org).

### Logging

Provides to the application the ability of logging error messages.

### Authentication

It contains two services, `PasswordManager` and `AuthenticateUser`, where the latter is using the former.

Being infrastructural services their interface and implementation are defined together in the same module.

## Application

The application layer deals mainly with two issues:
- setting up all the services required by the application
- describing and implementing the API of the application

### Services

The `AppServices` modules deals with building all the required services and packing them all together.

To achieve this we first need to choose an explicit implementation for each service.

At this level we want all the services to work directly in the `Handler` monad. This requires us to hoist the services from the concrete context where they are initially defined to `Handler`.

Doing so implies that we also need to handle the errors generated by the services. Hence at this point we decide how to log the error messages and which status code is appropriate for every specific error.

#### A concrete example

Considering our recurring example with `UserRepository`, at the domain level we're using a generic monadic context `m`, while at the infrastructure level we specialized it to `ExceptT UserRepositoryError IO`.

Now, at the application level, we're working in the `Handler` context, because that is where `Servant` handlers operate. To fill the gap and connect all the pieces, we hoist the `ExceptT UserRepositoryError IO` context into the `Handler` one, using a natural transformation `forall a. ExceptT UserRepositoryError IO a -> Handler a`.

```haskell
postgresUserRepository :: UserRepository (ExceptT UserRepositoryError IO)

hoistUserRepository :: (forall a. m a -> n a) -> UserRepository m -> UserRepository n

eitherTToHandler :: forall a. ExceptT UserRepositoryError IO a -> Handler a

handlerUserRepository :: UserRepository Handler
handlerUserRepository = hoistUserRepository eitherTToHandler postgresUserRepository
```

### API endpoints

Once we setup all the required services, we can pass them to the API endpoints which are defined in the `Application` module.

We make use of Servant `NamedRoutes` to define our API is a clear and composable way, splitting their respective handlers in several modules.

### Configuration

Another issue which is dealt with at the application level is configuration.

We define the schema of the required configuration and a bidirectional codec.
