# Tagger Elm client

This folder contains a client application built with [Elm](https://elm-lang.org/), which allows interacting in a human-friendly way with the Tagger API.

## Building

You can build the client application using
```sh
# From the project root with convenience script
bin/frontend/build

# From the project root with nix
nix-shell --run 'cd elm; elm make src/Main.elm'

# With npm
elm make src/Main.elm
```

## Serving for development
```sh
# From the project root with convenience script
bin/frontend/serve

# From the project root with nix
nix-shell --run 'cd elm; elm-live src/Main.elm'

# With npm
cd elm; elm-live src/Main.elm'
```

Then, you can directly go to `http://localhost:1234` (default port) to view the application.

## Workflow

The application requires you to first register a new user. Once this is done, you can log in with the same credentials and access the private area.

In the private area, you'll see the contents for the logged-in user, and you can also:

- add new contents with their tags;
- filter the shown contents by tag.

## Specification

The `spec` folder contains some end-to-end acceptance tests written using [Quickstrom](https://quickstrom.io/).

To run them, just execute `bin/test/quickstrom` from the root of the project, given your application is exposed on `localhost:8000`.

Then in the `elm/spec/report` folder you'll find an `index.html` file containing a report of each test which was executed.
