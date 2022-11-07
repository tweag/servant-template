# Tagger Elm client

This folder contains a client application built with [Elm](https://elm-lang.org/), which allows interacting in a human-friendly way with the Tagger API.

## Building

You can build the client application using

```
elm make src/Main.elm
```

## Serving for development

You can start the application with live reload using:

```
elm-live src/Main.elm --open -- --debug'
```

It will open a new tab in your browser with the time-traveler available.

## Workflow

The application requires you to first register a new user. Once this is done, you can log in with the same credentials and access the private area.

In the private area, you'll see the contents for the logged-in user, and you can also:

- add new contents with their tags;
- filter the shown contents by tag.

## Specification

The `spec` folder contains some end-to-end acceptance tests written using [Quickstrom](https://quickstrom.io/).

To run them, just execute `docker-compose up` from the `elm/spec` folder, given your application is exposed on `localhost:8000`.

Then in the `elm/spec/report` folder you'll find an `index.html` file containing a report of each test which was executed.
