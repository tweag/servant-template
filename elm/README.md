# Tagger Elm client

This folder contains a client application built with [Elm](https://elm-lang.org/), which allows to interact in a human-friendly way with the Tagger api.

## Build

You can build the client application using

```
elm make src/Main.elm
```

Then, you can directly open `index.html` to interact with the application.

## Workflow

The application requires you to first register a new user. Once this is done, you can login with the same credentials and access the private area.

In the private area, you'll see the contents for the logged in user and you can also:

- add new contents with their tags;
- filter the shown contents by tag.
