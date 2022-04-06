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

## Specification

The `spec` folder contains some end-to-end acceptance tests written using [Quickstrom](https://quickstrom.io/).

To run them, you could execute the following commands inside the `spec` folder, given your application is exposed on `localhost:8000`:

```
docker run --rm -d \
  --name webdriver \
  --network=host \
  -v /dev/shm:/dev/shm \
  -v $PWD:/spec \
  selenium/standalone-chrome:3.141.59-20200826

docker run --rm \
  --network=host \
  -v $PWD:/spec \
  quickstrom/quickstrom \
  quickstrom check \
  --webdriver-host=webdriver \
  --webdriver-path=/wd/hub \
  --browser=chrome \
  --reporter=html \
  --html-report-directory=/spec/report \
  --tests=10 \
  /spec/Tagger.spec.purs \
  http://localhost:8000
```

Then in the `spec/report` folder you'll find an `index.html` file containing a report of each test which was executed.
