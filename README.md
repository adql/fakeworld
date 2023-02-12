# FakeWorld #

_FakeWorld_ is a _wip_ full-stack implementation of the
[RealWorld](https://github.com/gothinkster/realworld) demo Medium.com
clone, but with a twist: the "front-end" is instead a _TUI_ mock-up!

Here it is running on _RealWorld_'s demo API:

![FakeWorld animated screenshot](https://user-images.githubusercontent.com/6893125/217037860-b4ba632e-4878-49c0-836c-86ed91a1a185.gif)

It shows the main page and an article (the problem with newlines showing as `\n` is on the demo API's side). [See below](#status) for development status information.

### Why? ###

- Because I can
- Because Haskell is fun
- Because it's a great learning opportunity

### How? ###

The project utilizes the following technologies and libraries:

- [brick](https://github.com/jtdaugherty/brick/) for creating the TUI
  and managing everything on the front-end
- [Servant](https://github.com/haskell-servant/servant) for
  generating, serving (via _wai_) and requesting the API
- [PostgreSQL](https://www.postgresql.org/) as database, using
  [Hasql](https://github.com/nikita-volkov/hasql/) on the Haskell side
- [Aeson](https://github.com/haskell/aeson/) for all JSON heavy-lifting
- [optparse-applicative](https://github.com/pcapriotti/optparse-applicative)
  for command line parsing

### Status ###

In the first stage only content that doesn't require authentication is implemented. The following list is thus limited.

- "Front-end"
  - [x] Basic layout with header, footer, banner
  - [x] Home page with article list and popular tags
  - [x] Article page (almost complete; needs some small refinements)
    - [ ] Comment section
  - [ ] Profile page
  - [ ] Sign up / log in pages

- Back-end
  - [x] API endpoints and responses
  - Database
    - [x] Articles table
    - [x] Profiles table
    - [x] Comments table
  - Database queries
    - [ ] Get a list of articles
    - [x] Get an article / its comments by slug
    - [x] Get a profile by username
    - [x] Get all tags

## Running ##

Haskell development environment needs to be installed. The common and simple way is using GHCup ([installation instructions](https://www.haskell.org/ghcup/)). _FakeWorld_ uses Cabal.

After having installed GHCup install the necessary toolchain:

``` shell
ghcup install ghc
ghcup install cabal latest
```

### Just the TUI frontend

Use cabal and pass the `-e` (i.e. `--external`) argument to run the TUI frontend on the demo backend, set by default to `https://api.realworld.io:443`:

``` shell
cabal run fakeworld -- -e
```

Make sure you're on a normal terminal! Running from within e.g. Emacs will not work.

### Full-Stack ###

These instructions are for Linux. Most of the code in the `Makefile` is trivial and should be easily adaptable to MacOS and (maybe not quite easily) to Windows.

The PostgreSQL database is run on a Docker container, so make sure you have [Docker installed](https://docs.docker.com/get-docker/) in your path.

**Important**: You must provide a password to the Postgres database with the `DB_PASSWORD` environment variable using a `.env` file. Otherwise the scripts in the `Makefile` fail.

initiate the database with some dummy data:

``` shell
make populate-db
```

This will start a Docker container for the database (with a persistent volume named `fakeworld-postgres`), create tables and populate them with some Lorem Ipsums. You can then run the app full-stack from a terminal with

``` shell
make run
```
