# FakeWorld #

_FakeWorld_ is a full-stack implementation of the
[RealWorld](https://github.com/gothinkster/realworld) demo Medium.com
clone, but with a twist: the "front-end" is instead a _TUI_ mock-up!

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
