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
