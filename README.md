purescript-mote-runner
==

Creates a CLI wrapper for test suites written with the `purescript-mote` DSL,
that allows to selectively run tests.

Right now the pattern language is very simple, and requires the passed string to
exactly match the wanted group/test name.

## Example:

```sh
npm run example:build
npm run example:run -- --list
npm run example:run -- --pattern "generated from file"
```
