# `rustfmt` -- A code formatting tool for the Rust Programming Language

### Building `rustfmt`

With a recent build of `rustc`:

~~~~
rustc rustfmt.rs
~~~~

This will give you a `rustfmt` binary that behaves as detailed below.

### Functionality

`rustfmt` currently:

* Reads from `stdin`
* Lexes the string to ensure correctness
* Prints the lexed code to `stdout` in an idiomatically arranged format

`rustfmt` does *not*:

* Preserve comments
* Lots of other stuff
