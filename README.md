### Important notice

This project is deprecated in favor of nrc's `rustfmt` available here:

    https://github.com/rust-lang-nursery/rustfmt

## `rustfmt` -- code formatting Rust

`rustfmt` is distributed under the terms of the "MIT License." See `LICENSE.txt` in the root of this repository for more information.

### Building `rustfmt`

With a recent build of `rustc` and `cargo` installed:

~~~~
cargo build
~~~~

Failing a cargo install, the project can be built with:

~~~~
make
~~~~

or

~~~~
rustc src/main.rs
~~~~

This will give you a `rustfmt` or `main` binary that behaves as detailed below.

### Functionality

`rustfmt` currently:

* Reads from `stdin`
* Lexes the string to ensure correctness
* Prints the lexed code to `stdout` in an idiomatically arranged format

`rustfmt` does *not*:

* Preserve comments
* Lots of other stuff
