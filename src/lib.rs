// Copyright (c) 2021 James O. D. Hunt.
//
// SPDX-License-Identifier: Apache-2.0
//

#![deny(missing_docs)]
#![forbid(unsafe_code)]

//! Simple crate for parsing command-line arguments.
//!
//! If you want lots of extra features, you should consider the excellent
//! [`clap`](https://crates.io/crates/clap) crate instead.
//!
//! To understand what "simple" means, see the [Limitations](#limitations) section.
//!
//! ---
//!
//! Table of contents:
//!
//! * [Overview](#overview)
//! * [Quickstart](#quickstart)
//! * [Examples](#examples)
//! * [Details](#details)
//! * [Terminology](#terminology)
//! * [Rationale](#rationale)
//! * [Summary of features and behaviour](#summary-of-features-and-behaviour)
//! * [Limitations](#limitations)
//!
//! ---
//!
//! # Overview
//!
//! This crate is used to parse command-line arguments. It calls a handler
//! function for each registered option it parses.
//!
//! # Quickstart
//!
//! > **Note:** If you are not familiar with command-line handling,
//! > see the [terminology](#terminology) section.
//!
//! 1. Create a `struct` type to represent the handler which will be used
//!    to process _all_ your options.
//!
//!    The `struct` can be empty if you just want to be notified when a
//!    particular option is specified, or your could specify members to record
//!    or calculate particular details.
//!
//!    ```rust
//!    #[derive(Clone, Debug, Default)]
//!    struct MyHandler {}
//!    ```
//!
//! 1. Implement the [Handler] trait for the `struct`.
//!
//!    This trait only requires you to create a single `handle()` method which
//!    returns a [Result] to indicate success or failure.
//!
//!    ```rust
//!    # use ap::{App, Arg, Args, Handler, Need, Result};
//!    #
//!    # #[derive(Clone, Debug, Default)]
//!    # struct MyHandler {}
//!    #
//!    impl Handler for &mut MyHandler {
//!        fn handle(&mut self, arg: Arg) -> Result<()> {
//!            // ...
//!
//!            Ok(())
//!        }
//!    }
//!    ```
//!
//! 1. Create a handler variable for your `struct` type.
//!
//!    ```rust
//!    # use ap::{App, Arg, Args, Handler, Need, Result};
//!    #
//!    # #[derive(Clone, Debug, Default)]
//!    # struct MyHandler {}
//!    #
//!    # impl Handler for &mut MyHandler {
//!    #     fn handle(&mut self, arg: Arg) -> Result<()> {
//!    #         // ...
//!    #
//!    #        Ok(())
//!    #    }
//!    # }
//!    #
//!    let mut handler = MyHandler::default();
//!    ```
//!
//! 1. Create an [Args] variable to hold all the arguments you wish to support.
//!
//!    ```rust
//!    # use ap::{App, Arg, Args, Handler, Need, Result};
//!    #
//!    let mut args = Args::default();
//!    ```
//!
//! 1. Add a new [Arg] for each argument you wish to support to the [Args]
//!    variable.
//!
//!    As a minimum, you must specify a "name" (single-character short option
//!    value) for the argument.
//!
//!    By default, options are "flags" (see the [Terminology section](#terminology)).
//!
//!    ```rust
//!    # use ap::{App, Arg, Args, Handler, Need, Result};
//!    #
//!    # let mut args = Args::default();
//!    #
//!    // Support "-a <value>" option.
//!    args.add(Arg::new('a').needs(Need::Argument));
//!
//!    // Support "-d" flag option.
//!    args.add(Arg::new('d'));
//!    ```
//!
//! 1. Create an [App] variable to represent your program, specifying the
//!    [Args] and [Handler] variables:
//!
//!    ```rust
//!    # use ap::{App, Arg, Args, Handler, Need, Result};
//!    #
//!    # #[derive(Clone, Debug, Default)]
//!    # struct MyHandler {}
//!    #
//!    # impl Handler for &mut MyHandler {
//!    #     fn handle(&mut self, arg: Arg) -> Result<()> {
//!    #         // ...
//!    #
//!    #        Ok(())
//!    #    }
//!    # }
//!    #
//!    # let mut handler = MyHandler::default();
//!    #
//!    # let mut args = Args::default();
//!    # // Support "-a <value>" option.
//!    # args.add(Arg::new('a').needs(Need::Argument));
//!    #
//!    # // Support "-d" flag option.
//!    # args.add(Arg::new('d'));
//!    #
//!    let mut args = App::new("my app")
//!        .help("some text")
//!        .args(args)
//!        .handler(Box::new(&mut handler));
//!    ```
//!
//! 1. Call the `parse()` method on the [App] variable. The handler will be
//!    called for all [Arg] arguments added to the [Args] variable:
//!
//!    ```rust
//!    # use ap::{App, Arg, Args, Handler, Need, Result};
//!    #
//!    # #[derive(Clone, Debug, Default)]
//!    # struct MyHandler {}
//!    #
//!    # impl Handler for &mut MyHandler {
//!    #     fn handle(&mut self, arg: Arg) -> Result<()> {
//!    #         // ...
//!    #
//!    #        Ok(())
//!    #    }
//!    # }
//!    #
//!    # let mut handler = MyHandler::default();
//!    #
//!    # let mut args = Args::default();
//!    # // Support "-a <value>" option.
//!    # args.add(Arg::new('a').needs(Need::Argument));
//!    #
//!    # // Support "-d" flag option.
//!    # args.add(Arg::new('d'));
//!    #
//!    # let mut args = App::new("my app")
//!    #     .help("some text")
//!    #     .args(args)
//!    #     .handler(Box::new(&mut handler));
//!    #
//!    // Parse the command-line
//!    let result = args.parse();
//!    ```
//!
//! # Examples
//!
//! Below is a full example showing how to write a program that supports
//! a few command line options. It also shows how the handler can modify
//! it's state, allowing stateful and conditional option handling.
//!
//! ```rust
//! use ap::{App, Arg, Args, Handler, Need, Result};
//!
//! // The type that will be used to handle all the CLI options
//! // for this program.
//! #[derive(Clone, Debug, Default)]
//! struct MyHandler {
//!     i: usize,
//!     v: Vec<String>,
//!     s: String,
//! }
//!
//! impl Handler for &mut MyHandler {
//!     fn handle(&mut self, arg: Arg) -> Result<()> {
//!         println!(
//!             "option: {:?}, value: {:?}, count: {}",
//!             arg.option, arg.value, arg.count
//!         );
//!
//!         // Change behaviour if user specified '-d'
//!         if arg.option == 'd' {
//!             self.i += 7;
//!         } else {
//!             self.i += 123;
//!         }
//!
//!         self.s = "string value set by handler".into();
//!         self.v.push("vector modified by handler".into());
//!
//!         Ok(())
//!     }
//! }
//!
//! fn main() -> Result<()> {
//!     let mut handler = MyHandler::default();
//!
//!     println!("Initial state of handler: {:?}", handler);
//!
//!     let mut args = Args::default();
//!
//!     // Support "-a <value>" option.
//!     args.add(Arg::new('a').needs(Need::Argument));
//!
//!     // Support "-b <value>" option.
//!     args.add(Arg::new('b').needs(Need::Argument));
//!
//!     // Support "-d" flag option.
//!     args.add(Arg::new('d'));
//!
//!     let mut args = App::new("my app")
//!         .help("some text")
//!         .args(args)
//!         .handler(Box::new(&mut handler));
//!
//!     // Parse the command-line
//!     let result = args.parse();
//!
//!     // If you want to inspect the handler after parsing, you need to
//!     // force ownership to be returned by dropping the [Args] variable.
//!     drop(args);
//!
//!     println!("Final state of handler: {:?}", handler);
//!
//!     // return value
//!     result
//! }
//! ```
//!
//! For further examples, try out the programs in the `examples/` directory:
//!
//! ```bash
//! $ cargo run --example simple -- -a foo -d -a bar -d -a baz
//! $ cargo run --example positional-args-only -- one two "hello world" three "foo bar" four "the end"
//! $ cargo run --example option-and-positional-args -- "posn 1" -d "posn 2" -a "hello world" -a "foo bar" "the end" -d
//! ```
//!
//! # Details
//!
//! ## Terminology
//!
//! > **Note:** For further details, see `getopt(3)`.
//!
//! - An "argument" is a value passed to a program on the command-line.
//!
//!   Arguments can be "options" or "positional arguments".
//!
//!   > **Note:** A single or double quoted string counts as _one_ argument,
//!   > even if that string comprises more than one word (this magic is handled
//!   > by the shell).
//!
//! - An "option" is an argument that starts with a dash character (`-`) and ends
//!   with a single character which is itself not `-`, for example, `-a`, `-z`,
//!   `-A`, `-Z`, `-0`, `-9`, _etc_.
//!
//!   This character is the options "name". Option names are case sensitive:
//!   upper and lower-case characters represent different options.
//!
//!   This type of option is known as a "short option" since it is identified
//!   with only a single character.
//!
//! - Options which accept an argument (a value, called an "option argument"
//!   or "optarg" in `getopt(3)` parlance) are often referred to simply
//!   as "options" since these are the commonest form of options.
//!
//! - An "option argument" is the value that immediately follows an option. It
//!   is considered to be "bound" or paired with the option immediately
//!   preceding it. By definition, the option argument cannot start with a
//!   dash to avoid it being considered an option itself.
//!
//! - Options that do not accept an argument are call "flags" or
//!   "stand-alone options". These tend to be used to toggle some
//!   functionality on or off.
//!
//!   > **Examples of flags:**
//!   >
//!   > Most programs support a few common flags:
//!   >
//!   > - `-h`: Display a help/usage statement and exit.
//!   > - `-v`: Display a version number and exit, or sometimes enable verbose mode.
//!
//! - A "positional argument" (also known as a "non-option argument")
//!   is an argument that is not an option: it is a word or a quoted string
//!   (which cannot start with a dash, unless it is escaped as `\-`).
//!
//!   > **Example of positional arguments:**
//!   >
//!   > `echo(1)` is a good example of a program that deals with positional arguments:
//!   >
//!   > ```bash
//!   > $ echo one two three "hello, world" four five "the end"
//!   > ```
//!
//! - The special option `--` is reserved to mean "end of all options": it can
//!   be used by programs which need to accept a set of options followed by a
//!   set of positional arguments. Even if an argument starting with a single dash
//!   follows the double-dash, it will not be considered an option.
//!
//!   This crate will stop processing command-line arguments if it finds `--`
//!   on the command-line.
//!
//! ### Example of argument types
//!
//! Assume a program that is run as follows:
//!
//! ```bash
//! $ myprog -d 371 "hello, world" -x "awesome value" -v "the end"
//! ```
//!
//! The program has 7 actual CLI arguments:
//!
//! ```text
//! 1: '-d'
//! 2: '371'
//! 3: 'hello, world'
//! 4: '-x'
//! 5: 'awesome value'
//! 6: '-v'
//! 7: 'the end'
//! ```
//!
//! How these arguments are interpreted depends on whether each of the options
//! (the arguments starting with `-`) are specified as taking a value.
//!
//! If all the options are specified as flags, the arguments are
//! interpreted as follows:
//!
//! ```text
//! '-d'            # A flag option.
//! '371'           # A positional argument.
//! 'hello, world'  # A positional argument.
//! '-x'            # A flag option.
//! 'awesome value' # A positional argument.
//! '-v'            # A flag option.
//! 'the end'       # A positional argument.
//! ```
//!
//! But if we assume that `-d` and `-x` are specified as taking a value, then the arguments group as follows:
//!
//! ```text
//! '-d 371'           # An option ('d') with a numeric option argument ('371').
//! 'hello, world'     # A positional argument ('hello, world').
//! '-x awesome value' # An option ('x') with a string option argument ('awesome value').
//! '-v'               # A flag option.
//! 'the end'          # A positional argument.
//! ```
//!
//! Alternatively, if we assume that all the options take a value, then the arguments group as follows:
//!
//! ```text
//! '-d 371'             # An option ('d') with a numeric option argument ('371').
//! 'hello, world'       # A positional argument ('hello, world').
//! '-x 'awesome value'' # An option ('x') with a string option argument ('awesome value').
//! '-v 'the end''       # An option('v') with a string option argument ('the end').
//! ```
//!
//! # Rationale
//!
//! Why yet another command-line parser?
//!
//! There are many rust CLI argument parsing crates. This one was written
//! since I couldn't find a crate that satisfied all of the following
//! requirements:
//!
//! - Allow the intermingling of options and non-options ("positional arguments").
//!
//!   This is an extremely useful feature for certain use cases and is a
//!   standard library call available in POSIX-compliant `libc` implementations.
//!   Quoting from `getopt(3)`:
//!
//!   > If the first character of `optstring` is '`-`', then each nonoption
//!   > `argv`-element is handled as if it were the argument of an option with
//!   > character code `1`.
//!
//! - Parse the command-line arguments _in order_.
//!
//!   The modern fashion seems to be to build a hash of options to allow the
//!   program to query _if an option was specified_. This is useful in most
//!   circumstances, but I had a use-case which required the _order_ and
//!   _number of occurences_ of each option to be important.
//!
//! - Allow a handler function to be specified for dealing with the
//!   arguments as they are encountered.
//!
//! In summary, I needed a more POSIX-like (`POSIXLY_CORRECT`) command line
//! argument parser, so, here it is.
//!
//! # Summary of features and behaviour
//!
//! - Simple and intuitive ("ergonomic") API.
//! - Small codebase.
//! - Comprehensive set of unit tests.
//! - Parses arguments in strict order.
//! - Handles each argument immediately.
//!
//!   As soon as a (registered and valid) argument is encountered,
//!   the handler is called.
//!
//! - Arguments are not permuted.
//! - Requires a function to be specified for handling each option.
//! - Option arguments are always returned as strings.
//!
//!   The caller can convert them into numerics, _etc_ as required.
//!
//! - Allows intermingling of flag options, options-with-arguments
//!   and "positional arguments" (arguments that are not options).
//!
//! - Allows options to be specified multiple times
//!   (and records that number).
//!
//!   > **Note:** You can limit the number of occurences if you wish
//!   > by checking the `Arg.count` value in your handler.
//!
//! - Options can be defined as mandatory.
//!
//! - Unknown options can be configured to be ignored
//!   or passed to the handler.
//!
//!   > **Notes:**
//!   >
//!   > - By default, unknown options are not handled and an error is
//!   >   generated if one is found.
//!   > - If you want to support positional arguments, either register
//!   >   an [Arg] for [POSITIONAL_HANDLER_OPT], or set
//!   >   `Settings.ignore_unknown_options`.
//!
//! - "Unknown" positional arguments can be configured to be ignored
//!    or passed to the handler.
//!
//!   > **Notes:**
//!   >
//!   > - By default, positional arguments are not handled and an
//!   >   error is generated if one is found.
//!   > - If you want to support positional arguments, either register an
//!   >   [Arg] for [POSITIONAL_HANDLER_OPT], or set
//!   >   `Settings.ignore_unknown_posn_args`.
//!
//! - Automatically generates help / usage statement (`-h`).
//!
//! # Limitations
//!
//! - Option bundling is not supported
//!
//!   **Example:** `-d -v -a "foo bar"` is valid, but `-dva "foo bar"` is not.
//!
//! - Non-ASCII option names are not supported.
//! - Long options are not supported
//!
//!   **Example:** `-v` is valid, but `--verbose` is invalid.
//!
//! - Options and their arguments must be separated by whitespace.
//!
//!   **Example:** '`-d 3`' is valid, but '`-d3`' is valid.
//!
//! - Options with optional arguments are not supported.
//!
//!   **Explanation:** An option has to be defined as being a flag (no
//!   argument) or a standard option (requiring a value). It cannot be both.
//!
//! - Options cannot accept multiple values
//!
//!   **Example:** `-a "foo bar" "baz" "the end"` cannot be parsed
//!   as a single `-a` option.
//!
//!   However:
//!
//!   - Options can be specified multiple times, each with different values.
//!   - You _could_ parse that command-line using [POSITIONAL_HANDLER_OPT].

mod args;
mod error;

pub use error::{Error, Result};

pub use args::{
    get_args, App, Arg, Args, Handler, Need, Settings, POSITIONAL_HANDLER_OPT,
    UNKNOWN_OPTION_HANDLER_OPT,
};
