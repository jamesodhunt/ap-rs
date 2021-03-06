// Copyright (c) 2021 James O. D. Hunt.
//
// SPDX-License-Identifier: Apache-2.0
//

use std::io;
use thiserror::Error;

/// The error type that represents all possible parse errors
/// and handler errors.
#[derive(Error, Debug)]
pub enum Error {
    //------------------------------
    // Incorrect API usage errors (programmer error)
    //------------------------------
    /// No handler means nothing will happen.
    #[error("no handler specified")]
    NoHandler,

    /// No registered arguments means nothing will happen.
    #[error("no arguments registered")]
    NoArgs,

    //------------------------------
    // Runtime errors (user error)
    //------------------------------
    /// User specified an unknown option.
    #[error("unknown option")]
    UnknownOpt,

    /// User specified an invalid argument that looks
    /// like an option but doesn't have a name.
    #[error("missing option name")]
    MissingOptName,

    /// Option argument was not specified.
    #[error("missing option argument")]
    MissingOptArg,

    /// An option that must be specified was not specified.
    #[error("missing required option")]
    MissingReqOpt,

    /// Positional arguments are not permitted by default.
    ///
    /// To allow them, either register an `Arg` for `POSITIONAL_HANDLER_OPT`,
    /// or set `Settings.ignore_unknown_posn_args`.
    #[error("positional arguments not allowed")]
    NoPosnArgs,

    //------------------------------
    // Limitations
    //------------------------------
    /// Options must be kept separate on the command line
    /// (`-d -a foo` rather than `-da foo`).
    #[error("option bundling not supported")]
    NoBundling,

    /// Option names can only be a single character long
    /// (`-v` rather than, say, `--verbose`).
    #[error("long options not supported")]
    NoLongOpts,

    /// Custom error that a handler can use if the other errors are not
    /// appropriate.
    #[error("handler error: {0:?}")]
    HandlerError(String),

    /// An unknown error (used for testing).
    #[error("generic error: {0:?}")]
    GenericError(String),

    /// An I/O error
    #[error("I/O error: {0:?}")]
    IOError(#[from] io::Error),
}

/// Convenience type that allows a function to be defined as returning a
/// [Result], but which only requires the success type to be specified,
/// defaulting the error type to this crates `Error` type.
pub type Result<T, E = Error> = std::result::Result<T, E>;
