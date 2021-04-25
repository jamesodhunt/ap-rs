// Copyright (c) 2021 James O. D. Hunt.
//
// SPDX-License-Identifier: Apache-2.0
//

/// An example showing how to return an error from the handler
use ap::{App, Arg, Args, Error, Handler, Result, UNKNOWN_OPTION_HANDLER_OPT};

/// The type we will use to handle CLI parsing for this program.
#[derive(Clone, Debug, Default)]
struct MyHandler {}

impl Handler for &mut MyHandler {
    /// Handler function for handling all CLI arguments this program supports.
    ///
    /// The handler must return `Ok(())` on success, or one of the crates
    /// Error::* values on error. If the parsing errors defined are not
    /// appropriate, use `Error::HandlerError(string)` to return a more
    /// specific handler error.
    fn handle(&mut self, arg: Arg) -> Result<()> {
        match arg.option {
            // Only accept flag option that are a vowel.
            'a' | 'e' | 'i' | 'o' | 'u' => Ok(()),

            // Reject any other values.
            _ => Err(Error::HandlerError(format!(
                "option {:?} not a vowel",
                arg.option
            ))),
        }
    }
}

fn main() -> Result<()> {
    let mut my_handler = MyHandler::default();

    let mut args = Args::default();

    // Support all (flag) option values
    args.add(Arg::new(UNKNOWN_OPTION_HANDLER_OPT));

    let mut app = App::default()
        .help("some text")
        .args(args)
        .handler(Box::new(&mut my_handler));

    // Parse the command-line
    app.parse()
}
