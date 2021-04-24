// Copyright (c) 2021 James O. D. Hunt.
//
// SPDX-License-Identifier: Apache-2.0
//

/// A simple example showing how to handle options that need arguments.
use ap::{App, Arg, Args, Handler, Need, Result};

/// The type we will use to handle CLI parsing for this program.
#[derive(Clone, Debug, Default)]
struct MyHandler {
    // Let's keep track of the number of times particular argument are
    // specified on the command-line.
    a_count: usize,
    b_count: usize,
    d_count: usize,

    // Some random fields to demonstrate behaviour
    i: usize,
    v: Vec<String>,
    s: String,
}

impl Handler for &mut MyHandler {
    /// Handler function for handling all CLI arguments this program supports.
    fn handle(&mut self, arg: Arg) -> Result<()> {
        println!("INFO: MyHandler: arg: {:?}", arg);

        match arg.option {
            'a' => self.a_count += 1,
            'b' => self.b_count += 1,
            'd' => self.d_count += 1,
            _ => (),
        };

        self.i += 999;
        self.s = "string value set by handler".into();
        self.v.push("vector value set by handler".into());

        println!("INFO: MyHandler: self: {:?}", self);

        Ok(())
    }
}

fn main() -> Result<()> {
    let mut my_handler = MyHandler::default();

    println!("INFO: Initial value of my handler: {:?}", my_handler);

    let mut args = Args::default();

    // Support "-a <value>" option.
    args.add(Arg::new('a').needs(Need::Argument));

    // Support "-b <value>" option.
    args.add(Arg::new('b').needs(Need::Argument));

    // Support "-d" flag option.
    //
    // Note that since this option does not require an argument,
    // the `arg` passed to the handler will have an empty `value`.
    args.add(Arg::new('d').needs(Need::Nothing));

    let mut app = App::default()
        .help("some text")
        .args(args)
        .handler(Box::new(&mut my_handler));

    // Parse the command-line
    let result = app.parse();

    // XXX: essential!
    drop(app);

    println!("INFO: Final value of my handler: {:?}", my_handler);

    result
}
