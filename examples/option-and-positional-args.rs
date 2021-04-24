// Copyright (c) 2021 James O. D. Hunt.
//
// SPDX-License-Identifier: Apache-2.0
//

/// An example showing how to handle positional arguments and options.
use ap::{App, Arg, Args, Handler, Need, Result, POSITIONAL_HANDLER_OPT};

#[derive(Clone, Debug, Default)]
struct PositionalHandler {
    i: usize,
    v: Vec<String>,
    s: String,
}

impl Handler for &mut PositionalHandler {
    fn handle(&mut self, arg: Arg) -> Result<()> {
        println!("INFO: PositionalHandler: arg: {:?}", arg);

        self.i += 999;
        self.s = "string value set by handler".into();
        self.v.push("vector value set by handler".into());

        match arg.option {
            'd' => println!("INFO: found '-d' option"),
            'a' => println!(
                "INFO: found '-a <value>' option (value: {:?})",
                arg.value.unwrap()
            ),
            _ => (),
        };

        println!("INFO: PositionalHandler: self: {:?}", self);

        Ok(())
    }
}

fn main() -> Result<()> {
    let mut posn_handler = PositionalHandler::default();

    println!(
        "INFO: Initial value of positional handler: {:?}",
        posn_handler
    );

    let mut args = Args::default();

    args.add(Arg::new('a').needs(Need::Argument));
    args.add(Arg::new('d').needs(Need::Nothing));
    args.add(Arg::new(POSITIONAL_HANDLER_OPT));

    let mut app = App::default()
        .help("some text")
        .args(args)
        .handler(Box::new(&mut posn_handler));

    // Parse the command-line
    let result = app.parse();

    // XXX: essential!
    drop(app);

    println!(
        "INFO: Final value of positional handler: {:?}",
        posn_handler
    );

    result
}
