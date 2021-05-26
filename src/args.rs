// Copyright (c) 2021 James O. D. Hunt.
//
// SPDX-License-Identifier: Apache-2.0
//

use std::cell::RefCell;
use std::collections::{hash_map::Entry, HashMap};
use std::env;
use std::fmt;
use std::io::Write;
use std::rc::Rc;

use crate::error::{Error, Result};

const OPT_PREFIX: char = '-';
const HELP_OPTION: &str = "-h";

/// String to show in usage if an option is required
const REQUIRED_STR: &str = " (required)";

/// Special argument that is silently consumed and used to denote the end of
/// all options; all arguments that follow are considered to be positional
/// arguments (even if they start with `-`!)
///
/// See: `getopt(3)`.
const END_OF_OPTIONS: &str = "--";
const LONG_OPT_PREFIX: &str = END_OF_OPTIONS;

/// If an [Arg] is registered with this value, all positional (non-option)
/// arguments will be passed to the handler.
//
/// # Notes
///
/// - *Similar* to [Settings::ignore_unknown_posn_args], but whereas that
///   option will entirely ignore positional arguments, this feature will
///   redirect them to your handler for processing.
pub const POSITIONAL_HANDLER_OPT: char = '"';

/// Special value that if registered as an [Arg] will pass all unknown options
/// to the handler to all it to deal with them. When the handler is called in
/// this scenario, [Arg.option] will be set to [UNKNOWN_OPTION_HANDLER_OPT].
///
/// # Notes
///
/// - *Similar* to [Settings::ignore_unknown_options], but whereas that
///   option will entirely ignore unknown options, this feature will redirect
///   unknown options to your handler for processing.
/// - You probably don't want to have to deal with this. But if you are
///   sure, read on...
/// - Since the option is not registered, the option is treated
///   as a flag. To handle unknown "normal options" with an argument,
///   you would need to register an [Arg] for [POSITIONAL_HANDLER_OPT] too.
///   It would then be up to your handler to decide if the unknown option was
///   a "normal option", or a flag. If it was a normal option, the handler would
///   need to save some state and look at the next argument provided to the
///   handler which, assuming it is a non-option would be either the unknown
///   option's argument, or a positional argument.
pub const UNKNOWN_OPTION_HANDLER_OPT: char = '?';

const USAGE_PREFIX_SPACES: &str = "    ";

/// Used to specify whether an option is a "stand-alone" flag option
/// (needs no value), or whether it requires an option argument.
#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone, Copy)]
pub enum Need {
    /// Option is stand-alone (no argument required).
    Nothing,
    /// Option needs an argument.
    Argument,
}

impl Default for Need {
    fn default() -> Self {
        Need::Nothing
    }
}

impl Need {
    /// Create a new default requirement for an [Arg].
    pub fn new() -> Self {
        Need::default()
    }
}

/// Trait that an argument handler must implement.
pub trait Handler {
    /// Function that will handle all registered options.
    ///
    /// Since the handler is provided with a mutable reference to itself,
    /// it can store and modify its state when called.
    ///
    /// # Parameters
    ///
    /// `arg` - The [Arg] provides details of the argument found
    ///         on the command-line.
    ///
    /// # Return value
    //
    /// - If the handler logic succeeds, return `Ok(())`.
    /// - If the handler needs to fail, it should return one of the [Error]
    ///   values. If the main errors are not appropriate, make the handler
    ///   return [Error::HandlerError].
    ///
    /// # Notes
    ///
    /// If a handler call fails, the command-line parsing will
    /// stop and the error will be returned to the caller of the parsing function.
    fn handle(&mut self, arg: Arg) -> Result<()>;
}

impl<'a> fmt::Debug for dyn Handler + 'a {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Handler: {:p}", self)
    }
}

impl<'a> PartialEq for Box<dyn Handler + 'a> {
    fn eq(&self, other: &Box<dyn Handler + 'a>) -> bool {
        self == other
    }
}

/// An argument, either an option or a positional argument.
///
/// 1) It is used to specify how an argument is to be handled.
///
///    If it is to be considered an option, the [Arg.option]
///    member should be set.
///
/// 2) To store the results of the parse for the argument.
///
///    For example, the parser records the number of times the argument was
///    handled in the `count` member.
///
/// # Note
///
/// - All members are public for handler convenience.
#[derive(Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd)]
pub struct Arg {
    /// Single character short option name (required).
    /// Can be set to special values for non-standard behaviour:
    ///
    /// - [POSITIONAL_HANDLER_OPT] (to handle positional arguments).
    /// - [UNKNOWN_OPTION_HANDLER_OPT] (to handle unknown options).
    pub option: char,
    /// Type of option (required, but defaults).
    pub needs: Need,
    /// Description of the option.
    pub help: Option<String>,
    /// Set if the option must be specified.
    pub required: bool,

    //----------------------------------------
    // The following are set by the parser.
    //----------------------------------------
    /// Value specified for this option
    /// (if the `needs` member is not [Need::Nothing]).
    ///
    /// # Notes
    ///
    /// - This is equivalent to `getopt(3)`'s `optarg` value.
    /// - This will be [None] for flag options.
    pub value: Option<String>,
    /// Number of times the option was specified.
    ///
    /// # Notes
    ///
    /// - Used internally for [Error::MissingReqOpt].
    /// - If a positional argument handler has been registered,
    ///   value will be incremented for each positional argument specified.
    /// - This is _similar_ to `getopt(3)`'s `optind` value, but rather than
    ///   being the overall index, it is a value incremented each time the
    ///   particular option is specified on the command-line.
    pub count: usize,
}

impl Arg {
    /// Create a new argument handler.
    pub fn new(option: char) -> Self {
        Arg::default().option(option)
    }

    /// Specify the option character (name) for the option.
    pub fn option(self, option: char) -> Self {
        Arg { option, ..self }
    }

    /// Specify the requirement for the option.
    pub fn needs(self, needs: Need) -> Self {
        Arg { needs, ..self }
    }

    /// Specify the help text for the option.
    pub fn help(self, help: &str) -> Self {
        Arg {
            help: Some(help.into()),
            ..self
        }
    }

    /// Specify that the option must be provided on the command-line.
    pub fn required(self) -> Self {
        Arg {
            required: true,
            ..self
        }
    }
}

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let value = if self.needs == Need::Argument {
            " <value>"
        } else {
            ""
        };

        let required = if self.required { REQUIRED_STR } else { "" };

        let help: String = match &self.help {
            Some(help) => format!(" # {}", help),
            _ => "".into(),
        };

        write!(
            f,
            "{}{}{}{}{}",
            OPT_PREFIX, self.option, value, required, help
        )
    }
}

/// Settings used to control the parsers behaviour.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialOrd, Default, PartialEq)]
pub struct Settings {
    /// If set, ignore any unknown options; by default an unknown option is
    /// considered an error.
    ignore_unknown_options: bool,

    /// If set and no [POSITIONAL_HANDLER_OPT] [Arg] has been registered,
    /// ignore positional arguments rather than erroring.
    ignore_unknown_posn_args: bool,

    /// Don't automatically consume the argument immediately _after_
    /// a [Need::Argument] option. This effectively stops option _values_ from
    /// starting with a dash which is permitted by `getopt(3)`.
    ///
    /// > **Notes:** Setting this to `true` means the parsing will
    /// > no longer be "`getopt`-like".
    no_strict_options: bool,
}

impl Settings {
    /// Create a new settings object.
    pub fn new() -> Self {
        Settings::default()
    }

    /// Specify that unknown options should be silently ignored
    /// (by default, the first unknown option will generate an error).
    pub fn ignore_unknown_options(self) -> Self {
        Settings {
            ignore_unknown_options: true,
            ..self
        }
    }

    /// Specify that unknown positional arguments should be silently ignored
    /// (by default, they will generate an error).
    pub fn ignore_unknown_posn_args(self) -> Self {
        Settings {
            ignore_unknown_posn_args: true,
            ..self
        }
    }

    /// By default, arguments are parsed as they would be by `getopt(3)`
    /// whereby if an option is marked as requiring a value
    /// ([Need::Argument]) and the option is found on the command line, the
    /// next argument (whether it starts with a dash or not!) is "consumed" as
    /// the options argument.
    ///
    /// However, when this setting is enabled, option values cannot start with
    /// a dash.
    ///
    /// # Advice
    ///
    /// - If you want to your program to behave like the traditional `getopt(3)`,
    ///   leave this setting unset.
    /// - If you need your program to accept an argument starting with a dash
    ///   (for example, you have an option which could accept a negative
    ///   number), you should leave this setting unset.
    /// - If your program provides options and flags and you wish to minimse
    ///   the chance of a flag (particularly a numeric flag such as `-1` or `-2`)
    ///   being interpreted as an options value, consider setting this option to
    ///   disable support for option values starting with a dash.
    ///
    /// # Example
    ///
    /// If a program accepts a flag (`-f`) and an option that
    /// requires an value (`-r <value>`) and the following command-line is
    /// specified to the program...
    ///
    /// ```bash
    /// $ prog -r -f
    /// ```
    ///
    /// ... the outcome of the parse will depend on this setting:
    ///
    /// - If `no_strict_options=false` (the default), the command line will be
    ///   passed successfully and the `r` option ([Arg]) will be given the value
    ///   "`-f`" and the `f` option ([Arg]) will be considered to not have been
    ///   specified.
    ///
    ///   > **Note:**
    ///   >
    ///   > This is how the POSIX command line argument `getopt(3)` works.
    ///
    /// - If `no_strict_options=true`, the parse will fail with the error
    ///   `Error::MissingOptArg` since in this mode, option values may not begin
    ///   with a dash so the `-f` is treated as the next argument meaning the
    ///   user forgot to specify a value for the previous argument (`-r`), which
    ///   is an error.
    ///
    ///   > **Note:**
    ///   >
    ///   > This is an alternative behaviour adopted by some modern
    ///   > command line argument parsers.
    ///
    pub fn no_strict_options(self) -> Self {
        Settings {
            no_strict_options: true,
            ..self
        }
    }
}

/// Get a list of all command-line arguments specified to the program with
/// the program name (the first argument) removed.
///
/// # Note
///
/// Used with [App::parse_with_args()]. However, this isn't usually
/// required: just call [App::parse()].
pub fn get_args() -> Vec<String> {
    let mut args: Vec<String> = env::args().collect();

    // Remove program name
    let _ = args.remove(0);

    args
}

/// Represents a collection of arguments.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Args {
    /// Hash of Arg objects.
    ///
    /// - name: option name.
    /// - value: the argument details for the option.
    entries: HashMap<char, Rc<RefCell<Arg>>>,
}

impl Args {
    /// Create a new argument collection.
    pub fn new() -> Self {
        Args {
            entries: HashMap::<char, Rc<RefCell<Arg>>>::new(),
        }
    }

    /// Returns the number of registered arguments.
    fn len(&self) -> usize {
        self.entries.len()
    }

    /// Convenience method to add a set of arguments in one go.
    ///
    /// # Note
    ///
    /// Used by the test code.
    #[allow(dead_code)]
    fn set(&mut self, args: Vec<Arg>) {
        self.entries.clear();

        for arg in args {
            self.entries.insert(arg.option, Rc::new(RefCell::new(arg)));
        }
    }

    /// Register a single argument.
    pub fn add(&mut self, arg: Arg) {
        self.entries.insert(arg.option, Rc::new(RefCell::new(arg)));
    }

    /// Determine if an [Arg] with the specified option name has been registered.
    pub fn exists(&self, option: &char) -> bool {
        self.entries.get(option).is_some()
    }

    /// Returns the [Arg] with the specified option name.
    pub fn get(&self, option: char) -> Option<Arg> {
        // XXX: Lookup the value in the hash. If found, unwrap it, deref the
        // RefCell ("borrow()") and re-wrap as an Option!
        self.entries.get(&option).map(|a| a.borrow().clone())
    }
}

impl Default for Args {
    /// Create a default argument handlers object.
    fn default() -> Self {
        Self::new()
    }
}

/// The main object used to represent the program.
///
/// All consumers of the crate need to create a
/// single object of this type.
#[derive(Clone, Default, Debug, PartialEq)]
pub struct App<'a> {
    name: String,
    version: String,
    summary: String,
    help: String,
    notes: String,
    settings: Settings,
    args: Args,
    handler: Option<Rc<RefCell<Box<dyn Handler + 'a>>>>,
}

impl<'a> App<'a> {
    /// Create a new application object.
    pub fn new(name: &str) -> Self {
        App::default().name(name)
    }

    /// Specify the version of the program.
    fn name(self, name: &str) -> Self {
        App {
            name: name.into(),
            ..self
        }
    }

    /// Specify a set of argument handlers to parse the command-line with.
    pub fn args(self, args: Args) -> Self {
        App { args, ..self }
    }

    /// Specify the version of the program.
    pub fn version(self, version: &str) -> Self {
        App {
            version: version.into(),
            ..self
        }
    }

    /// Specify brief explanatory text for the program.
    pub fn summary(self, summary: &str) -> Self {
        App {
            summary: summary.into(),
            ..self
        }
    }

    /// Specify extended usage information for the program.
    pub fn help(self, help: &str) -> Self {
        App {
            help: help.into(),
            ..self
        }
    }

    /// Specify notes for the program.
    pub fn notes(self, notes: &str) -> Self {
        App {
            notes: notes.into(),
            ..self
        }
    }

    /// Specify any settings for the program.
    pub fn settings(self, settings: Settings) -> Self {
        App { settings, ..self }
    }

    /// If set, don't error if unknown options are specified - just
    /// ignore them.
    ///
    /// # Note
    ///
    /// This is an alternative to calling the `settings()` method.
    pub fn ignore_unknown_options(self) -> Self {
        App {
            settings: Settings {
                ignore_unknown_options: true,
                ..Default::default()
            },
            ..self
        }
    }

    /// If set, don't error if unknown positional arguments are specified - just
    /// ignore them.
    ///
    /// # Note
    ///
    /// This is an alternative to calling the `settings()` method.
    pub fn ignore_unknown_posn_args(self) -> Self {
        App {
            settings: Settings {
                ignore_unknown_posn_args: true,
                ..Default::default()
            },
            ..self
        }
    }

    /// If set, disallow option values from starting with as dash.
    ///
    /// See the [Settings] method of the same name for full details
    /// and an example showing the effect of this call.
    ///
    /// # Note
    ///
    /// This is an alternative to calling the `settings()` method.
    pub fn no_strict_options(self) -> Self {
        App {
            settings: Settings {
                no_strict_options: true,
                ..Default::default()
            },
            ..self
        }
    }

    /// Generate a help/usage statement from the registered [Arg]'s.
    ///
    /// This is called automatically when the user specifies `-h` _anywhere_
    /// on the command line; you do not need to register an [Arg] for `-h`.
    pub fn generate_help<W>(&self, writer: &mut W) -> Result<()>
    where
        W: Write + Send + Sync + 'static,
    {
        let mut lines = Vec::<String>::new();

        let line = format!("NAME:\n{}{}\n", USAGE_PREFIX_SPACES, self.name);
        lines.push(line);

        if !self.version.is_empty() {
            let line = format!("VERSION:\n{}{}\n", USAGE_PREFIX_SPACES, self.version);
            lines.push(line);
        }

        if !self.summary.is_empty() {
            let line = format!("SUMMARY:\n{}{}\n", USAGE_PREFIX_SPACES, self.summary.trim());
            lines.push(line);
        }

        let have_posn_handler = self.args.get(POSITIONAL_HANDLER_OPT).is_some();

        let posn_args = match have_posn_handler {
            true => " [ARGUMENT..]",
            false => "",
        };

        lines.push("USAGE:".into());

        let name: String = if self.name.is_empty() {
            env::args().collect::<Vec<String>>().pop().unwrap()
        } else {
            self.name.clone()
        };

        let line = format!("{}{} [FLAGS]{}\n", USAGE_PREFIX_SPACES, name, posn_args);
        lines.push(line);

        //------------------------------------------------------------

        lines.push("FLAGS:".into());

        let mut keys: Vec<char> = self
            .args
            .entries
            .iter()
            .map(|(key, _)| *key)
            .filter(|k| *k != POSITIONAL_HANDLER_OPT)
            .collect();

        keys.sort_unstable();

        for key in keys.clone() {
            // Note: unwrap safe as 'keys' must be valid.
            let arg_ref = self.args.entries.get(&key).unwrap();

            let arg = arg_ref.borrow();

            if arg.needs == Need::Nothing {
                let line = format!("{}{}", USAGE_PREFIX_SPACES, arg.to_string());
                lines.push(line);
            }
        }

        //------------------------------------------------------------

        lines.push("\nOPTIONS:".into());

        for key in keys {
            let arg_ref = self.args.entries.get(&key).unwrap();

            let arg = arg_ref.borrow();

            if arg.needs != Need::Nothing {
                let line = format!("{}{}", USAGE_PREFIX_SPACES, arg.to_string());
                lines.push(line);
            }
        }

        //------------------------------------------------------------

        let pos_arg = self.args.entries.get(&POSITIONAL_HANDLER_OPT);
        if let Some(arg) = pos_arg {
            lines.push("\nPOSITIONAL ARGUMENTS:\n".into());

            // Extract the raw text to avoid the formatted Display version of
            // Arg (which exposes the value of POSITIONAL_HANDLER_OPT and
            // which also looks ugly).
            if let Some(help_text) = arg.borrow().help.as_ref() {
                lines.push(help_text.trim().into());
            } else {
                lines.push("Supported.".into());
            }
        }

        //------------------------------------------------------------

        if !self.help.is_empty() {
            let line = format!("\nHELP:\n\n{}", self.help.trim());
            lines.push(line);
        }

        if !self.notes.is_empty() {
            let line = format!("\nNOTES:\n\n{}", self.notes.trim());
            lines.push(line);
        }

        // Join all the lines together, remove white space at either and and
        // finally append a single newline.
        let mut final_lines = lines.join("\n").trim().to_string();
        final_lines.push('\n');

        writeln!(writer, "{}", final_lines)?;

        Ok(())
    }

    /// Parse a set of command line arguments (without the program name).
    ///
    /// # Arguments
    ///
    /// - `cli_args`: Vector of string arguments. Specify your own,
    ///    or call [get_args()].
    ///
    /// # Notes
    ///
    /// - The `cli_args` vector must _not_ specify the command name (which by default
    ///   is returned as the first element by `env::args().collect()` for example.
    ///   Use [get_args()] as this handles this for you.
    ///
    pub fn parse_with_args(&mut self, cli_args: Vec<String>) -> Result<()> {
        let mut need = Need::new();
        let mut end_of_options = false;

        // The short option name for the option that is currently being
        // handled. This will be None for flags and will only be set for
        // options if a handler has been specified.
        //
        // Ideally, we'd save a reference to the Arg, but there be dragons due
        // to multiple mutable borrows.
        let mut current_option: Option<char> = None;

        // Show help if requested.
        for cli_arg in cli_args.iter() {
            match cli_arg.as_str() {
                HELP_OPTION => return self.generate_help(&mut std::io::stdout()),
                // No more options so help was not requested
                END_OF_OPTIONS => break,
                _ => (),
            }
        }

        if self.handler.is_none() {
            return Err(Error::NoHandler);
        }

        if self.args.len() == 0 {
            return Err(Error::NoArgs);
        }

        for (_i, cli_arg) in cli_args.iter().enumerate() {
            if cli_arg.starts_with(OPT_PREFIX) && !end_of_options {
                // Found an option argument

                if cli_arg == END_OF_OPTIONS {
                    end_of_options = true;
                    continue;
                }

                // Handle the (relatively rare) scenario where a option's
                // _value_ starts with a dash.
                if !self.settings.no_strict_options {
                    if let Some(option) = current_option {
                        if need == Need::Argument {
                            if let Entry::Occupied(entry) = self.args.entries.entry(option) {
                                let mut arg = entry.get().borrow_mut();

                                // Save the value found
                                arg.value = Some(cli_arg.into());

                                if let Some(h) = self.handler.clone() {
                                    // Call the handler
                                    arg.count += 1;
                                    h.borrow_mut().handle(arg.clone())?;
                                }

                                // Job done
                                need = Need::Nothing;
                                current_option = None;
                                continue;
                            }
                        }
                    }
                }

                if cli_arg.starts_with(LONG_OPT_PREFIX) {
                    return Err(Error::NoLongOpts);
                }

                let mut chars = cli_arg.chars();

                if cli_arg.len() > 2 {
                    let option_name = *chars.nth(1).as_ref().unwrap();

                    if option_name.is_ascii_whitespace() {
                        return Err(Error::MissingOptName);
                    } else {
                        return Err(Error::NoBundling);
                    }
                }

                let option = chars.nth(1).ok_or(Error::MissingOptName)?;

                if need == Need::Argument {
                    return Err(Error::MissingOptArg);
                }

                if let Entry::Occupied(entry) = self.args.entries.entry(option) {
                    let arg_ref = entry.get();

                    let mut arg = arg_ref.borrow_mut();

                    need = arg.needs;

                    if need == Need::Nothing {
                        if let Some(h) = self.handler.clone() {
                            // Call the handler
                            arg.count += 1;
                            h.borrow_mut().handle(arg.clone())?;
                        }
                    } else if need == Need::Argument {
                        // Record the fact that we're "in the middle" of handling
                        // a particular option and that the next argument will
                        // be this options value.
                        current_option = Some(arg.option);
                    }
                } else if let Entry::Occupied(entry) =
                    self.args.entries.entry(UNKNOWN_OPTION_HANDLER_OPT)
                {
                    let arg_ref = entry.get();

                    let mut arg = arg_ref.borrow_mut();

                    if let Some(h) = self.handler.clone() {
                        // Call the handler
                        arg.count += 1;

                        // Pass a copy of the unknown Arg (which is
                        // generic) to the handler function for the _specific_
                        // unknown option found.
                        //
                        // This is simple, but has the downside of not having
                        // an accurate `.count` value. We could handle this in
                        // a better way (maybe by adding an `Arg` for each
                        // unknown option found?), but if we do that, we
                        // should also mark those `Arg`'s somehow to show they
                        // were "auto-added" rather than being added by the
                        // caller.
                        let mut option_specific = arg.clone();
                        option_specific.option = option;

                        h.borrow_mut().handle(option_specific)?;
                    }
                } else if self.settings.ignore_unknown_options {
                    continue;
                } else {
                    return Err(Error::UnknownOpt);
                }
            } else if let Some(option) = current_option {
                if let Entry::Occupied(entry) = self.args.entries.entry(option) {
                    let mut arg = entry.get().borrow_mut();

                    // Save the value found
                    arg.value = Some(cli_arg.into());

                    if let Some(h) = self.handler.clone() {
                        // Call the handler
                        arg.count += 1;
                        h.borrow_mut().handle(arg.clone())?;
                    }
                } else {
                    // This strictly is an "impossible" situation whereby
                    // a previously registered handler was somehow removed.
                    return Err(Error::NoHandler);
                }

                // Job done
                need = Need::Nothing;
                current_option = None;
            } else {
                // Positional (non-option) parameter
                if let Entry::Occupied(entry) = self.args.entries.entry(POSITIONAL_HANDLER_OPT) {
                    let arg_ref = entry.get();

                    let mut arg = arg_ref.borrow_mut();

                    // Found the option value, so save it
                    arg.value = Some(cli_arg.into());

                    if let Some(h) = self.handler.clone() {
                        // Call the handler
                        arg.count += 1;
                        h.borrow_mut().handle(arg.clone())?;
                    }
                } else if !self.settings.ignore_unknown_posn_args {
                    // After all options have been handled, anything
                    // can follow.
                    if !end_of_options {
                        return Err(Error::NoPosnArgs);
                    }
                }

                // Job done
                need = Need::Nothing;
                current_option = None;
            }
        }

        // Final checks
        for arg_ref in self.args.entries.values() {
            let arg = arg_ref.borrow();

            // There shouldn't be any half-handled options left
            if let Some(option) = current_option {
                if arg.needs == Need::Argument && option == arg.option {
                    return Err(Error::MissingOptArg);
                }
            }

            // Check that mandatory options were specified
            if arg.required && arg.count == 0 {
                return Err(Error::MissingReqOpt);
            }
        }

        Ok(())
    }

    /// Specify the handler for the option which must implement the
    /// [Handler] trait.
    ///
    /// # Note
    ///
    /// If the handler needs to modify its own state when called,
    /// the specified boxed trait must provide a mutable reference.
    pub fn handler(self, boxed_handler: Box<dyn Handler + 'a>) -> Self {
        let boxed = Rc::new(RefCell::new(boxed_handler));

        App {
            handler: Some(boxed),
            ..self
        }
    }

    /// Simplest interface to the parser.
    pub fn parse(&mut self) -> Result<()> {
        let args = get_args();

        self.parse_with_args(args)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use regex::Regex;
    use std::sync::{Arc, Mutex};

    /// Writer that stores all data written to it.
    #[derive(Default, Clone)]
    struct BufWriter(Arc<Mutex<Vec<u8>>>);

    impl std::io::Write for BufWriter {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.0.lock().unwrap().write(buf)
        }

        fn flush(&mut self) -> std::io::Result<()> {
            self.0.lock().unwrap().flush()
        }
    }

    #[allow(dead_code)]
    impl BufWriter {
        fn new() -> Self {
            BufWriter::default()
        }

        fn clear(&mut self) {
            self.0.lock().unwrap().clear();
        }

        fn len(&mut self) -> usize {
            self.0.lock().unwrap().len()
        }

        fn capacity(&mut self) -> usize {
            self.0.lock().unwrap().capacity()
        }
    }

    impl ToString for BufWriter {
        fn to_string(&self) -> String {
            let data_ref = self.0.clone();
            let output = data_ref.lock().unwrap();
            let s = (*output).clone();
            let output = String::from_utf8(s).unwrap();

            output
        }
    }

    #[test]
    fn test_requirement() {
        let r1 = Need::new();
        let r2 = Need::default();

        assert_eq!(r1, Need::Nothing);
        assert_eq!(r1, r2);
    }

    #[test]
    fn test_settings() {
        let new_settings = Settings::new();
        let def_settings = Settings::default();

        assert_eq!(new_settings.ignore_unknown_options, false);
        assert_eq!(new_settings.ignore_unknown_posn_args, false);
        assert_eq!(new_settings.no_strict_options, false);
        assert_eq!(new_settings, def_settings);

        let settings = Settings::new()
            .ignore_unknown_options()
            .ignore_unknown_posn_args()
            .no_strict_options();

        assert_eq!(settings.ignore_unknown_options, true);
        assert_eq!(settings.ignore_unknown_posn_args, true);
        assert_eq!(settings.no_strict_options, true);
    }

    #[test]
    fn test_arg() {
        //--------------------
        let default_arg = Arg::default();

        let expected_default = Arg {
            option: '\u{0}',
            needs: Need::Nothing,
            help: None,
            required: false,
            value: None,
            count: 0,
        };

        assert_eq!(default_arg, expected_default);

        //--------------------

        let new_arg = Arg::new('a');

        let expected_new = Arg {
            option: 'a',
            needs: Need::Nothing,
            help: None,
            required: false,
            value: None,
            count: 0,
        };

        assert_eq!(new_arg, expected_new);

        //--------------------

        let option_arg = Arg::new('a').option('b');

        assert_eq!(option_arg.option, 'b');

        //--------------------

        let def_option_arg = Arg::default().option('a');

        assert_eq!(def_option_arg.option, 'a');

        //--------------------

        let needs_arg = Arg::new('a').needs(Need::Argument);

        assert_eq!(needs_arg.needs, Need::Argument);

        //--------------------

        let help = "some help text\nfoo bar\nthe end";
        let help_arg = Arg::new('a').help(help);
        assert_eq!(help_arg.help, Some(help.into()));

        //--------------------

        let required_arg = Arg::new('a').required();
        assert_eq!(required_arg.required, true);
    }

    #[test]
    fn test_args() {
        let new_args = Args::new();
        let def_args = Args::default();

        assert_eq!(new_args, def_args);

        let mut args = Args::new();

        assert_eq!(args.len(), 0);
        assert!(!args.exists(&'a'));
        assert!(!args.exists(&'b'));
        assert_eq!(args.get('a'), None);
        assert_eq!(args.get('b'), None);

        let arg_1 = Arg::new('a');
        let arg_2 = Arg::new('b');

        args.add(arg_1);
        assert_eq!(args.len(), 1);
        assert!(args.exists(&'a'));
        assert!(!args.exists(&'b'));
        assert!(args.get('a').is_some());
        assert_eq!(args.get('a').unwrap().option, 'a');
        assert_eq!(args.get('b'), None);

        args.add(arg_2);
        assert_eq!(args.len(), 2);
        assert!(args.exists(&'a'));
        assert!(args.exists(&'b'));
        assert!(args.get('a').is_some());
        assert!(args.get('b').is_some());
        assert_eq!(args.get('a').unwrap().option, 'a');
        assert_eq!(args.get('b').unwrap().option, 'b');
    }

    //----------------------------------------
    // Handle that always succeeds

    #[derive(Clone, Debug, Default)]
    struct OkHandler {}

    impl Handler for &OkHandler {
        fn handle(&mut self, arg: Arg) -> Result<()> {
            assert!(arg.count > 0);

            Ok(())
        }
    }

    impl Handler for &mut OkHandler {
        fn handle(&mut self, arg: Arg) -> Result<()> {
            assert!(arg.count > 0);

            Ok(())
        }
    }

    //----------------------------------------
    // Handle that always fails

    #[derive(Clone, Debug, Default)]
    struct ErrHandler {}

    const TEST_ERR: &str = "dang";

    impl Handler for &ErrHandler {
        fn handle(&mut self, arg: Arg) -> Result<()> {
            assert!(arg.count > 0);

            Err(Error::GenericError(TEST_ERR.into()))
        }
    }

    impl Handler for &mut ErrHandler {
        fn handle(&mut self, _arg: Arg) -> Result<()> {
            Err(Error::GenericError(TEST_ERR.into()))
        }
    }

    //----------------------------------------
    // Handle that modifies itself

    #[derive(Clone, Debug, Default, PartialEq)]
    struct ModifyHandler {
        // Some random fields to demonstrate behaviour
        i: usize,
        v: Vec<String>,
        s: String,

        // Overall count of options parsed
        count: usize,

        a_count: usize,
        b_count: usize,
        d_count: usize,
        e_count: usize,
        r_count: usize,
    }

    const INT_INCREMENT: usize = 17;
    const HANDLER_SET_MSG: &str = "set by handler";

    impl Handler for &mut ModifyHandler {
        fn handle(&mut self, arg: Arg) -> Result<()> {
            assert!(arg.count > 0);

            self.count += arg.count;

            self.i += INT_INCREMENT;
            self.s = HANDLER_SET_MSG.into();

            if let Some(value) = arg.value {
                self.v.push(value);
            }

            match arg.option {
                'a' => self.a_count += 1,
                'b' => self.b_count += 1,
                'd' => self.d_count += 1,
                'e' => self.e_count += 1,
                'r' => self.r_count += 1,
                _ => (),
            };

            Ok(())
        }
    }

    #[test]
    fn test_parse_with_args() {
        #[derive(Debug)]
        struct TestData<'a> {
            cli_args: Vec<&'a str>,
            args: Option<Vec<Arg>>,
            use_handler: bool,
            result: Result<()>,
        }

        let mut ok_handler = OkHandler::default();

        let need_arg_opt_1 = Arg::new('a').needs(Need::Argument);
        let need_arg_opt_2 = Arg::new('b').needs(Need::Argument);
        let need_arg_opt_3 = Arg::new('3').needs(Need::Argument);

        let flag_opt = Arg::new('d').needs(Need::Nothing);
        let flag_opt_2 = Arg::new('人').needs(Need::Nothing);
        let flag_opt_3 = Arg::new('0').needs(Need::Nothing);

        let required_flag_opt = Arg::new('e').needs(Need::Nothing).required();
        let required_need_arg_opt = Arg::new('r').needs(Need::Argument).required();

        let tests = &[
            TestData {
                cli_args: vec![],
                args: None,
                use_handler: false,
                result: Err(Error::NoHandler),
            },
            TestData {
                cli_args: vec![],
                args: None,
                use_handler: true,
                result: Err(Error::NoArgs),
            },
            TestData {
                cli_args: vec![],
                args: Some(vec![need_arg_opt_1.clone()]),
                use_handler: true,
                result: Ok(()),
            },
            TestData {
                cli_args: vec![],
                args: Some(vec![need_arg_opt_1.clone(), need_arg_opt_2.clone()]),
                use_handler: true,
                result: Ok(()),
            },
            TestData {
                cli_args: vec![],
                args: Some(vec![required_flag_opt.clone()]),
                use_handler: true,
                result: Err(Error::MissingReqOpt),
            },
            TestData {
                cli_args: vec![],
                args: Some(vec![need_arg_opt_1.clone(), required_flag_opt.clone()]),
                use_handler: true,
                result: Err(Error::MissingReqOpt),
            },
            TestData {
                cli_args: vec![],
                args: Some(vec![required_need_arg_opt.clone()]),
                use_handler: true,
                result: Err(Error::MissingReqOpt),
            },
            TestData {
                cli_args: vec!["-"],
                args: Some(vec![flag_opt.clone()]),
                use_handler: true,
                result: Err(Error::MissingOptName),
            },
            TestData {
                // Weird
                cli_args: vec!["- -"],
                args: Some(vec![flag_opt.clone()]),
                use_handler: true,
                result: Err(Error::MissingOptName),
            },
            TestData {
                // Weird
                cli_args: vec!["- - "],
                args: Some(vec![flag_opt.clone()]),
                use_handler: true,
                result: Err(Error::MissingOptName),
            },
            TestData {
                // Odd positional argument
                cli_args: vec![" - "],
                args: Some(vec![flag_opt.clone()]),
                use_handler: true,
                result: Err(Error::NoPosnArgs),
            },
            TestData {
                cli_args: vec![END_OF_OPTIONS],
                args: Some(vec![flag_opt.clone()]),
                use_handler: true,
                result: Ok(()),
            },
            TestData {
                // Odd positional argument
                cli_args: vec![" -"],
                args: Some(vec![flag_opt.clone()]),
                use_handler: true,
                result: Err(Error::NoPosnArgs),
            },
            TestData {
                // Another odd positional argument
                cli_args: vec![" --"],
                args: Some(vec![flag_opt.clone()]),
                use_handler: true,
                result: Err(Error::NoPosnArgs),
            },
            TestData {
                // Another odd positional argument
                cli_args: vec![" ---"],
                args: Some(vec![flag_opt.clone()]),
                use_handler: true,
                result: Err(Error::NoPosnArgs),
            },
            TestData {
                // Another odd positional argument
                cli_args: vec![" --- "],
                args: Some(vec![flag_opt.clone()]),
                use_handler: true,
                result: Err(Error::NoPosnArgs),
            },
            TestData {
                // An invalid long option
                cli_args: vec!["---"],
                args: Some(vec![flag_opt.clone()]),
                use_handler: true,
                result: Err(Error::NoLongOpts),
            },
            TestData {
                cli_args: vec!["--notsupported"],
                args: Some(vec![flag_opt.clone()]),
                use_handler: true,
                result: Err(Error::NoLongOpts),
            },
            TestData {
                cli_args: vec!["--not-supported"],
                args: Some(vec![flag_opt.clone()]),
                use_handler: true,
                result: Err(Error::NoLongOpts),
            },
            TestData {
                cli_args: vec![],
                args: Some(vec![required_flag_opt.clone()]),
                use_handler: true,
                result: Err(Error::MissingReqOpt),
            },
            TestData {
                cli_args: vec![],
                args: Some(vec![required_need_arg_opt.clone()]),
                use_handler: true,
                result: Err(Error::MissingReqOpt),
            },
            TestData {
                cli_args: vec!["--", "-r"],
                args: Some(vec![required_need_arg_opt.clone()]),
                use_handler: true,
                result: Err(Error::MissingReqOpt),
            },
            TestData {
                cli_args: vec!["-r"],
                args: Some(vec![required_need_arg_opt.clone()]),
                use_handler: true,
                result: Err(Error::MissingOptArg),
            },
            TestData {
                cli_args: vec!["-r", "--"],
                args: Some(vec![required_need_arg_opt.clone()]),
                use_handler: true,
                result: Err(Error::MissingOptArg),
            },
            TestData {
                cli_args: vec![],
                args: Some(vec![required_flag_opt.clone()]),
                use_handler: true,
                result: Err(Error::MissingReqOpt),
            },
            TestData {
                cli_args: vec!["-r"],
                args: Some(vec![required_need_arg_opt.clone()]),
                use_handler: true,
                result: Err(Error::MissingOptArg),
            },
            TestData {
                cli_args: vec!["-r", "--"],
                args: Some(vec![required_need_arg_opt.clone()]),
                use_handler: true,
                result: Err(Error::MissingOptArg),
            },
            TestData {
                cli_args: vec!["-r"],
                args: Some(vec![required_need_arg_opt.clone()]),
                use_handler: true,
                result: Err(Error::MissingOptArg),
            },
            TestData {
                cli_args: vec!["-r", "foo"],
                args: Some(vec![required_need_arg_opt.clone()]),
                use_handler: true,
                result: Ok(()),
            },
            TestData {
                cli_args: vec!["-3", "three", "-0"],
                args: Some(vec![need_arg_opt_3.clone(), flag_opt_3.clone()]),
                use_handler: true,
                result: Ok(()),
            },
            TestData {
                cli_args: vec!["-人"],
                args: Some(vec![flag_opt_2.clone()]),
                use_handler: true,
                // Since the UTF-8 char is >1 byte long and we don't support
                // unicode chars.
                result: Err(Error::NoBundling),
            },
            TestData {
                cli_args: vec!["-ab"],
                args: Some(vec![flag_opt.clone()]),
                use_handler: true,
                result: Err(Error::NoBundling),
            },
            TestData {
                cli_args: vec!["-abc"],
                args: Some(vec![flag_opt.clone()]),
                use_handler: true,
                result: Err(Error::NoBundling),
            },
            TestData {
                cli_args: vec!["--", "-abc"],
                args: Some(vec![flag_opt.clone()]),
                use_handler: true,
                result: Ok(()),
            },
            TestData {
                cli_args: vec!["--", "abc", "def", "hello world", "--wibble", "-abc"],
                args: Some(vec![flag_opt.clone()]),
                use_handler: true,
                result: Ok(()),
            },
            TestData {
                cli_args: vec!["--foo"],
                args: Some(vec![flag_opt.clone()]),
                use_handler: true,
                result: Err(Error::NoLongOpts),
            },
            TestData {
                cli_args: vec!["-y"],
                args: Some(vec![flag_opt.clone()]),
                use_handler: true,
                result: Err(Error::UnknownOpt),
            },
        ];

        for (i, d) in tests.iter().enumerate() {
            // Create a string containing details of the test
            //
            // Note: Ideally we'd just format 'd', but we can't since rust
            // doesn't implement Debug for (all) function pointers at the time
            // of writing.
            let msg = format!("test[{}]: {:?}", i, d);

            let string_args = d.cli_args.clone().into_iter().map(String::from).collect();

            let mut app: App;

            if d.use_handler {
                app = App::default().handler(Box::new(&mut ok_handler));
            } else {
                app = App::default();
            }

            let mut args = Args::default();

            if let Some(a) = d.args.clone() {
                args.set(a);
            }

            app = app.args(args);

            // Call the function under test
            let result = app.parse_with_args(string_args);

            // Update the test details string with the results of the call
            let msg = format!("{}, result: {:?}", msg, result);

            // Perform the checks
            if d.result.is_ok() {
                assert!(result.is_ok(), "{}", msg);
                continue;
            }

            assert!(result.is_err(), "{}", msg);

            let expected_err = format!("{:?}", d.result.as_ref().err());
            let actual_err = format!("{:?}", result.as_ref().err());

            assert_eq!(actual_err, expected_err, "{}", msg);
        }
    }

    #[test]
    fn test_parse_with_handler() {
        #[derive(Debug)]
        struct TestData<'a> {
            cli_args: Vec<&'a str>,
            args: Vec<Arg>,
            result: Result<()>,
            // The expected state of the handler after parsing.
            handler_result: ModifyHandler,
        }

        let need_arg_opt_1 = Arg::new('a').needs(Need::Argument);

        let flag_opt = Arg::new('d').needs(Need::Nothing);

        let tests = &[
            TestData {
                cli_args: vec![],
                args: vec![flag_opt.clone()],
                result: Ok(()),
                handler_result: ModifyHandler::default(),
            },
            TestData {
                cli_args: vec!["-d"],
                args: vec![flag_opt.clone()],
                result: Ok(()),
                handler_result: ModifyHandler {
                    i: INT_INCREMENT,
                    s: HANDLER_SET_MSG.into(),
                    count: 1,
                    d_count: 1,
                    ..ModifyHandler::default()
                },
            },
            TestData {
                cli_args: vec!["-a", "foo"],
                args: vec![need_arg_opt_1.clone()],
                result: Ok(()),
                handler_result: ModifyHandler {
                    i: INT_INCREMENT,
                    s: HANDLER_SET_MSG.into(),
                    v: vec!["foo".into()],
                    count: 1,
                    a_count: 1,
                    ..ModifyHandler::default()
                },
            },
            TestData {
                cli_args: vec!["-a", "foo", "-a", "bar", "-a", "baz"],
                args: vec![need_arg_opt_1.clone()],
                result: Ok(()),
                handler_result: ModifyHandler {
                    i: INT_INCREMENT * 3,
                    s: HANDLER_SET_MSG.into(),
                    v: vec!["foo".into(), "bar".into(), "baz".into()],
                    // XXX: arg.count was 1 initially, then 2, then 3.
                    count: (1 + 2 + 3),
                    a_count: 3,
                    ..ModifyHandler::default()
                },
            },
            TestData {
                cli_args: vec!["-d", "-a", "foo", "-d", "-a", "bar", "-a", "baz"],
                args: vec![flag_opt.clone(), need_arg_opt_1.clone()],
                result: Ok(()),
                handler_result: ModifyHandler {
                    i: INT_INCREMENT * 5,
                    s: HANDLER_SET_MSG.into(),
                    v: vec!["foo".into(), "bar".into(), "baz".into()],
                    // Values for the '-a' handler plus values for the '-d'
                    // handler.
                    count: (1 + 2 + 3) + (1 + 2),
                    a_count: 3,
                    d_count: 2,
                    ..ModifyHandler::default()
                },
            },
        ];

        for (i, d) in tests.iter().enumerate() {
            let msg = format!("test[{}]: {:?}", i, d);
            let string_args: Vec<String> =
                d.cli_args.clone().into_iter().map(String::from).collect();

            let mut args = Args::default();

            args.set(d.args.clone());

            let mut handler = ModifyHandler::default();
            let mut app = App::default().args(args).handler(Box::new(&mut handler));

            let result = app.parse_with_args(string_args);

            drop(app);

            if result.is_err() {
                assert!(d.result.is_err(), "{}", msg);

                let expected_err = format!("{:?}", d.result.as_ref().err());
                let actual_err = format!("{:?}", result.as_ref().err());
                assert_eq!(expected_err, actual_err, "{}", msg);

                continue;
            }

            assert!(result.is_ok(), "{}", msg);
            assert_eq!(d.handler_result, handler, "{}", msg);
        }
    }

    #[test]
    fn test_parse_with_bad_handler() {
        let flag_opt = Arg::new('d').needs(Need::Nothing);

        let mut args = Args::default();
        args.add(flag_opt);

        // Handler that should fail the parse
        let mut handler = ErrHandler::default();
        let mut app = App::default().args(args).handler(Box::new(&mut handler));

        let result = app.parse_with_args(vec!["-d".into()]);
        assert!(result.is_err());

        let expected_err = format!("{:?}", Error::GenericError(TEST_ERR.into()));
        let actual_err = format!("{:?}", result.err().unwrap());

        assert_eq!(expected_err, actual_err);
    }

    #[test]
    fn test_ensure_handler_not_called_on_parse_fail() {
        let mut handler = ModifyHandler::default();
        let mut args = Args::default();

        let need_arg_opt_1 = Arg::new('a').needs(Need::Argument);
        let need_arg_opt_2 = Arg::new('b').needs(Need::Argument);
        let flag_opt = Arg::new('d').needs(Need::Nothing);

        args.add(flag_opt);
        args.add(need_arg_opt_1);
        args.add(need_arg_opt_2);

        // An incorrect CLI (since the '-b' option is missing its required arg)
        let cli_args = vec!["-d", "-a", "foo bar", "-d", "-a", "hello world", "-b"];

        let string_args = cli_args.clone().into_iter().map(String::from).collect();

        let mut app = App::default().args(args).handler(Box::new(&mut handler));
        let result = app.parse_with_args(string_args);

        assert!(result.is_err());

        drop(app);

        let expected_handler = ModifyHandler {
            i: INT_INCREMENT * 4,
            s: HANDLER_SET_MSG.into(),
            v: vec!["foo bar".into(), "hello world".into()],
            // Values for the '-a' handler plus values for the '-d'
            // handler.
            count: (1 + 2) + (1 + 2),
            a_count: 2,
            d_count: 2,
            b_count: 0, // XXX: Crucially, this should not be set!
            ..ModifyHandler::default()
        };

        assert_eq!(expected_handler, handler);
    }

    #[test]
    fn test_parse_positional_args() {
        let posn_arg = Arg::new(POSITIONAL_HANDLER_OPT);

        #[derive(Debug)]
        struct TestData<'a> {
            cli_args: Vec<&'a str>,
            result: Result<()>,
            // The expected state of the handler after parsing.
            handler_result: ModifyHandler,
        }

        let tests = &[
            TestData {
                cli_args: vec![],
                result: Ok(()),
                handler_result: ModifyHandler::default(),
            },
            TestData {
                cli_args: vec!["foo"],
                result: Ok(()),
                handler_result: ModifyHandler {
                    i: INT_INCREMENT,
                    v: vec!["foo".into()],
                    s: HANDLER_SET_MSG.into(),
                    count: 1,
                    ..Default::default()
                },
            },
            TestData {
                cli_args: vec!["\\- -"],
                result: Ok(()),
                handler_result: ModifyHandler {
                    i: INT_INCREMENT,
                    v: vec!["\\- -".into()],
                    s: HANDLER_SET_MSG.into(),
                    count: 1,
                    ..Default::default()
                },
            },
            TestData {
                cli_args: vec!["\\- - "],
                result: Ok(()),
                handler_result: ModifyHandler {
                    i: INT_INCREMENT,
                    v: vec!["\\- - ".into()],
                    s: HANDLER_SET_MSG.into(),
                    count: 1,
                    ..Default::default()
                },
            },
            TestData {
                cli_args: vec![" - "],
                result: Ok(()),
                handler_result: ModifyHandler {
                    i: INT_INCREMENT,
                    v: vec![" - ".into()],
                    s: HANDLER_SET_MSG.into(),
                    count: 1,
                    ..Default::default()
                },
            },
            TestData {
                cli_args: vec![" - -"],
                result: Ok(()),
                handler_result: ModifyHandler {
                    i: INT_INCREMENT,
                    v: vec![" - -".into()],
                    s: HANDLER_SET_MSG.into(),
                    count: 1,
                    ..Default::default()
                },
            },
            TestData {
                cli_args: vec![" - - "],
                result: Ok(()),
                handler_result: ModifyHandler {
                    i: INT_INCREMENT,
                    v: vec![" - - ".into()],
                    s: HANDLER_SET_MSG.into(),
                    count: 1,
                    ..Default::default()
                },
            },
            TestData {
                cli_args: vec![" -"],
                result: Ok(()),
                handler_result: ModifyHandler {
                    i: INT_INCREMENT,
                    v: vec![" -".into()],
                    s: HANDLER_SET_MSG.into(),
                    count: 1,
                    ..Default::default()
                },
            },
            TestData {
                cli_args: vec![" --"],
                result: Ok(()),
                handler_result: ModifyHandler {
                    i: INT_INCREMENT,
                    v: vec![" --".into()],
                    s: HANDLER_SET_MSG.into(),
                    count: 1,
                    ..Default::default()
                },
            },
            TestData {
                cli_args: vec![" ---"],
                result: Ok(()),
                handler_result: ModifyHandler {
                    i: INT_INCREMENT,
                    v: vec![" ---".into()],
                    s: HANDLER_SET_MSG.into(),
                    count: 1,
                    ..Default::default()
                },
            },
            TestData {
                cli_args: vec![" --- "],
                result: Ok(()),
                handler_result: ModifyHandler {
                    i: INT_INCREMENT,
                    v: vec![" --- ".into()],
                    s: HANDLER_SET_MSG.into(),
                    count: 1,
                    ..Default::default()
                },
            },
            TestData {
                cli_args: vec!["-d", "foo"],
                result: Err(Error::UnknownOpt),
                handler_result: ModifyHandler::default(),
            },
        ];

        for (i, d) in tests.iter().enumerate() {
            let msg = format!("test[{}]: {:?}", i, d);
            let string_args: Vec<String> =
                d.cli_args.clone().into_iter().map(String::from).collect();

            let mut args = Args::default();
            args.add(posn_arg.clone());

            let mut handler = ModifyHandler::default();
            let mut app = App::default().args(args).handler(Box::new(&mut handler));

            let result = app.parse_with_args(string_args);

            let msg = format!("{}, result: {:?}", msg, result);

            drop(app);

            if result.is_err() {
                assert!(d.result.is_err(), "{}", msg);

                let expected_err = format!("{:?}", d.result.as_ref().err());
                let actual_err = format!("{:?}", result.as_ref().err());
                assert_eq!(expected_err, actual_err, "{}", msg);

                continue;
            }

            assert!(result.is_ok(), "{}", msg);
            assert_eq!(d.handler_result, handler, "{}", msg);
        }
    }

    #[test]
    fn test_parse_unknown_options() {
        let unknown_opts_arg = Arg::new(UNKNOWN_OPTION_HANDLER_OPT);

        let mut args = Args::default();
        args.add(Arg::new('d'));
        args.add(unknown_opts_arg);

        let cli_args = vec!["-d", "-a", "-b", "-r", "-d"];

        let string_args = cli_args.clone().into_iter().map(String::from).collect();

        let mut handler = ModifyHandler::default();
        assert_eq!(handler.count, 0);

        let mut app = App::default()
            .args(args)
            .ignore_unknown_options()
            .handler(Box::new(&mut handler));

        let result = app.parse_with_args(string_args);

        assert!(result.is_ok());

        drop(app);

        // '-d' was specified twice and '-?' was specified three times
        let expected_count = (1 + 2) + (1 + 2 + 3);
        assert_eq!(handler.count, expected_count);

        assert_eq!(handler.a_count, 1);
        assert_eq!(handler.b_count, 1);
        assert_eq!(handler.d_count, 2);
        assert_eq!(handler.r_count, 1);
    }

    #[test]
    fn test_intermingling_arguments() {
        let mut handler = ModifyHandler::default();
        let mut args = Args::default();

        args.add(Arg::new('a').needs(Need::Argument));
        args.add(Arg::new('d').needs(Need::Nothing));
        args.add(Arg::new(POSITIONAL_HANDLER_OPT));

        let mut app = App::default()
            .help("some text")
            .args(args)
            .handler(Box::new(&mut handler));

        // A mixture of options, flags and positional arguments.
        let cli_args = vec![
            "the start",
            "-d",
            "foo bar",
            "-a",
            "hello world",
            "-d",
            "alpha omega",
            "one",
            "two",
            "-d",
            "-a",
            "moo bar haz",
            "the end",
        ];

        let string_args = cli_args.clone().into_iter().map(String::from).collect();
        let result = app.parse_with_args(string_args);

        assert!(result.is_ok());

        drop(app);

        let expected_handler = ModifyHandler {
            i: 187,
            v: vec![
                "the start".into(),
                "foo bar".into(),
                "hello world".into(),
                "alpha omega".into(),
                "one".into(),
                "two".into(),
                "moo bar haz".into(),
                "the end".into(),
            ],
            s: "set by handler".into(),
            count: 30,
            a_count: 2,
            d_count: 3,
            ..Default::default()
        };

        assert_eq!(handler, expected_handler);
    }

    #[test]
    fn test_ignore_unknown_options() {
        #[derive(Debug)]
        struct TestData<'a> {
            cli_args: Vec<&'a str>,
            args: Vec<Arg>,
            allow_unknown_options: bool,
            result: Result<()>,
        }

        let need_arg_opt = Arg::new('a').needs(Need::Argument);
        let flag_opt = Arg::new('d').needs(Need::Nothing);

        let tests = &[
            TestData {
                cli_args: vec!["-z"],
                args: vec![need_arg_opt.clone()],
                allow_unknown_options: false,
                result: Err(Error::UnknownOpt),
            },
            TestData {
                cli_args: vec!["-z"],
                args: vec![need_arg_opt.clone()],
                allow_unknown_options: true,
                result: Ok(()),
            },
            //------------------------------
            TestData {
                cli_args: vec!["-z"],
                args: vec![flag_opt.clone()],
                allow_unknown_options: false,
                result: Err(Error::UnknownOpt),
            },
            TestData {
                cli_args: vec!["-z"],
                args: vec![flag_opt.clone()],
                allow_unknown_options: true,
                result: Ok(()),
            },
            //------------------------------
            TestData {
                cli_args: vec!["-z", "-a", "foo"],
                args: vec![need_arg_opt.clone()],
                allow_unknown_options: false,
                result: Err(Error::UnknownOpt),
            },
            TestData {
                cli_args: vec!["-a", "foo", "-z"],
                args: vec![need_arg_opt.clone()],
                allow_unknown_options: false,
                result: Err(Error::UnknownOpt),
            },
            //------------------------------
            TestData {
                cli_args: vec!["-z", "-a", "foo"],
                args: vec![need_arg_opt.clone()],
                allow_unknown_options: true,
                result: Err(Error::UnknownOpt),
            },
            TestData {
                cli_args: vec!["-a", "foo", "-z"],
                args: vec![need_arg_opt.clone()],
                allow_unknown_options: true,
                result: Ok(()),
            },
            //------------------------------
            TestData {
                cli_args: vec!["-z", "-d"],
                args: vec![flag_opt.clone()],
                allow_unknown_options: false,
                result: Err(Error::UnknownOpt),
            },
            TestData {
                cli_args: vec!["-d", "-z"],
                args: vec![flag_opt.clone()],
                allow_unknown_options: false,
                result: Err(Error::UnknownOpt),
            },
            //------------------------------
            TestData {
                cli_args: vec!["-z", "-d"],
                args: vec![flag_opt.clone()],
                allow_unknown_options: true,
                result: Ok(()),
            },
            TestData {
                cli_args: vec!["-d", "-z"],
                args: vec![flag_opt.clone()],
                allow_unknown_options: true,
                result: Ok(()),
            },
        ];

        for (i, d) in tests.iter().enumerate() {
            let msg = format!("test[{}]: {:?}", i, d);
            let string_args: Vec<String> =
                d.cli_args.clone().into_iter().map(String::from).collect();

            let mut args = Args::default();
            args.set(d.args.clone());

            let mut handler = OkHandler::default();
            let mut app = App::default().args(args).handler(Box::new(&mut handler));

            if d.allow_unknown_options {
                app = app.ignore_unknown_options();
            }

            let result = app.parse_with_args(string_args);

            let msg = format!("{}, result: {:?}", msg, result);

            if result.is_err() {
                assert!(d.result.is_err(), "{}", msg);

                let expected_err = format!("{:?}", d.result.as_ref().err());
                let actual_err = format!("{:?}", result.as_ref().err());
                assert_eq!(expected_err, actual_err, "{}", msg);

                continue;
            }

            assert!(result.is_ok(), "{}", msg);
        }
    }

    #[test]
    fn test_ignore_unknown_posn_args() {
        #[derive(Debug)]
        struct TestData<'a> {
            cli_args: Vec<&'a str>,
            args: Vec<Arg>,
            allow_unknown_posn_args: bool,
            result: Result<()>,
        }

        let need_arg_opt = Arg::new('a').needs(Need::Argument);
        let flag_opt = Arg::new('d').needs(Need::Nothing);

        let tests = &[
            TestData {
                cli_args: vec!["foo bar"],
                args: vec![need_arg_opt.clone()],
                allow_unknown_posn_args: false,
                result: Err(Error::NoPosnArgs),
            },
            TestData {
                cli_args: vec!["foo bar"],
                args: vec![need_arg_opt.clone()],
                allow_unknown_posn_args: true,
                result: Ok(()),
            },
            //------------------------------
            TestData {
                cli_args: vec!["foo bar"],
                args: vec![flag_opt.clone()],
                allow_unknown_posn_args: false,
                result: Err(Error::NoPosnArgs),
            },
            TestData {
                cli_args: vec!["foo bar"],
                args: vec![flag_opt.clone()],
                allow_unknown_posn_args: true,
                result: Ok(()),
            },
            //------------------------------
            TestData {
                cli_args: vec!["foo bar", "-a", "foo"],
                args: vec![need_arg_opt.clone()],
                allow_unknown_posn_args: false,
                result: Err(Error::NoPosnArgs),
            },
            TestData {
                cli_args: vec!["-a", "foo", "foo bar"],
                args: vec![need_arg_opt.clone()],
                allow_unknown_posn_args: false,
                result: Err(Error::NoPosnArgs),
            },
            //------------------------------
            TestData {
                cli_args: vec!["foo bar", "-a", "foo"],
                args: vec![need_arg_opt.clone()],
                allow_unknown_posn_args: true,
                result: Err(Error::NoPosnArgs),
            },
            TestData {
                cli_args: vec!["-a", "foo", "foo bar"],
                args: vec![need_arg_opt.clone()],
                allow_unknown_posn_args: true,
                result: Ok(()),
            },
            //------------------------------
            TestData {
                cli_args: vec!["foo bar", "-d"],
                args: vec![flag_opt.clone()],
                allow_unknown_posn_args: false,
                result: Err(Error::NoPosnArgs),
            },
            TestData {
                cli_args: vec!["-d", "foo bar"],
                args: vec![flag_opt.clone()],
                allow_unknown_posn_args: false,
                result: Err(Error::NoPosnArgs),
            },
            //------------------------------
            TestData {
                cli_args: vec!["foo bar", "-d"],
                args: vec![flag_opt.clone()],
                allow_unknown_posn_args: true,
                result: Ok(()),
            },
            TestData {
                cli_args: vec!["-d", "foo bar"],
                args: vec![flag_opt.clone()],
                allow_unknown_posn_args: true,
                result: Ok(()),
            },
        ];

        for (i, d) in tests.iter().enumerate() {
            let msg = format!("test[{}]: {:?}", i, d);
            let string_args: Vec<String> =
                d.cli_args.clone().into_iter().map(String::from).collect();

            let mut args = Args::default();
            args.set(d.args.clone());

            let mut handler = OkHandler::default();
            let mut app = App::default().args(args).handler(Box::new(&mut handler));

            if d.allow_unknown_posn_args {
                app = app.ignore_unknown_posn_args();
            }

            let result = app.parse_with_args(string_args);

            let msg = format!("{}, result: {:?}", msg, result);

            if result.is_err() {
                assert!(d.result.is_err(), "{}", msg);

                let expected_err = format!("{:?}", d.result.as_ref().err());
                let actual_err = format!("{:?}", result.as_ref().err());
                assert_eq!(expected_err, actual_err, "{}", msg);

                continue;
            }

            assert!(result.is_ok(), "{}", msg);
        }
    }

    #[test]
    fn test_app_creation() {
        let new_app = App::new("foo bar");
        let def_app = App::default();

        let expected_def_app = App {
            name: "".into(),
            version: "".into(),
            summary: "".into(),
            help: "".into(),
            notes: "".into(),
            settings: Settings::default(),
            args: Args::default(),
            handler: None,
        };

        let expected_new_app = App {
            name: "foo bar".into(),
            ..Default::default()
        };

        assert_eq!(def_app, expected_def_app);
        assert_eq!(new_app, expected_new_app);
    }

    #[test]
    fn test_app() {
        let mut app = App::default();

        assert_eq!(app.name, "");
        let name = "foo bar";
        app = app.name(name);
        assert_eq!(app.name, name);

        let version = "1.2.3-beta5";
        assert_eq!(app.version, "");
        app = app.version(version);
        assert_eq!(app.version, version);

        let summary = "my awesome app";
        assert_eq!(app.summary, "");
        app = app.summary(summary);
        assert_eq!(app.summary, summary);

        let help = "this app does something\nthe end\n";
        assert_eq!(app.help, "");
        app = app.help(help);
        assert_eq!(app.help, help);

        let notes = "a b c d e f# g";
        assert_eq!(app.notes, "");
        app = app.notes(notes);
        assert_eq!(app.notes, notes);

        let settings = Settings::new().no_strict_options();
        let def_settings = Settings::new();
        assert_eq!(app.settings, def_settings);

        app = app.settings(settings);
        assert_eq!(app.settings, settings);
    }

    #[test]
    fn test_generate_help() {
        let mut writer = BufWriter::new();

        let mut args = Args::default();

        // flags
        args.add(Arg::new('d').help("enable debug"));
        args.add(Arg::new('e').required());
        args.add(Arg::new('f').required().help("force mode"));

        // options
        args.add(Arg::new('n').needs(Need::Argument));
        args.add(Arg::new('r').needs(Need::Argument).required());
        args.add(
            Arg::new('s')
                .needs(Need::Argument)
                .required()
                .help("silly option"),
        );

        let posn_help = "I am the positional handler \
             help text";

        // positional parameters magic option
        args.add(Arg::new(POSITIONAL_HANDLER_OPT).help(posn_help));

        let flags_re = concat!(
            r#"FLAGS:\n"#,
            r#"\s+-d # enable debug\n"#,
            r#"\s+-e \(required\)\n"#,
            r#"\s+-f \(required\) # force mode\n"#,
        );

        let options_re = concat!(
            r#"OPTIONS:\n"#,
            r#"\s+-n <value>\n"#,
            r#"\s+-r <value> \(required\)\n"#,
            r#"\s+-s <value> \(required\) # silly option\n"#,
        );

        let pos_handler_re = concat!(
            r#"POSITIONAL ARGUMENTS:\n\n"#,
            r#"I am the positional handler help text"#
        );

        //--------------------

        let name = "my app";
        let name_re = format!(r#"NAME:\n\s+{}\n"#, name);

        let version = "1.2.3-alpha4";
        let version_re = format!(r"VERSION:\n\s+{}\n", version);

        let summary = "This is one awesome app";
        let summary_re = format!(r"SUMMARY:\n\s+{}\n", summary);

        let help = concat!(
            "help line 1\n",
            "help line 2\n",
            "help line 3\n",
            "help last line\n"
        );
        let help_re = format!(r"HELP:\n\s+{}\n", help);

        let notes = concat!(
            "notes line 1\n",
            "notes line 2\n",
            "notes line 3\n",
            "notes last line\n"
        );

        let notes_re = format!(r"NOTES:\n\s+{}\n", notes);

        //--------------------

        let mut handler = OkHandler::default();

        let app = App::new(name)
            .summary(summary)
            .version(version)
            .help(help)
            .notes(notes)
            .args(args)
            .handler(Box::new(&mut handler));

        let result = app.generate_help(&mut writer);

        drop(app);

        assert!(result.is_ok());

        let value = writer.to_string();

        let re = Regex::new(&name_re).unwrap();
        assert!(re.is_match(&value));

        let re = Regex::new(&version_re).unwrap();
        assert!(re.is_match(&value));

        let re = Regex::new(&summary_re).unwrap();
        assert!(re.is_match(&value));

        let re = Regex::new(&help_re).unwrap();
        assert!(re.is_match(&value));

        let re = Regex::new(&notes_re).unwrap();
        assert!(re.is_match(&value));

        let re = Regex::new(&flags_re).unwrap();
        assert!(re.is_match(&value));

        let re = Regex::new(&options_re).unwrap();
        assert!(re.is_match(&value));

        println!("FIXME: value: {:?}", value);

        let re = Regex::new(&pos_handler_re).unwrap();
        assert!(re.is_match(&value));
    }

    #[test]
    fn test_get_args() {
        let get_args_result = get_args();

        let mut args: Vec<String> = env::args().collect();
        args.remove(0);

        assert_eq!(get_args_result, args);
    }

    #[test]
    fn test_arg_display() {
        #[derive(Debug)]
        struct TestData<'a> {
            arg: Arg,
            display: &'a str,
        }

        let tests = &[
            TestData {
                arg: Arg::new('d'),
                display: "-d",
            },
            TestData {
                arg: Arg::new('d').required(),
                display: "-d (required)",
            },
            //------------------------------
            TestData {
                arg: Arg::new('d').needs(Need::Nothing),
                display: "-d",
            },
            TestData {
                arg: Arg::new('d').required().needs(Need::Nothing),
                display: "-d (required)",
            },
            //------------------------------
            TestData {
                arg: Arg::new('r').needs(Need::Argument),
                display: "-r <value>",
            },
            TestData {
                arg: Arg::new('r').needs(Need::Argument).required(),
                display: "-r <value> (required)",
            },
            //------------------------------
            TestData {
                arg: Arg::new('r').needs(Need::Argument),
                display: "-r <value>",
            },
            TestData {
                arg: Arg::new('r').needs(Need::Argument).required(),
                display: "-r <value> (required)",
            },
            //------------------------------
            TestData {
                arg: Arg::new('d').help("some help text"),
                display: "-d # some help text",
            },
            TestData {
                arg: Arg::new('d').required().help("some help text"),
                display: "-d (required) # some help text",
            },
            //------------------------------
        ];

        for (i, d) in tests.iter().enumerate() {
            let value = format!("{:}", d.arg);

            let msg = format!("test[{}]: {:?}, value: {:?}", i, d, value);

            assert_eq!(value, d.display, "{}", msg);
        }
    }

    #[test]
    fn test_handler_display() {
        let mut ok_handler = OkHandler::default();
        let mut mod_handler = ModifyHandler::default();
        let mut err_handler = ErrHandler::default();

        let mut handlers: Vec<Box<dyn Handler>> = Vec::new();
        handlers.push(Box::new(&mut ok_handler));
        handlers.push(Box::new(&mut err_handler));
        handlers.push(Box::new(&mut mod_handler));

        for (i, handler) in handlers.iter().enumerate() {
            let p = handler;

            let value = format!("{:?}", p);

            let msg = format!("test[{}]: value: {:?}", i, value);

            assert!(value.starts_with("Handler: "), "{}", msg);
        }
    }

    #[test]
    fn test_no_strict_options() {
        #[derive(Debug)]
        struct TestData<'a> {
            cli_args: Vec<&'a str>,
            args: Vec<Arg>,
            no_strict_options: bool,
            result: Result<()>,
            r_count: usize,
            d_count: usize,
            values: Vec<&'a str>,
        }

        let flag_opt = Arg::new('d').needs(Need::Nothing);
        let need_arg_opt = Arg::new('r').needs(Need::Argument);

        let tests = &[
            TestData {
                cli_args: vec!["-r", "foo", "-d"],
                args: vec![flag_opt.clone(), need_arg_opt.clone()],
                no_strict_options: false,
                result: Ok(()),
                r_count: 1,
                d_count: 1,
                values: vec!["foo"],
            },
            TestData {
                cli_args: vec!["-r", "foo", "-d"],
                args: vec![flag_opt.clone(), need_arg_opt.clone()],
                no_strict_options: true,
                result: Ok(()),
                r_count: 1,
                d_count: 1,
                values: vec!["foo"],
            },
            //------------------------------
            TestData {
                cli_args: vec!["-r", "-d"],
                args: vec![flag_opt.clone(), need_arg_opt.clone()],
                no_strict_options: false,
                result: Ok(()),
                r_count: 1,
                d_count: 0,
                values: vec!["-d"],
            },
            TestData {
                cli_args: vec!["-r", "-d"],
                args: vec![flag_opt.clone(), need_arg_opt.clone()],
                no_strict_options: true,
                result: Err(Error::MissingOptArg),
                r_count: 0,
                d_count: 0,
                values: vec![],
            },
            //------------------------------
            TestData {
                cli_args: vec!["-r", "--"],
                args: vec![need_arg_opt.clone()],
                no_strict_options: false,
                result: Err(Error::MissingOptArg),
                r_count: 0,
                d_count: 0,
                values: vec![],
            },
            TestData {
                cli_args: vec!["-r", "--"],
                args: vec![need_arg_opt.clone()],
                no_strict_options: true,
                result: Err(Error::MissingOptArg),
                r_count: 0,
                d_count: 0,
                values: vec![],
            },
            //------------------------------
            TestData {
                cli_args: vec!["-r", "-d", "--"],
                args: vec![need_arg_opt.clone()],
                no_strict_options: false,
                result: Ok(()),
                r_count: 1,
                d_count: 0,
                values: vec!["-d"],
            },
            TestData {
                cli_args: vec!["-r", "-d", "--"],
                args: vec![need_arg_opt.clone()],
                no_strict_options: true,
                result: Err(Error::MissingOptArg),
                r_count: 0,
                d_count: 0,
                values: vec![],
            },
        ];

        for (i, d) in tests.iter().enumerate() {
            let msg = format!("test[{}]: {:?}", i, d);
            let string_args: Vec<String> =
                d.cli_args.clone().into_iter().map(String::from).collect();

            let mut args = Args::default();
            args.set(d.args.clone());

            let mut handler = ModifyHandler::default();
            let mut app = App::default().args(args).handler(Box::new(&mut handler));

            if d.no_strict_options {
                app = app.no_strict_options();
            }

            let result = app.parse_with_args(string_args);

            let msg = format!("{}, result: {:?}", msg, result);

            if result.is_err() {
                assert!(d.result.is_err(), "{}", msg);

                let expected_err = format!("{:?}", d.result.as_ref().err());
                let actual_err = format!("{:?}", result.as_ref().err());
                assert_eq!(expected_err, actual_err, "{}", msg);

                continue;
            }

            assert!(result.is_ok(), "{}", msg);

            drop(app);

            assert_eq!(d.r_count, handler.r_count);
            assert_eq!(d.d_count, handler.d_count);

            let v: Vec<String> = d.values.clone().into_iter().map(String::from).collect();
            assert_eq!(v, handler.v);
        }
    }
}
