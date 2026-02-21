use super::{m_fs::source_as_string, m_program_error::ProgramResult};
use clap::{Arg, ArgMatches, Command};
use enfc::config::Config;

pub struct Args {
    args: ArgMatches,
}

impl Args {
    pub fn parse_args() -> Args {
        Args {
            args: Command::new("enfc")
                .version("0.1.0")
                .author("enescevik")
                .about("enf compiler")
                .arg(Arg::new("source").short('s').long("source").required(true))
                .arg(Arg::new("output").short('o').long("output").required(false))
                .get_matches(),
        }
    }

    pub fn get_arg(&self, arg_name: &str) -> Option<String> {
        Some(self.args.get_one::<String>(arg_name)?.clone())
    }

    pub fn config(self) -> ProgramResult<Config> {
        let path = self.get_arg("source").unwrap_or(String::from("main.enf"));
        Ok(Config {
            source_code: source_as_string(&path)?,
            source_path: path,
            output_path: self.get_arg("output").unwrap_or(String::from("./")),
        })
    }
}
