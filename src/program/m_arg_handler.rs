use super::{m_fs::source_as_string, m_program_error::ProgramResult};
use clap::{Arg, ArgAction, ArgMatches, Command};
use enfc::config::Config;

pub struct Args {
    args: ArgMatches,
}

impl Args {
    pub fn parse_args() -> Args {
        Args {
            args: Command::new("enfc")
                .version("0.1.0")
                .author("Enes Çevik")
                .about("enf compiler")
                .arg(
                    Arg::new("source")
                        .required(true)
                        .index(1)
                        .help("The main .enf source file to compile"),
                )
                .arg(
                    Arg::new("output")
                        .short('o')
                        .long("output")
                        .required(false)
                        .help("Set the output binary name"),
                )
                .arg(
                    Arg::new("emit_c")
                        .long("emit-c")
                        .action(ArgAction::SetTrue)
                        .help("Only emit the C source file, do not compile to binary"),
                )
                .arg(
                    Arg::new("dump_tokens")
                        .long("dump-tokens")
                        .action(ArgAction::SetTrue),
                )
                .arg(
                    Arg::new("dump_ast")
                        .long("dump-ast")
                        .action(ArgAction::SetTrue),
                )
                .arg(
                    Arg::new("dump_tast")
                        .long("dump-tast")
                        .action(ArgAction::SetTrue),
                )
                .arg(
                    Arg::new("dump_ir")
                        .long("dump-ir")
                        .action(ArgAction::SetTrue),
                )
                .get_matches(),
        }
    }

    pub fn get_arg(&self, arg_name: &str) -> Option<String> {
        Some(self.args.get_one::<String>(arg_name)?.clone())
    }
    pub fn get_flag(&self, flag_name: &str) -> bool {
        self.args.get_flag(flag_name)
    }

    pub fn config(self) -> ProgramResult<Config> {
        let path = self.get_arg("source").unwrap_or(String::from("main.enf"));
        let default_output = if path.ends_with(".enf") {
            path.strip_suffix(".enf").unwrap().to_string()
        } else {
            format!("{}_out", path)
        };

        Ok(Config {
            source_code: source_as_string(&path)?,
            source_path: path,
            output_path: self.get_arg("output").unwrap_or(default_output),
            emit_c: self.get_flag("emit_c"),
            dump_tokens: self.get_flag("dump_tokens"),
            dump_ast: self.get_flag("dump_ast"),
            dump_tast: self.get_flag("dump_tast"),
            dump_ir: self.get_flag("dump_ir"),
        })
    }
}
