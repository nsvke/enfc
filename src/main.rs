pub mod program;

use std::{io::Write, process::Command};

use crate::program::m_arg_handler::Args;
use crate::program::m_program_error::ProgramResult as Result;
use enfc::driver::{Binary, Driver, SourceCode};

// use std::sync::OnceLock;

fn main() -> Result<()> {
    let start = std::time::Instant::now();

    let config = Args::parse_args().config()?;
    let mut driver = Driver::new(config);
    driver.run();

    let packet = driver.result();

    let code = match packet.output {
        Some(out) => match out {
            SourceCode(s) => s,
            Binary(_) => unreachable!(),
        },
        None => return Ok(()), // TODO add error
    };

    let out_file_name = packet.diagnostics.config.output_path;
    let mut file_name = out_file_name.clone();
    file_name.push_str(".c");
    let mut out_file = std::fs::File::create(file_name.clone())?;
    out_file.write(code.as_bytes())?;

    let output = Command::new("cc")
        .arg(file_name)
        .arg("-o")
        .arg(out_file_name)
        .output()
        .expect("cc not found!");

    if output.status.success() {
        println!("compiled successfuly");
    } else {
        eprintln!("an error occured while compiling")
    }

    println!("--------------------------------");
    println!("exec time: {:?}", start.elapsed());
    println!("--------------------------------");

    Ok(())
}
