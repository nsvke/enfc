pub mod program;

use std::{io::Write, process::Command};

use crate::program::m_arg_handler::Args;
use crate::program::m_program_error::ProgramResult as Result;
use enfc::driver::{Binary, Driver, SourceCode};

fn main() -> Result<()> {
    let start = std::time::Instant::now();

    let config = Args::parse_args().config()?;

    let mut driver = Driver::new(config);
    driver.run();

    let packet = driver.result();

    if packet.diagnostics.config.dump_tokens {
        packet.print_tokens();
    }
    if packet.diagnostics.config.dump_ast {
        packet.print_ast();
    }
    if packet.diagnostics.config.dump_tast {
        packet.print_tast();
    }
    if packet.diagnostics.config.dump_ir {
        packet.print_ir();
    }

    if packet.diagnostics.has_errors() {
        packet.diagnostics.print_errors();
        std::process::exit(1);
    }

    let code = match packet.output {
        Some(out) => match out {
            SourceCode(s) => s,
            Binary(_) => unreachable!(),
        },
        None => std::process::exit(1),
    };

    let out_file_name = packet.diagnostics.config.output_path;
    let mut file_name = out_file_name.clone();
    file_name.push_str(".c");

    let mut out_file = std::fs::File::create(file_name.clone())?;
    out_file.write(code.as_bytes())?;

    if packet.diagnostics.config.emit_c {
        println!("C source emitted successfully in {:?}", start.elapsed());
        return Ok(());
    }

    let output = Command::new("cc")
        .arg(&file_name)
        .arg("-o")
        .arg(&out_file_name)
        .output()
        .expect("fatal error: 'cc' compiler not found in system path!");

    if output.status.success() {
        let _ = std::fs::remove_file(&file_name);
        println!("compiled successfuly in {:?}", start.elapsed());
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        eprintln!(
            "An error occurred while compiling C code \n ---> \n{}",
            stderr
        );
        std::process::exit(1);
    }

    Ok(())
}
