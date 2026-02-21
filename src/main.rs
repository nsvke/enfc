pub mod program;

use crate::program::m_arg_handler::Args;
use crate::program::m_program_error::ProgramResult as Result;
use enfc::driver::Driver;

// use std::sync::OnceLock;

fn main() -> Result<()> {
    let start = std::time::Instant::now();

    let config = Args::parse_args().config()?;
    let mut driver = Driver::new(config);
    driver.run();

    println!("--------------------------------");
    println!("exec time: {:?}", start.elapsed());
    println!("--------------------------------");

    Ok(())
}
