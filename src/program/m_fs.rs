use super::m_program_error::ProgramResult;
use std::fs::File;
use std::io::Read;
// use std::path::Path;

// pub fn full_path(path_arg: &String) -> ProgramResult<String> {
//     match canonicalize(Path::new(path_arg)) {
//         Ok(abs_path) => Ok(String::from(abs_path.to_str().unwrap())),
//         Err(e) => Err(Error::Io(e)),
//     }
// }

pub fn source_as_string(source_path: &String) -> ProgramResult<String> {
    let mut source_file = File::open(source_path)?;
    let mut source = String::new();
    source_file.read_to_string(&mut source)?;
    Ok(source)
}
