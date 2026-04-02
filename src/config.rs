#[derive(Debug)]
pub struct Config {
    pub source_path: String,
    pub source_code: String,
    pub output_path: String,
    pub emit_c: bool,
    pub dump_tokens: bool,
    pub dump_ast: bool,
    pub dump_tast: bool,
    pub dump_ir: bool,
}

impl Config {
    pub fn empty_config(s: String) -> Self {
        Self {
            source_path: "./empty_config_path_holder".into(),
            source_code: s,
            output_path: "./output".into(),
            emit_c: false,
            dump_tokens: false,
            dump_ast: false,
            dump_tast: false,
            dump_ir: false,
        }
    }
}
