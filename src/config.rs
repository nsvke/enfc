#[derive(Debug)]
pub struct Config {
    pub source_path: String,
    pub source_code: String,
    pub output_path: String,
}

impl Config {
    pub fn empty_config(s: String) -> Self {
        Self {
            source_path: "./empty_config_path_holder".into(),
            source_code: s,
            output_path: "./".into(),
        }
    }
}
