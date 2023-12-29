use std::io::Write;
use std::path::Path;

pub fn create_file_from_text(program_text: &str) -> &Path {
    let program_path = Path::new("target/tmp/test_file.faux");
    let mut program_file = std::fs::File::create(program_path).unwrap();
    write!(program_file, "{}", program_text).unwrap();
    program_path
}