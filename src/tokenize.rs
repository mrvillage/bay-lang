use crate::{prelude::*, store::add_file};

pub fn tokenize(file: &std::path::Path) -> Result<Vec<Token>> {
    if !file.exists() {
        panic!("File does not exist: {}", file.display());
    }
    let (file, input) = add_file(file)?;
    let mut tokens = Vec::with_capacity(input.len() / 2);
    let chars = input.chars().collect::<Vec<_>>();
    let mut i = 0;
    let mut line = 1;
    let mut col = 0;
    let mut indent = 0;
    let mut indenting = true;
    // if we're tokenizing a string, we want to find the string's indent and trim
    // that many spaces
    let mut tokenizing_str = false;
    let mut span = None;
    while i < input.len() {
        let c = chars[i];
        i += 1;
        col += 1;
    }
    tokens.shrink_to_fit();
    Ok(tokens)
}
