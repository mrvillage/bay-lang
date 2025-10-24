use token::*;

use crate::{prelude::*, store::add_file};

const SYMBOL_FIRST: [char; 27] = [
    '+', '-', '*', '/', '%', '!', '&', '|', '^', '~', '=', '<', '>', ';', ':', ',', '.', '(', ')',
    '{', '}', '[', ']', '?', '@', '#', '$',
];

pub fn tokenize(file: &std::path::Path) -> Result<Vec<Token>> {
    if !file.exists() {
        panic!("File does not exist: {}", file.display());
    }
    let (file, input) = add_file(file)?;
    tokenize_str(input, file)
}

pub fn tokenize_str(input: &'static str, file: u64) -> Result<Vec<Token>> {
    let mut tokens = Vec::with_capacity(input.len() / 2);
    let chars = input.chars().collect::<Vec<_>>();
    let mut i = 0;
    let mut line = 1;
    let mut col = 1;
    let mut indent = 0;
    let mut indenting = true;
    'next_token: while i < chars.len() {
        let c = chars[i];
        if c == '\n' {
            line += 1;
            col = 1;
            indenting = true;
            indent = 0;
            i += 1;
            continue;
        }
        if c.is_whitespace() {
            if indenting {
                indent += 1;
            }
            i += 1;
            col += 1;
            continue;
        }
        indenting = false;
        // first, we skip comments
        if c == '/' && chars.get(i + 1) == Some(&'/') {
            i += 2;
            col += 2;
            while i < chars.len() && chars[i] != '\n' {
                i += 1;
                col += 1;
            }
            continue;
        } else if c == '/' && chars.get(i + 1) == Some(&'*') {
            i += 2;
            col += 2;
            let mut depth = 1;
            while i < chars.len() {
                if chars[i] == '\n' {
                    line += 1;
                    col = 1;
                    i += 1;
                    continue;
                } else if chars[i] == '*' && chars.get(i + 1) == Some(&'/') {
                    depth -= 1;
                    i += 2;
                    col += 2;
                    if depth == 0 {
                        break;
                    }
                    continue;
                } else if chars[i] == '/' && chars.get(i + 1) == Some(&'*') {
                    depth += 1;
                    i += 2;
                    col += 2;
                    continue;
                }
                i += 1;
                col += 1;
            }
            if depth != 0 {
                return Err(CompileError::UnterminatedComment(Span {
                    start: i,
                    end: i + 1,
                    line,
                    col,
                    indent,
                    file,
                }));
            }
            continue;
        }
        // we've found an actual token to parse!
        // first, we see if we've found a symbol
        // we special case _, if it is followed by a letter or number, then it's an
        // ident
        if SYMBOL_FIRST.contains(&c)
            || (c == '_'
                && !chars
                    .get(i + 1)
                    .map(|x| x.is_ascii_alphanumeric())
                    .unwrap_or(false))
        {
            let (sym, end) = match (c, chars.get(i + 1), chars.get(i + 2)) {
                ('+', Some('='), _) => (Symbol::PlusEqual, i + 1),
                ('+', ..) => (Symbol::Plus, i),
                ('-', Some('='), _) => (Symbol::MinusEqual, i + 1),
                ('-', Some('>'), _) => (Symbol::Arrow, i + 1),
                ('-', ..) => (Symbol::Minus, i),
                ('*', Some('='), _) => (Symbol::StarEqual, i + 1),
                ('*', ..) => (Symbol::Star, i),
                ('/', Some('='), _) => (Symbol::SlashEqual, i + 1),
                ('/', ..) => (Symbol::Slash, i),
                ('%', Some('='), _) => (Symbol::PercentEqual, i + 1),
                ('%', ..) => (Symbol::Percent, i),
                ('!', Some('='), _) => (Symbol::BangEqual, i + 1),
                ('!', ..) => (Symbol::Bang, i),
                ('&', Some('&'), _) => (Symbol::AndAnd, i + 1),
                ('&', Some('='), _) => (Symbol::AndEqual, i + 1),
                ('&', ..) => (Symbol::And, i),
                ('|', Some('|'), _) => (Symbol::PipePipe, i + 1),
                ('|', Some('='), _) => (Symbol::PipeEqual, i + 1),
                ('|', ..) => (Symbol::Pipe, i),
                ('^', Some('='), _) => (Symbol::CaretEqual, i + 1),
                ('^', ..) => (Symbol::Caret, i),
                ('~', ..) => (Symbol::Tilde, i),
                ('=', Some('='), _) => (Symbol::EqualEqual, i + 1),
                ('=', Some('>'), _) => (Symbol::FatArrow, i + 1),
                ('=', ..) => (Symbol::Equal, i),
                ('<', Some('<'), Some('=')) => (Symbol::ShiftLeftEqual, i + 2),
                ('<', Some('<'), _) => (Symbol::ShiftLeft, i + 1),
                ('<', Some('='), _) => (Symbol::LessEqual, i + 1),
                ('<', Some('-'), _) => (Symbol::ReverseArrow, i + 1),
                ('<', ..) => (Symbol::Less, i),
                ('>', Some('>'), Some('=')) => (Symbol::ShiftRightEqual, i + 2),
                ('>', Some('>'), _) => (Symbol::ShiftRight, i + 1),
                ('>', Some('='), _) => (Symbol::GreaterEqual, i + 1),
                ('>', ..) => (Symbol::Greater, i),
                (';', ..) => (Symbol::Semicolon, i),
                (':', Some(':'), _) => (Symbol::ColonColon, i + 1),
                (':', ..) => (Symbol::Colon, i),
                (',', ..) => (Symbol::Comma, i),
                ('.', Some('.'), Some('=')) => (Symbol::DotDotEqual, i + 2),
                ('.', Some('.'), _) => (Symbol::DotDot, i + 1),
                ('.', ..) => (Symbol::Dot, i),
                ('(', ..) => (Symbol::OpenParen, i),
                (')', ..) => (Symbol::CloseParen, i),
                ('{', ..) => (Symbol::OpenBrace, i),
                ('}', ..) => (Symbol::CloseBrace, i),
                ('[', ..) => (Symbol::OpenBracket, i),
                (']', ..) => (Symbol::CloseBracket, i),
                ('?', ..) => (Symbol::Question, i),
                ('@', ..) => (Symbol::At, i),
                ('#', ..) => (Symbol::Hash, i),
                ('$', ..) => (Symbol::Dollar, i),
                ('_', ..) => (Symbol::Underscore, i),
                _ => unreachable!(),
            };
            tokens.push(Token::Symbol(TokenSymbol {
                kind: sym,
                span: Span {
                    start: i,
                    end: end + 1,
                    line,
                    col,
                    indent,
                    file,
                },
            }));
            col += end + 1 - i;
            i = end + 1;
        // next, we see if we've found a string
        } else if c == '"' {
            let start = i;
            let mut escape = false;
            let mut str = String::with_capacity(chars.len() - i);
            i += 1;
            while i < chars.len() {
                let c = chars[i];
                if c == '\\' {
                    escape = true;
                } else if escape {
                    match c {
                        'n' => str.push('\n'),
                        'r' => str.push('\r'),
                        't' => str.push('\t'),
                        '\\' => str.push('\\'),
                        '"' => str.push('"'),
                        other => {
                            return Err(CompileError::UnexpectedCharacter(
                                Span {
                                    start: i,
                                    end: i + 1,
                                    line,
                                    col,
                                    indent,
                                    file,
                                },
                                other,
                            ));
                        },
                    }
                    escape = false;
                } else if c == '"' {
                    i += 1;
                    str.shrink_to_fit();
                    tokens.push(Token::Literal(TokenLiteral::String(StringLiteral {
                        value: str,
                        span:  Span {
                            start,
                            end: i,
                            line,
                            col,
                            indent,
                            file,
                        },
                    })));
                    col += i - start;
                    continue 'next_token;
                } else {
                    str.push(c);
                }
                i += 1;
            }
            col += i - start;
            return Err(CompileError::UnexpectedEndOfFile(
                Span {
                    start: i,
                    end: i + 1,
                    line,
                    col,
                    indent,
                    file,
                },
                '"',
            ));
        // next, we see if we've found a char
        } else if c == '\'' {
            let start = i;
            i += 1;
            let mut char = None;
            match chars.get(i) {
                Some('\\') => {
                    i += 1;
                    match chars.get(i) {
                        Some('n') => {
                            char = Some('\n');
                            i += 1;
                        },
                        Some('r') => {
                            char = Some('\r');
                            i += 1;
                        },
                        Some('t') => {
                            char = Some('\t');
                            i += 1;
                        },
                        Some('\'') => {
                            char = Some('\'');
                            i += 1;
                        },
                        Some('\\') => {
                            char = Some('\\');
                            i += 1;
                        },
                        Some(other) => {
                            return Err(CompileError::UnexpectedCharacter(
                                Span {
                                    start: i,
                                    end: i + 1,
                                    line,
                                    col: col + 2,
                                    indent,
                                    file,
                                },
                                *other,
                            ));
                        },
                        _ => {},
                    }
                },
                Some('\'') => {
                    return Err(CompileError::UnexpectedCharacter(
                        Span {
                            start: i,
                            end: i + 1,
                            line,
                            col: col + 2,
                            indent,
                            file,
                        },
                        '\'',
                    ));
                },
                Some(c) => {
                    char = Some(*c);
                    i += 1;
                },
                _ => {},
            }
            if let Some(c) = char {
                if chars.get(i) == Some(&'\'') {
                    i += 1;
                    tokens.push(Token::Literal(TokenLiteral::Char(CharLiteral {
                        value: c,
                        span:  Span {
                            start,
                            end: i,
                            line,
                            col,
                            indent,
                            file,
                        },
                    })));
                    col += i - start;
                    continue;
                }
            }
            return Err(CompileError::UnexpectedEndOfFile(
                Span {
                    start: i,
                    end: i + 1,
                    line,
                    col,
                    indent,
                    file,
                },
                '\'',
            ));
        // next, we see if we have a number (int or float)
        } else if c.is_ascii_digit() {
            let start = i;
            let format = if c == '0' {
                match chars.get(i + 1) {
                    Some('x') | Some('X') => {
                        i += 2;
                        Radix::Hex
                    },
                    Some('b') | Some('B') => {
                        i += 2;
                        Radix::Binary
                    },
                    Some('o') | Some('O') => {
                        i += 2;
                        Radix::Octal
                    },
                    _ => {
                        // a decimal of some kind
                        Radix::Decimal
                    },
                }
            } else {
                Radix::Decimal
            };
            while i < chars.len() && (format.is_digit(chars[i]) || chars[i] == '_') {
                i += 1;
            }
            // now we have the integer part, we need to see if it is an integer,
            // an integer with a suffix, or a float
            // first, we check if it just has a suffix
            let mut suffix = None;
            let mut is_float = false;
            if let (Radix::Decimal, Some('.')) = (format, chars.get(i)) {
                if let Some(c) = chars.get(i + 1) {
                    if c.is_ascii_digit() {
                        is_float = true;
                        i += 1;
                        while i < chars.len() && (chars[i].is_ascii_digit() || chars[i] == '_') {
                            i += 1;
                        }
                    } else {
                        // if there's no digit after the '.', then we just
                        // have an int followed by a range operator or
                        // method call, or just something that can't be
                        // parsed
                    }
                } else {
                    return Err(CompileError::UnexpectedEndOfFile(
                        Span {
                            start: i + 1,
                            end: i + 2,
                            line,
                            col: col + (i - start) + 1,
                            indent,
                            file,
                        },
                        '\0',
                    ));
                }
            }
            if let (Radix::Decimal, Some('e')) = (format, chars.get(i)) {
                is_float = true;
                i += 1;
                if let Some(c) = chars.get(i) {
                    if *c == '+' || *c == '-' {
                        i += 1;
                    }
                }
                if let Some(c) = chars.get(i) {
                    if c.is_ascii_digit() {
                        while i < chars.len() && chars[i].is_ascii_digit() {
                            i += 1;
                        }
                    } else {
                        return Err(CompileError::UnexpectedCharacter(
                            Span {
                                start: i,
                                end: i + 1,
                                line,
                                col: col + (i - start) + 1,
                                indent,
                                file,
                            },
                            *c,
                        ));
                    }
                } else {
                    return Err(CompileError::UnexpectedEndOfFile(
                        Span {
                            start: i,
                            end: i + 1,
                            line,
                            col: col + (i - start) + 1,
                            indent,
                            file,
                        },
                        '\0',
                    ));
                }
            }
            let end = i;
            if let Some(c) = chars.get(i) {
                if *c == 'u' || *c == 'i' || *c == 'f' {
                    let suffix_start = i;
                    i += 1;
                    if let Some(cc) = chars.get(i) {
                        if *cc == '8' && *c != 'f' {
                            i += 1;
                            suffix = Some(&input[suffix_start..i]);
                        } else if *cc == '1' {
                            i += 1;
                            if let Some(c) = chars.get(i) {
                                if *c == '6' {
                                    i += 1;
                                    suffix = Some(&input[suffix_start..i]);
                                } else if *c == '2' {
                                    i += 1;
                                    if let Some(c) = chars.get(i) {
                                        if *c == '8' {
                                            i += 1;
                                            suffix = Some(&input[suffix_start..i]);
                                        } else {
                                            return Err(CompileError::UnexpectedCharacter(
                                                Span {
                                                    start: i,
                                                    end: i + 1,
                                                    line,
                                                    col: col + (i - start),
                                                    indent,
                                                    file,
                                                },
                                                *c,
                                            ));
                                        }
                                    } else {
                                        return Err(CompileError::UnexpectedEndOfFile(
                                            Span {
                                                start: i,
                                                end: i + 1,
                                                line,
                                                col: col + (i - start) + 1,
                                                indent,
                                                file,
                                            },
                                            '\0',
                                        ));
                                    }
                                } else {
                                    return Err(CompileError::UnexpectedCharacter(
                                        Span {
                                            start: i,
                                            end: i + 1,
                                            line,
                                            col: col + (i - start),
                                            indent,
                                            file,
                                        },
                                        *c,
                                    ));
                                }
                            } else {
                                return Err(CompileError::UnexpectedEndOfFile(
                                    Span {
                                        start: i,
                                        end: i + 1,
                                        line,
                                        col: col + (i - start) + 1,
                                        indent,
                                        file,
                                    },
                                    '0',
                                ));
                            }
                        } else if *cc == '3' {
                            i += 1;
                            if let Some(c) = chars.get(i) {
                                if *c == '2' {
                                    i += 1;
                                    suffix = Some(&input[suffix_start..i]);
                                } else {
                                    return Err(CompileError::UnexpectedCharacter(
                                        Span {
                                            start: i,
                                            end: i + 1,
                                            line,
                                            col: col + (i - start),
                                            indent,
                                            file,
                                        },
                                        *c,
                                    ));
                                }
                            } else {
                                return Err(CompileError::UnexpectedEndOfFile(
                                    Span {
                                        start: i,
                                        end: i + 1,
                                        line,
                                        col: col + (i - start),
                                        indent,
                                        file,
                                    },
                                    '0',
                                ));
                            }
                        } else if *cc == '6' {
                            i += 1;
                            if let Some(c) = chars.get(i) {
                                if *c == '4' {
                                    i += 1;
                                    suffix = Some(&input[suffix_start..i]);
                                } else {
                                    return Err(CompileError::UnexpectedCharacter(
                                        Span {
                                            start: i,
                                            end: i + 1,
                                            line,
                                            col: col + (i - start),
                                            indent,
                                            file,
                                        },
                                        *c,
                                    ));
                                }
                            } else {
                                return Err(CompileError::UnexpectedEndOfFile(
                                    Span {
                                        start: i,
                                        end: i + 1,
                                        line,
                                        col: col + (i - start) + 1,
                                        indent,
                                        file,
                                    },
                                    '0',
                                ));
                            }
                        } else {
                            return Err(CompileError::UnexpectedCharacter(
                                Span {
                                    start: i,
                                    end: i + 1,
                                    line,
                                    col: col + (i - start),
                                    indent,
                                    file,
                                },
                                *cc,
                            ));
                        }
                    } else {
                        return Err(CompileError::UnexpectedEndOfFile(
                            Span {
                                start: i,
                                end: i + 1,
                                line,
                                col: col + (i - start) + 1,
                                indent,
                                file,
                            },
                            '0',
                        ));
                    }
                }
            }
            if i < chars.len() && chars[i].is_ascii_alphabetic() {
                return Err(CompileError::UnexpectedCharacter(
                    Span {
                        start: i,
                        end: i + 1,
                        line,
                        col: col + (i - start),
                        indent,
                        file,
                    },
                    chars[i],
                ));
            }
            let span = Span {
                start,
                end: end + suffix.map_or(0, |s| s.len()),
                line,
                col,
                indent,
                file,
            };
            tokens.push(if is_float {
                Token::Literal(TokenLiteral::Float(FloatLiteral {
                    value: &input[start..end],
                    suffix,
                    span,
                }))
            } else {
                Token::Literal(TokenLiteral::Int(IntLiteral {
                    value: &input[start..end],
                    radix: format,
                    suffix,
                    span,
                }))
            });
            // tokens.push(Token {
            //     kind: TokenKind::Literal(TokenLiteral {
            //         kind: if is_float {
            //             TokenLiteral::Float
            //         } else {
            //             TokenLiteral::Int
            //         },
            //         value: LiteralValue::Borrowed(&input[start..end]),
            //         suffix,
            //     }),
            // });
            col += end - start + suffix.map_or(0, |s| s.len());
        // finally, we must have an ident
        } else if c.is_ascii_alphabetic() || c == '_' {
            let start = i;
            i += 1;
            while i < chars.len() && (chars[i].is_ascii_alphanumeric() || chars[i] == '_') {
                i += 1;
            }
            let end = i;
            let ident = &input[start..end];
            let span = Span {
                start,
                end,
                line,
                col,
                indent,
                file,
            };
            tokens.push(match ident {
                "true" | "false" => {
                    Token::Literal(TokenLiteral::Bool(BoolLiteral {
                        value: ident == "true",
                        span,
                    }))
                },
                "nil" => Token::Literal(TokenLiteral::Nil(NilLiteral { span })),
                _ => {
                    if let Some(keyword) = TokenKeyword::from_str(ident, span) {
                        Token::Keyword(keyword)
                    } else {
                        Token::Ident(Ident { value: ident, span })
                    }
                },
            });
            col += end - start;
        } else {
            return Err(CompileError::UnexpectedCharacter(
                Span {
                    start: i,
                    end: i + 1,
                    line,
                    col,
                    indent,
                    file,
                },
                c,
            ));
        }
    }
    tokens.shrink_to_fit();
    Ok(tokens)
}

#[cfg(test)]
#[path = "test_tokenizer.rs"]
mod tests;
