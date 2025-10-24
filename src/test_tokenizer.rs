use super::super::tokenizer::*;

#[test]
fn test_symbol() {
    let input = "+ - * / % ! & | ^ ~ = == != <= >= < > && || => -> <- ; : :: ( ) { } [ ] ? @ # . \
                 .. += -= *= /= %= &= |= ^= << >> <<= >>= $ ..= _";
    let tokens = tokenize_str(input, 0).unwrap();
    assert_eq!(tokens.len(), 51);
    assert_eq!(
        tokens[0],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Plus,
            span: Span {
                start:  0,
                end:    1,
                line:   1,
                col:    1,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[1],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Minus,
            span: Span {
                start:  2,
                end:    3,
                line:   1,
                col:    3,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[2],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Star,
            span: Span {
                start:  4,
                end:    5,
                line:   1,
                col:    5,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[3],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Slash,
            span: Span {
                start:  6,
                end:    7,
                line:   1,
                col:    7,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[4],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Percent,
            span: Span {
                start:  8,
                end:    9,
                line:   1,
                col:    9,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[5],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Bang,
            span: Span {
                start:  10,
                end:    11,
                line:   1,
                col:    11,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[6],
        Token::Symbol(TokenSymbol {
            kind: Symbol::And,
            span: Span {
                start:  12,
                end:    13,
                line:   1,
                col:    13,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[7],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Pipe,
            span: Span {
                start:  14,
                end:    15,
                line:   1,
                col:    15,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[8],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Caret,
            span: Span {
                start:  16,
                end:    17,
                line:   1,
                col:    17,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[9],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Tilde,
            span: Span {
                start:  18,
                end:    19,
                line:   1,
                col:    19,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[10],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Equal,
            span: Span {
                start:  20,
                end:    21,
                line:   1,
                col:    21,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[11],
        Token::Symbol(TokenSymbol {
            kind: Symbol::EqualEqual,
            span: Span {
                start:  22,
                end:    24,
                line:   1,
                col:    23,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[12],
        Token::Symbol(TokenSymbol {
            kind: Symbol::BangEqual,
            span: Span {
                start:  25,
                end:    27,
                line:   1,
                col:    26,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[13],
        Token::Symbol(TokenSymbol {
            kind: Symbol::LessEqual,
            span: Span {
                start:  28,
                end:    30,
                line:   1,
                col:    29,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[14],
        Token::Symbol(TokenSymbol {
            kind: Symbol::GreaterEqual,
            span: Span {
                start:  31,
                end:    33,
                line:   1,
                col:    32,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[15],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Less,
            span: Span {
                start:  34,
                end:    35,
                line:   1,
                col:    35,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[16],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Greater,
            span: Span {
                start:  36,
                end:    37,
                line:   1,
                col:    37,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[17],
        Token::Symbol(TokenSymbol {
            kind: Symbol::AndAnd,
            span: Span {
                start:  38,
                end:    40,
                line:   1,
                col:    39,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[18],
        Token::Symbol(TokenSymbol {
            kind: Symbol::PipePipe,
            span: Span {
                start:  41,
                end:    43,
                line:   1,
                col:    42,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[19],
        Token::Symbol(TokenSymbol {
            kind: Symbol::FatArrow,
            span: Span {
                start:  44,
                end:    46,
                line:   1,
                col:    45,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[20],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Arrow,
            span: Span {
                start:  47,
                end:    49,
                line:   1,
                col:    48,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[21],
        Token::Symbol(TokenSymbol {
            kind: Symbol::ReverseArrow,
            span: Span {
                start:  50,
                end:    52,
                line:   1,
                col:    51,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[22],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Semicolon,
            span: Span {
                start:  53,
                end:    54,
                line:   1,
                col:    54,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[23],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Colon,
            span: Span {
                start:  55,
                end:    56,
                line:   1,
                col:    56,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[24],
        Token::Symbol(TokenSymbol {
            kind: Symbol::ColonColon,
            span: Span {
                start:  57,
                end:    59,
                line:   1,
                col:    58,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[25],
        Token::Symbol(TokenSymbol {
            kind: Symbol::OpenParen,
            span: Span {
                start:  60,
                end:    61,
                line:   1,
                col:    61,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[26],
        Token::Symbol(TokenSymbol {
            kind: Symbol::CloseParen,
            span: Span {
                start:  62,
                end:    63,
                line:   1,
                col:    63,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[27],
        Token::Symbol(TokenSymbol {
            kind: Symbol::OpenBrace,
            span: Span {
                start:  64,
                end:    65,
                line:   1,
                col:    65,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[28],
        Token::Symbol(TokenSymbol {
            kind: Symbol::CloseBrace,
            span: Span {
                start:  66,
                end:    67,
                line:   1,
                col:    67,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[29],
        Token::Symbol(TokenSymbol {
            kind: Symbol::OpenBracket,
            span: Span {
                start:  68,
                end:    69,
                line:   1,
                col:    69,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[30],
        Token::Symbol(TokenSymbol {
            kind: Symbol::CloseBracket,
            span: Span {
                start:  70,
                end:    71,
                line:   1,
                col:    71,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[31],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Question,
            span: Span {
                start:  72,
                end:    73,
                line:   1,
                col:    73,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[32],
        Token::Symbol(TokenSymbol {
            kind: Symbol::At,
            span: Span {
                start:  74,
                end:    75,
                line:   1,
                col:    75,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[33],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Hash,
            span: Span {
                start:  76,
                end:    77,
                line:   1,
                col:    77,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[34],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Dot,
            span: Span {
                start:  78,
                end:    79,
                line:   1,
                col:    79,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[35],
        Token::Symbol(TokenSymbol {
            kind: Symbol::DotDot,
            span: Span {
                start:  80,
                end:    82,
                line:   1,
                col:    81,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[36],
        Token::Symbol(TokenSymbol {
            kind: Symbol::PlusEqual,
            span: Span {
                start:  83,
                end:    85,
                line:   1,
                col:    84,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[37],
        Token::Symbol(TokenSymbol {
            kind: Symbol::MinusEqual,
            span: Span {
                start:  86,
                end:    88,
                line:   1,
                col:    87,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[38],
        Token::Symbol(TokenSymbol {
            kind: Symbol::StarEqual,
            span: Span {
                start:  89,
                end:    91,
                line:   1,
                col:    90,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[39],
        Token::Symbol(TokenSymbol {
            kind: Symbol::SlashEqual,
            span: Span {
                start:  92,
                end:    94,
                line:   1,
                col:    93,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[40],
        Token::Symbol(TokenSymbol {
            kind: Symbol::PercentEqual,
            span: Span {
                start:  95,
                end:    97,
                line:   1,
                col:    96,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[41],
        Token::Symbol(TokenSymbol {
            kind: Symbol::AndEqual,
            span: Span {
                start:  98,
                end:    100,
                line:   1,
                col:    99,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[42],
        Token::Symbol(TokenSymbol {
            kind: Symbol::PipeEqual,
            span: Span {
                start:  101,
                end:    103,
                line:   1,
                col:    102,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[43],
        Token::Symbol(TokenSymbol {
            kind: Symbol::CaretEqual,
            span: Span {
                start:  104,
                end:    106,
                line:   1,
                col:    105,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[44],
        Token::Symbol(TokenSymbol {
            kind: Symbol::ShiftLeft,
            span: Span {
                start:  107,
                end:    109,
                line:   1,
                col:    108,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[45],
        Token::Symbol(TokenSymbol {
            kind: Symbol::ShiftRight,
            span: Span {
                start:  110,
                end:    112,
                line:   1,
                col:    111,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[46],
        Token::Symbol(TokenSymbol {
            kind: Symbol::ShiftLeftEqual,
            span: Span {
                start:  113,
                end:    116,
                line:   1,
                col:    114,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[47],
        Token::Symbol(TokenSymbol {
            kind: Symbol::ShiftRightEqual,
            span: Span {
                start:  117,
                end:    120,
                line:   1,
                col:    118,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[48],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Dollar,
            span: Span {
                start:  121,
                end:    122,
                line:   1,
                col:    122,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[49],
        Token::Symbol(TokenSymbol {
            kind: Symbol::DotDotEqual,
            span: Span {
                start:  123,
                end:    126,
                line:   1,
                col:    124,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[50],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Underscore,
            span: Span {
                start:  127,
                end:    128,
                line:   1,
                col:    128,
                indent: 0,
                file:   0,
            },
        })
    );
}

#[test]
fn test_ident() {
    let input = "hello world _foo bar_baz Baz123 _1";
    let tokens = tokenize_str(input, 0).unwrap();
    assert_eq!(tokens.len(), 6);
    assert_eq!(
        tokens[0],
        Token::Ident(Ident {
            value: "hello",
            span:  Span {
                start:  0,
                end:    5,
                line:   1,
                col:    1,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[1],
        Token::Ident(Ident {
            value: "world",
            span:  Span {
                start:  6,
                end:    11,
                line:   1,
                col:    7,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[2],
        Token::Ident(Ident {
            value: "_foo",
            span:  Span {
                start:  12,
                end:    16,
                line:   1,
                col:    13,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[3],
        Token::Ident(Ident {
            value: "bar_baz",
            span:  Span {
                start:  17,
                end:    24,
                line:   1,
                col:    18,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[4],
        Token::Ident(Ident {
            value: "Baz123",
            span:  Span {
                start:  25,
                end:    31,
                line:   1,
                col:    26,
                indent: 0,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[5],
        Token::Ident(Ident {
            value: "_1",
            span:  Span {
                start:  32,
                end:    34,
                line:   1,
                col:    33,
                indent: 0,
                file:   0,
            },
        })
    );
}

#[test]
fn test_literal() {
    let input = r#"
        123 0x7B 0b1111011 0o173 123_456 123u32
        123.456 123e10 123.0e10 123.0f32
        "hello"
        'c'
        true false
        nil
        "#;
    let tokens = tokenize_str(input, 0).unwrap();
    assert_eq!(tokens.len(), 15);
    assert_eq!(
        tokens[0],
        Token::Literal(TokenLiteral::Int(IntLiteral {
            value:  "123",
            radix:  Radix::Decimal,
            suffix: None,
            span:   Span {
                start:  9,
                end:    12,
                line:   2,
                col:    9,
                indent: 8,
                file:   0,
            },
        }))
    );
    assert_eq!(
        tokens[1],
        Token::Literal(TokenLiteral::Int(IntLiteral {
            value:  "0x7B",
            radix:  Radix::Hex,
            suffix: None,
            span:   Span {
                start:  13,
                end:    17,
                line:   2,
                col:    13,
                indent: 8,
                file:   0,
            },
        }))
    );
    assert_eq!(
        tokens[2],
        Token::Literal(TokenLiteral::Int(IntLiteral {
            value:  "0b1111011",
            radix:  Radix::Binary,
            suffix: None,
            span:   Span {
                start:  18,
                end:    27,
                line:   2,
                col:    18,
                indent: 8,
                file:   0,
            },
        }))
    );
    assert_eq!(
        tokens[3],
        Token::Literal(TokenLiteral::Int(IntLiteral {
            value:  "0o173",
            radix:  Radix::Octal,
            suffix: None,
            span:   Span {
                start:  28,
                end:    33,
                line:   2,
                col:    28,
                indent: 8,
                file:   0,
            },
        }))
    );
    assert_eq!(
        tokens[4],
        Token::Literal(TokenLiteral::Int(IntLiteral {
            value:  "123_456",
            radix:  Radix::Decimal,
            suffix: None,
            span:   Span {
                start:  34,
                end:    41,
                line:   2,
                col:    34,
                indent: 8,
                file:   0,
            },
        }))
    );
    assert_eq!(
        tokens[5],
        Token::Literal(TokenLiteral::Int(IntLiteral {
            value:  "123",
            radix:  Radix::Decimal,
            suffix: Some("u32"),
            span:   Span {
                start:  42,
                end:    48,
                line:   2,
                col:    42,
                indent: 8,
                file:   0,
            },
        }))
    );
    assert_eq!(
        tokens[6],
        Token::Literal(TokenLiteral::Float(FloatLiteral {
            value:  "123.456",
            suffix: None,
            span:   Span {
                start:  57,
                end:    64,
                line:   3,
                col:    9,
                indent: 8,
                file:   0,
            },
        }))
    );
    assert_eq!(
        tokens[7],
        Token::Literal(TokenLiteral::Float(FloatLiteral {
            value:  "123e10",
            suffix: None,
            span:   Span {
                start:  65,
                end:    71,
                line:   3,
                col:    17,
                indent: 8,
                file:   0,
            },
        }))
    );
    assert_eq!(
        tokens[8],
        Token::Literal(TokenLiteral::Float(FloatLiteral {
            value:  "123.0e10",
            suffix: None,
            span:   Span {
                start:  72,
                end:    80,
                line:   3,
                col:    24,
                indent: 8,
                file:   0,
            },
        }))
    );
    assert_eq!(
        tokens[9],
        Token::Literal(TokenLiteral::Float(FloatLiteral {
            value:  "123.0",
            suffix: Some("f32"),
            span:   Span {
                start:  81,
                end:    89,
                line:   3,
                col:    33,
                indent: 8,
                file:   0,
            },
        }))
    );
    assert_eq!(
        tokens[10],
        Token::Literal(TokenLiteral::String(StringLiteral {
            value: "hello".to_string(),
            span:  Span {
                start:  98,
                end:    105,
                line:   4,
                col:    9,
                indent: 8,
                file:   0,
            },
        }))
    );
    assert_eq!(
        tokens[11],
        Token::Literal(TokenLiteral::Char(CharLiteral {
            value: 'c',
            span:  Span {
                start:  114,
                end:    117,
                line:   5,
                col:    9,
                indent: 8,
                file:   0,
            },
        }))
    );
    assert_eq!(
        tokens[12],
        Token::Literal(TokenLiteral::Bool(BoolLiteral {
            value: true,
            span:  Span {
                start:  126,
                end:    130,
                line:   6,
                col:    9,
                indent: 8,
                file:   0,
            },
        }))
    );
    assert_eq!(
        tokens[13],
        Token::Literal(TokenLiteral::Bool(BoolLiteral {
            value: false,
            span:  Span {
                start:  131,
                end:    136,
                line:   6,
                col:    14,
                indent: 8,
                file:   0,
            },
        }))
    );
    assert_eq!(
        tokens[14],
        Token::Literal(TokenLiteral::Nil(NilLiteral {
            span: Span {
                start:  145,
                end:    148,
                line:   7,
                col:    9,
                indent: 8,
                file:   0,
            },
        }))
    );
}

#[test]
fn test_comments() {
    let input = r#"
        // This is a single-line comment
        x = 42; /* This is a block comment */
        /*
        This is a
        /* nested */
        multi-line block comment
        */
        y = x + 1; // Another single-line comment
        "#;
    let tokens = tokenize_str(input, 0).unwrap();
    assert_eq!(tokens.len(), 10);
    assert_eq!(
        tokens[0],
        Token::Ident(Ident {
            value: "x",
            span:  Span {
                start:  50,
                end:    51,
                line:   3,
                col:    9,
                indent: 8,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[1],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Equal,
            span: Span {
                start:  52,
                end:    53,
                line:   3,
                col:    11,
                indent: 8,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[2],
        Token::Literal(TokenLiteral::Int(IntLiteral {
            value:  "42",
            radix:  Radix::Decimal,
            suffix: None,
            span:   Span {
                start:  54,
                end:    56,
                line:   3,
                col:    13,
                indent: 8,
                file:   0,
            },
        }))
    );
    assert_eq!(
        tokens[3],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Semicolon,
            span: Span {
                start:  56,
                end:    57,
                line:   3,
                col:    15,
                indent: 8,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[4],
        Token::Ident(Ident {
            value: "y",
            span:  Span {
                start:  190,
                end:    191,
                line:   9,
                col:    9,
                indent: 8,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[5],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Equal,
            span: Span {
                start:  192,
                end:    193,
                line:   9,
                col:    11,
                indent: 8,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[6],
        Token::Ident(Ident {
            value: "x",
            span:  Span {
                start:  194,
                end:    195,
                line:   9,
                col:    13,
                indent: 8,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[7],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Plus,
            span: Span {
                start:  196,
                end:    197,
                line:   9,
                col:    15,
                indent: 8,
                file:   0,
            },
        })
    );
    assert_eq!(
        tokens[8],
        Token::Literal(TokenLiteral::Int(IntLiteral {
            value:  "1",
            radix:  Radix::Decimal,
            suffix: None,
            span:   Span {
                start:  198,
                end:    199,
                line:   9,
                col:    17,
                indent: 8,
                file:   0,
            },
        }))
    );
    assert_eq!(
        tokens[9],
        Token::Symbol(TokenSymbol {
            kind: Symbol::Semicolon,
            span: Span {
                start:  199,
                end:    200,
                line:   9,
                col:    18,
                indent: 8,
                file:   0,
            },
        })
    );
}

#[test]
fn test_keywords() {
    let input = "fn let mut if else while for loop in return break continue struct enum impl mod \
                 use pub const static as match trait type where self Self super dyn ref box move \
                 async await try macro extern unsafe virtual override final abstract sealed event \
                 emit from new pkg";
    let tokens = tokenize_str(input, 0).unwrap();
    assert_eq!(tokens.len(), 48);
    let keywords = [
        ("fn", Keyword::Fn),
        ("let", Keyword::Let),
        ("mut", Keyword::Mut),
        ("if", Keyword::If),
        ("else", Keyword::Else),
        ("while", Keyword::While),
        ("for", Keyword::For),
        ("loop", Keyword::Loop),
        ("in", Keyword::In),
        ("return", Keyword::Return),
        ("break", Keyword::Break),
        ("continue", Keyword::Continue),
        ("struct", Keyword::Struct),
        ("enum", Keyword::Enum),
        ("impl", Keyword::Impl),
        ("mod", Keyword::Mod),
        ("use", Keyword::Use),
        ("pub", Keyword::Pub),
        ("const", Keyword::Const),
        ("static", Keyword::Static),
        ("as", Keyword::As),
        ("match", Keyword::Match),
        ("trait", Keyword::Trait),
        ("type", Keyword::Type),
        ("where", Keyword::Where),
        ("self", Keyword::SelfLower),
        ("Self", Keyword::SelfUpper),
        ("super", Keyword::Super),
        ("dyn", Keyword::Dyn),
        ("ref", Keyword::Ref),
        ("box", Keyword::Box),
        ("move", Keyword::Move),
        ("async", Keyword::Async),
        ("await", Keyword::Await),
        ("try", Keyword::Try),
        ("macro", Keyword::Macro),
        ("extern", Keyword::Extern),
        ("unsafe", Keyword::Unsafe),
        ("virtual", Keyword::Virtual),
        ("override", Keyword::Override),
        ("final", Keyword::Final),
        ("abstract", Keyword::Abstract),
        ("sealed", Keyword::Sealed),
        ("event", Keyword::Event),
        ("emit", Keyword::Emit),
        ("from", Keyword::From),
        ("new", Keyword::New),
        ("pkg", Keyword::Pkg),
    ];
    for (i, (kw_str, kw_kind)) in keywords.iter().enumerate() {
        let prev_lens = keywords[..i]
            .iter()
            .map(|(s, _)| s.len() + 1)
            .sum::<usize>();
        assert_eq!(
            tokens[i],
            Token::Keyword(TokenKeyword {
                kind: *kw_kind,
                span: Span {
                    start:  prev_lens,
                    end:    prev_lens + kw_str.len(),
                    line:   1,
                    col:    1 + prev_lens,
                    indent: 0,
                    file:   0,
                },
            })
        );
    }
}
