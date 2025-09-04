use crate::prelude::*;

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    Ident(TokenValue),
    Literal(Literal),
    Punct(PunctKind),
}

#[derive(Debug, Clone)]
pub enum TokenValue {
    Owned(String),
    Borrowed(&'static str),
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub kind:  LiteralKind,
    pub value: TokenValue,
}

#[derive(Debug, Clone)]
pub enum LiteralKind {
    /// `123`
    /// `0x123`
    /// `0b101`
    /// `0o123`
    /// `123_456`
    /// `123u32`
    /// `123e10`
    Int,
    /// `123.456`
    /// `123.0e10`
    /// `123.0f32`
    Float,
    /// `"hello"`
    /// `r"hello"`
    /// `b"hello"`
    /// `br"hello"`
    String,
    /// `'c'`
    /// `b'c'`
    Char,
    /// `true` or `false`
    Bool,
}

#[derive(Debug, Clone)]
pub enum PunctKind {
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `%`
    Percent,
    /// `!`
    Bang,
    /// `&`
    Ampersand,
    /// `|`
    Pipe,
    /// `^`
    Caret,
    /// `~`
    Tilde,
    /// `=`
    Equal,
    /// `==`
    DoubleEqual,
    /// `!=`
    NotEqual,
    /// `<=`
    LessEqual,
    /// `>=`
    GreaterEqual,
    /// `<`
    Less,
    /// `>`
    Greater,
    /// `&&`
    DoubleAmpersand,
    /// `||`
    DoublePipe,
    /// `=>`
    FatArrow,
    /// `->`
    Arrow,
    /// `<-`
    ReverseArrow,
    /// `;`
    Semicolon,
    /// `,`
    Comma,
    /// `:`
    Colon,
    /// `::`
    DoubleColon,
    /// `(`
    ParenL,
    /// `)`
    ParenR,
    /// `{`
    BraceL,
    /// `}`
    BraceR,
    /// `[`
    BracketL,
    /// `]`
    BracketR,
    /// `?`
    Question,
    /// `@`
    At,
    /// `#`
    Hash,
    /// `.`
    Dot,
    /// `..`
    DotDot,
    /// `+=`
    PlusEqual,
    /// `-=`
    MinusEqual,
    /// `*=`
    StarEqual,
    /// `/=`
    SlashEqual,
    /// `%=`
    PercentEqual,
    /// `&=`
    AmpersandEqual,
    /// `|=`
    PipeEqual,
    /// `^=`
    CaretEqual,
    /// `<<`
    ShiftLeft,
    /// `>>`
    ShiftRight,
    /// `<<=`
    ShiftLeftEqual,
    /// `>>=`
    ShiftRightEqual,
    /// `$`
    Dollar,
    /// `..=`
    DotDotEqual,
    /// `_`
    Underscore,
}
