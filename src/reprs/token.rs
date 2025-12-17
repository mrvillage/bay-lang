use crate::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Ident(Ident),
    Literal(TokenLiteral),
    Symbol(TokenSymbol),
    Keyword(TokenKeyword),
}

impl Token {
    pub fn span(&self) -> Span {
        match self {
            Token::Ident(ident) => ident.span,
            Token::Literal(lit) => {
                match lit {
                    TokenLiteral::Int(int) => int.span,
                    TokenLiteral::Float(float) => float.span,
                    TokenLiteral::String(string) => string.span,
                    TokenLiteral::Char(char) => char.span,
                    TokenLiteral::Bool(bool) => bool.span,
                    TokenLiteral::Nil(nil) => nil.span,
                }
            },
            Token::Symbol(sym) => sym.span,
            Token::Keyword(kw) => kw.span,
        }
    }

    pub fn unexpected_token<T>(&self, expected: Option<impl ToString>) -> Result<T> {
        Err(CompileError::UnexpectedToken(
            self.span(),
            expected.map(|s| s.to_string()),
        ))
    }

    pub fn consume_ident(&self) -> Result<&Ident> {
        if let Token::Ident(ident) = self {
            return Ok(ident);
        }
        Err(CompileError::UnexpectedToken(
            self.span(),
            Some("identifier".to_string()),
        ))
    }

    pub fn consume_symbol(&self, kind: Symbol) -> Result<&TokenSymbol> {
        if let Token::Symbol(sym) = self {
            if sym.kind == kind {
                return Ok(sym);
            }
            return Err(CompileError::UnexpectedToken(
                self.span(),
                Some(format!("'{}'", kind.as_str())),
            ));
        }
        Err(CompileError::UnexpectedToken(
            self.span(),
            Some(format!("'{}'", kind.as_str())),
        ))
    }

    pub fn consume_keyword(&self, kind: Keyword) -> Result<&TokenKeyword> {
        if let Token::Keyword(kw) = self {
            if kw.kind == kind {
                return Ok(kw);
            }
            return Err(CompileError::UnexpectedToken(
                self.span(),
                Some(format!("'{}'", kind.as_str())),
            ));
        }
        Err(CompileError::UnexpectedToken(
            self.span(),
            Some(format!("'{}'", kind.as_str())),
        ))
    }

    pub fn consume_literal(&self) -> Result<&TokenLiteral> {
        if let Token::Literal(lit) = self {
            return Ok(lit);
        }
        Err(CompileError::UnexpectedToken(
            self.span(),
            Some("literal".to_string()),
        ))
    }

    pub fn consume_int_literal(&self) -> Result<&IntLiteral> {
        if let Token::Literal(TokenLiteral::Int(int)) = self {
            return Ok(int);
        }
        Err(CompileError::UnexpectedToken(
            self.span(),
            Some("integer literal".to_string()),
        ))
    }
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub value: &'static str,
    pub span:  Span,
}

impl std::hash::Hash for Ident {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl Eq for Ident {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenSymbol {
    pub kind: Symbol,
    pub span: Span,
}

impl TokenSymbol {
    pub fn unexpected_token<T>(&self, expected: Option<impl ToString>) -> Result<T> {
        Err(CompileError::UnexpectedToken(
            self.span,
            expected.map(|s| s.to_string()),
        ))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenLiteral {
    /// `123`
    /// `0x123`
    /// `0b101`
    /// `0o123`
    /// `123_456`
    /// `123u32`
    Int(IntLiteral),
    /// `123.456`
    /// `123e10`
    /// `123.0e10`
    /// `123.0f32`
    Float(FloatLiteral),
    /// `"hello"`
    /// TODO:
    /// `r"hello"`
    /// `b"hello"`
    /// `br"hello"`
    String(StringLiteral),
    /// `'c'`
    /// TOOD:
    /// `b'c'`
    Char(CharLiteral),
    /// `true` or `false`
    Bool(BoolLiteral),
    /// 'nil'
    Nil(NilLiteral),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntLiteral {
    pub value:  &'static str,
    pub radix:  Radix,
    pub suffix: Option<&'static str>,
    pub span:   Span,
}

impl IntLiteral {
    pub fn as_len(&self) -> Result<usize> {
        if self.suffix.is_some() {
            return Err(CompileError::Error(
                self.span,
                "cannot convert integer literal with suffix to length".to_string(),
            ));
        }
        let s = self.value.replace('_', "");
        let len = match self.radix {
            #[allow(clippy::from_str_radix_10)]
            Radix::Decimal => usize::from_str_radix(&s, 10),
            Radix::Hex => usize::from_str_radix(&s[2..], 16),
            Radix::Octal => usize::from_str_radix(&s[2..], 8),
            Radix::Binary => usize::from_str_radix(&s[2..], 2),
        };
        match len {
            Ok(len) => Ok(len),
            Err(_) => {
                Err(CompileError::Error(
                    self.span,
                    "integer literal is too large to be array length".to_string(),
                ))
            },
        }
    }

    pub fn value(&self) -> Result<i128> {
        let value = self.value.replace('_', "");
        let res = match self.radix {
            #[allow(clippy::from_str_radix_10)]
            Radix::Decimal => i128::from_str_radix(&value, 10),
            Radix::Hex => i128::from_str_radix(&value[2..], 16),
            Radix::Octal => i128::from_str_radix(&value[2..], 8),
            Radix::Binary => i128::from_str_radix(&value[2..], 2),
        };
        match res {
            Ok(v) => Ok(v),
            Err(_) => {
                Err(CompileError::Error(
                    self.span,
                    "invalid integer literal".to_string(),
                ))
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Radix {
    Decimal,
    Hex,
    Octal,
    Binary,
}

impl Radix {
    pub fn is_digit(&self, c: char) -> bool {
        match self {
            Radix::Decimal => c.is_ascii_digit(),
            Radix::Hex => c.is_ascii_hexdigit(),
            Radix::Octal => matches!(c, '0'..='7'),
            Radix::Binary => c == '0' || c == '1',
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FloatLiteral {
    pub value:  &'static str,
    pub suffix: Option<&'static str>,
    pub span:   Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringLiteral {
    pub value: String,
    pub span:  Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CharLiteral {
    pub value: char,
    pub span:  Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BoolLiteral {
    pub value: bool,
    pub span:  Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NilLiteral {
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Symbol {
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
    And,
    /// `|`
    Pipe,
    /// `^`
    Caret,
    /// `~`
    Tilde,
    /// `=`
    Equal,
    /// `==`
    EqualEqual,
    /// `!=`
    BangEqual,
    /// `<=`
    LessEqual,
    /// `>=`
    GreaterEqual,
    /// `<`
    Less,
    /// `>`
    Greater,
    /// `&&`
    AndAnd,
    /// `||`
    PipePipe,
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
    ColonColon,
    /// `(`
    OpenParen,
    /// `)`
    CloseParen,
    /// `{`
    OpenBrace,
    /// `}`
    CloseBrace,
    /// `[`
    OpenBracket,
    /// `]`
    CloseBracket,
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
    AndEqual,
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

impl Symbol {
    pub fn as_str(&self) -> &'static str {
        match self {
            Symbol::Plus => "+",
            Symbol::Minus => "-",
            Symbol::Star => "*",
            Symbol::Slash => "/",
            Symbol::Percent => "%",
            Symbol::Bang => "!",
            Symbol::And => "&",
            Symbol::Pipe => "|",
            Symbol::Caret => "^",
            Symbol::Tilde => "~",
            Symbol::Equal => "=",
            Symbol::EqualEqual => "==",
            Symbol::BangEqual => "!=",
            Symbol::LessEqual => "<=",
            Symbol::GreaterEqual => ">=",
            Symbol::Less => "<",
            Symbol::Greater => ">",
            Symbol::AndAnd => "&&",
            Symbol::PipePipe => "||",
            Symbol::FatArrow => "=>",
            Symbol::Arrow => "->",
            Symbol::ReverseArrow => "<-",
            Symbol::Semicolon => ";",
            Symbol::Comma => ",",
            Symbol::Colon => ":",
            Symbol::ColonColon => "::",
            Symbol::OpenParen => "(",
            Symbol::CloseParen => ")",
            Symbol::OpenBrace => "{",
            Symbol::CloseBrace => "}",
            Symbol::OpenBracket => "[",
            Symbol::CloseBracket => "]",
            Symbol::Question => "?",
            Symbol::At => "@",
            Symbol::Hash => "#",
            Symbol::Dot => ".",
            Symbol::DotDot => "..",
            Symbol::PlusEqual => "+=",
            Symbol::MinusEqual => "-=",
            Symbol::StarEqual => "*=",
            Symbol::SlashEqual => "/=",
            Symbol::PercentEqual => "%=",
            Symbol::AndEqual => "&=",
            Symbol::PipeEqual => "|=",
            Symbol::CaretEqual => "^=",
            Symbol::ShiftLeft => "<<",
            Symbol::ShiftRight => ">>",
            Symbol::ShiftLeftEqual => "<<=",
            Symbol::ShiftRightEqual => ">>=",
            Symbol::Dollar => "$",
            Symbol::DotDotEqual => "..=",
            Symbol::Underscore => "_",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenKeyword {
    pub kind: Keyword,
    pub span: Span,
}

impl TokenKeyword {
    pub fn from_str(s: &'static str, span: Span) -> Option<Self> {
        let kind = match s {
            "fn" => Keyword::Fn,
            "let" => Keyword::Let,
            "mut" => Keyword::Mut,
            "if" => Keyword::If,
            "else" => Keyword::Else,
            "while" => Keyword::While,
            "for" => Keyword::For,
            "loop" => Keyword::Loop,
            "in" => Keyword::In,
            "return" => Keyword::Return,
            "break" => Keyword::Break,
            "continue" => Keyword::Continue,
            "struct" => Keyword::Struct,
            "enum" => Keyword::Enum,
            "impl" => Keyword::Impl,
            "mod" => Keyword::Mod,
            "use" => Keyword::Use,
            "pub" => Keyword::Pub,
            "const" => Keyword::Const,
            "static" => Keyword::Static,
            "as" => Keyword::As,
            "match" => Keyword::Match,
            "trait" => Keyword::Trait,
            "type" => Keyword::Type,
            "where" => Keyword::Where,
            "self" => Keyword::SelfLower,
            "Self" => Keyword::SelfUpper,
            "super" => Keyword::Super,
            "dyn" => Keyword::Dyn,
            "ref" => Keyword::Ref,
            "box" => Keyword::Box,
            "move" => Keyword::Move,
            "async" => Keyword::Async,
            "await" => Keyword::Await,
            "try" => Keyword::Try,
            "macro" => Keyword::Macro,
            "extern" => Keyword::Extern,
            "unsafe" => Keyword::Unsafe,
            "virtual" => Keyword::Virtual,
            "override" => Keyword::Override,
            "final" => Keyword::Final,
            "abstract" => Keyword::Abstract,
            "sealed" => Keyword::Sealed,
            "event" => Keyword::Event,
            "emit" => Keyword::Emit,
            "from" => Keyword::From,
            "new" => Keyword::New,
            "pkg" => Keyword::Pkg,
            _ => return None,
        };
        Some(Self { kind, span })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Fn,
    Let,
    Mut,
    If,
    Else,
    While,
    For,
    Loop,
    In,
    Return,
    Break,
    Continue,
    Struct,
    Enum,
    Impl,
    Mod,
    Use,
    Pub,
    Const,
    Static,
    As,
    Match,
    Trait,
    Type,
    Where,
    SelfLower,
    SelfUpper,
    Super,
    Dyn,
    Ref,
    Box,
    Move,
    Async,
    Await,
    Try,
    Macro,
    Extern,
    Unsafe,
    Virtual,
    Override,
    Final,
    Abstract,
    Sealed,
    Event,
    Emit,
    From,
    New,
    Pkg,
}

impl Keyword {
    pub fn as_str(&self) -> &'static str {
        match self {
            Keyword::Fn => "fn",
            Keyword::Let => "let",
            Keyword::Mut => "mut",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::While => "while",
            Keyword::For => "for",
            Keyword::Loop => "loop",
            Keyword::In => "in",
            Keyword::Return => "return",
            Keyword::Break => "break",
            Keyword::Continue => "continue",
            Keyword::Struct => "struct",
            Keyword::Enum => "enum",
            Keyword::Impl => "impl",
            Keyword::Mod => "mod",
            Keyword::Use => "use",
            Keyword::Pub => "pub",
            Keyword::Const => "const",
            Keyword::Static => "static",
            Keyword::As => "as",
            Keyword::Match => "match",
            Keyword::Trait => "trait",
            Keyword::Type => "type",
            Keyword::Where => "where",
            Keyword::SelfLower => "self",
            Keyword::SelfUpper => "Self",
            Keyword::Super => "super",
            Keyword::Dyn => "dyn",
            Keyword::Ref => "ref",
            Keyword::Box => "box",
            Keyword::Move => "move",
            Keyword::Async => "async",
            Keyword::Await => "await",
            Keyword::Try => "try",
            Keyword::Macro => "macro",
            Keyword::Extern => "extern",
            Keyword::Unsafe => "unsafe",
            Keyword::Virtual => "virtual",
            Keyword::Override => "override",
            Keyword::Final => "final",
            Keyword::Abstract => "abstract",
            Keyword::Sealed => "sealed",
            Keyword::Event => "event",
            Keyword::Emit => "emit",
            Keyword::From => "from",
            Keyword::New => "new",
            Keyword::Pkg => "pkg",
        }
    }
}
