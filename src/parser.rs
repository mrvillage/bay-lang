use concrete::*;
use token::*;

use crate::{
    prelude::*,
    store::{self, add_file},
};

pub struct Cursor<'a> {
    tokens: &'a [Token],
    pos:    usize,
}

impl<'a> Cursor<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn peek(&self) -> Option<&'a Token> {
        self.tokens.get(self.pos)
    }

    pub fn next(&mut self) -> Option<&'a Token> {
        let token = self.tokens.get(self.pos);
        if token.is_some() {
            self.pos += 1;
        }
        token
    }

    pub fn fork(&self) -> Self {
        Self {
            tokens: self.tokens,
            pos:    self.pos,
        }
    }

    pub fn unfork(&mut self, other: Self) {
        self.pos = other.pos;
    }

    #[allow(unused)]
    pub fn tokens(&self) -> &'a [Token] {
        self.tokens[self.pos..].as_ref()
    }

    pub fn is_eof(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    pub fn prev_span(&self) -> Span {
        if self.pos == 0 {
            panic!("no previous token");
        }
        self.tokens[self.pos - 1].span()
    }

    pub fn unexpected_end_of_input<T>(&self) -> Result<T> {
        Err(CompileError::UnexpectedEndOfInput(
            self.tokens
                .last()
                .map(|t| t.span())
                .unwrap_or_else(|| panic!("no tokens")),
        ))
    }

    pub fn consume_ident(&mut self) -> Result<&'a Ident> {
        let fork = self.fork();
        if let Some(token) = self.next() {
            match token.consume_ident() {
                Ok(t) => Ok(t),
                Err(e) => {
                    self.unfork(fork);
                    Err(e)
                },
            }
        } else {
            self.unexpected_end_of_input()
        }
    }

    pub fn consume_symbol(&mut self, sym: Symbol) -> Result<&'a TokenSymbol> {
        let fork = self.fork();
        if let Some(token) = self.next() {
            match token.consume_symbol(sym) {
                Ok(t) => Ok(t),
                Err(e) => {
                    self.unfork(fork);
                    Err(e)
                },
            }
        } else {
            self.unexpected_end_of_input()
        }
    }

    pub fn consume_keyword(&mut self, kind: Keyword) -> Result<&'a TokenKeyword> {
        let fork = self.fork();
        if let Some(token) = self.next() {
            match token.consume_keyword(kind) {
                Ok(t) => Ok(t),
                Err(e) => {
                    self.unfork(fork);
                    Err(e)
                },
            }
        } else {
            self.unexpected_end_of_input()
        }
    }

    pub fn consume_literal(&mut self) -> Result<&'a TokenLiteral> {
        let fork = self.fork();
        if let Some(token) = self.next() {
            match token.consume_literal() {
                Ok(t) => Ok(t),
                Err(e) => {
                    self.unfork(fork);
                    Err(e)
                },
            }
        } else {
            self.unexpected_end_of_input()
        }
    }

    pub fn consume_int_literal(&mut self) -> Result<&'a IntLiteral> {
        let fork = self.fork();
        if let Some(token) = self.next() {
            match token.consume_int_literal() {
                Ok(t) => Ok(t),
                Err(e) => {
                    self.unfork(fork);
                    Err(e)
                },
            }
        } else {
            self.unexpected_end_of_input()
        }
    }
}

trait Parse: Sized {
    fn parse(cursor: &mut Cursor) -> Result<Self>;

    #[allow(unused)]
    fn unparse(&self) -> String;
}

pub fn parse(tokens: &[Token]) -> Result<Source> {
    let mut cursor = Cursor::new(tokens);
    let mut items = Vec::new();
    while !cursor.is_eof() {
        let item = Item::parse(&mut cursor)?;
        items.push(item);
    }

    Ok(Source { items })
}

impl Parse for Source {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let mut items = Vec::new();
        while !cursor.is_eof() {
            let item = Item::parse(cursor)?;
            items.push(item);
        }
        Ok(Source { items })
    }

    fn unparse(&self) -> String {
        self.items
            .iter()
            .map(|i| i.unparse())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

impl Parse for Item {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let mut fork = cursor.fork();
        // try parsing a use item first
        Option::<Visibility>::parse(&mut fork)?;
        if fork.consume_keyword(Keyword::Use).is_ok() {
            return UseItem::parse(cursor).map(Item::Use);
        } else if fork.consume_keyword(Keyword::Fn).is_ok() {
            return FnItem::parse(cursor).map(Item::Fn);
        } else if fork.consume_keyword(Keyword::Struct).is_ok() {
            return StructItem::parse(cursor).map(Item::Struct);
        } else if fork.consume_keyword(Keyword::Enum).is_ok() {
            return EnumItem::parse(cursor).map(Item::Enum);
        } else if fork.consume_keyword(Keyword::Const).is_ok() {
            return ConstItem::parse(cursor).map(Item::Const);
        } else if fork.consume_keyword(Keyword::Mod).is_ok() {
            return ModItem::parse(cursor).map(Item::Mod);
        }

        if let Some(token) = cursor.peek() {
            token.unexpected_token(Some(
                "one of `use`, `fn`, `struct`, `enum`, `const`, or `mod`".to_string(),
            ))
        } else {
            cursor.unexpected_end_of_input()
        }
    }

    fn unparse(&self) -> String {
        match self {
            Item::Use(item) => item.unparse(),
            Item::Fn(item) => item.unparse(),
            Item::Struct(item) => item.unparse(),
            Item::Enum(item) => item.unparse(),
            Item::Const(item) => item.unparse(),
            Item::Mod(item) => item.unparse(),
        }
    }
}

impl Parse for Option<Visibility> {
    fn parse(cursor: &mut Cursor) -> Result<Option<Visibility>> {
        if cursor.consume_keyword(Keyword::Pub).is_ok() {
            Ok(Some(Visibility::Public))
        } else {
            Ok(None)
        }
    }

    fn unparse(&self) -> String {
        match self {
            Some(Visibility::Public) => "pub ".to_string(),
            Some(Visibility::Private) | None => "".to_string(),
        }
    }
}

impl Parse for UseItem {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let visibility = Option::<Visibility>::parse(cursor)?;
        cursor.consume_keyword(Keyword::Use)?;
        let span = cursor.peek().map(|t| t.span()).unwrap_or(span);
        // parsing a use tree starts with the possibility of a root `::`
        let tree = if cursor.consume_symbol(Symbol::ColonColon).is_ok() {
            let tree = UseTree::parse(cursor)?;
            UseTree::Root {
                tree: Box::new(tree),
            }
        } else {
            let tree = UseTree::parse(cursor)?;
            match tree {
                UseTree::Glob | UseTree::Group { .. } => {
                    return Err(CompileError::Error(
                        span,
                        "use cannot start with `*` or `{}`".to_string(),
                    ));
                },
                _ => tree,
            }
        };
        let token = cursor.consume_symbol(Symbol::Semicolon)?;
        let span = span.join(token.span);
        Ok(UseItem {
            visibility,
            tree,
            span,
        })
    }

    fn unparse(&self) -> String {
        format!("{}use {};", self.visibility.unparse(), self.tree.unparse())
    }
}

impl Parse for UseTree {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        if cursor.consume_symbol(Symbol::OpenBrace).is_ok() {
            // it's a group
            let mut items = Vec::new();
            while cursor.consume_symbol(Symbol::CloseBrace).is_err() {
                let item = UseTree::parse(cursor)?;
                items.push(item);
                if cursor.consume_symbol(Symbol::Comma).is_ok() {
                    continue;
                } else if cursor.consume_symbol(Symbol::CloseBrace).is_ok() {
                    break;
                } else {
                    return if cursor.is_eof() {
                        cursor.unexpected_end_of_input()
                    } else {
                        Err(CompileError::UnexpectedToken(
                            cursor.peek().unwrap().span(),
                            Some("`,` or `}`".to_string()),
                        ))
                    };
                }
            }
            Ok(UseTree::Group { items })
        } else {
            // otherwise, it starts with an ident
            if let Ok(ident) = cursor.consume_ident() {
                if cursor.consume_symbol(Symbol::ColonColon).is_ok() {
                    // followed by `::`, so it's a path
                    Ok(UseTree::Path {
                        ident: ident.clone(),
                        tree:  Box::new(UseTree::parse(cursor)?),
                    })
                } else if cursor.consume_keyword(Keyword::As).is_ok() {
                    // followed by `as`, so it's a rename
                    let alias = cursor.consume_ident()?;
                    Ok(UseTree::Rename {
                        ident: ident.clone(),
                        alias: alias.clone(),
                    })
                } else {
                    // otherwise, it's just a name
                    Ok(UseTree::Name {
                        ident: ident.clone(),
                    })
                }
            } else if cursor.consume_symbol(Symbol::Star).is_ok() {
                // followed by `*`, so it's a glob
                Ok(UseTree::Glob)
            } else {
                cursor.unexpected_end_of_input()
            }
        }
    }

    fn unparse(&self) -> String {
        match self {
            UseTree::Name { ident } => ident.value.to_string(),
            UseTree::Rename { ident, alias } => format!("{} as {}", ident.value, alias.value),
            UseTree::Glob => "*".to_string(),
            UseTree::Path { ident, tree } => format!("{}::{}", ident.value, tree.unparse()),
            UseTree::Group { items } => {
                let inner = items
                    .iter()
                    .map(|item| item.unparse())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{{}}}", inner)
            },
            UseTree::Root { tree } => format!("::{}", tree.unparse()),
            UseTree::Pkg { tree } => format!("pkg::{}", tree.unparse()),
        }
    }
}

impl Parse for FnItem {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let visibility = Option::<Visibility>::parse(cursor)?;
        cursor.consume_keyword(Keyword::Fn)?;
        let name = cursor.consume_ident()?.clone();
        cursor.consume_symbol(Symbol::OpenParen)?;
        let mut params = Vec::new();
        while cursor.consume_symbol(Symbol::CloseParen).is_err() {
            let param = FnParam::parse(cursor)?;
            params.push(param);
            if cursor.consume_symbol(Symbol::Comma).is_ok() {
                continue;
            } else if cursor.consume_symbol(Symbol::CloseParen).is_ok() {
                break;
            } else {
                return if cursor.is_eof() {
                    cursor.unexpected_end_of_input()
                } else {
                    Err(CompileError::UnexpectedToken(
                        cursor.peek().unwrap().span(),
                        Some("`,` or `)`".to_string()),
                    ))
                };
            }
        }
        let ret_ty = if cursor.consume_symbol(Symbol::Arrow).is_ok() {
            Some(ConcreteType::parse(cursor)?)
        } else {
            None
        };
        let body = Block::parse(cursor)?;
        let span = span.join(body.span);
        Ok(FnItem {
            visibility,
            name,
            params,
            ret_ty,
            body,
            span,
        })
    }

    fn unparse(&self) -> String {
        let params = self
            .params
            .iter()
            .map(|p| p.unparse())
            .collect::<Vec<_>>()
            .join(", ");
        let ret_ty = if let Some(ty) = &self.ret_ty {
            format!(" -> {}", ty.unparse())
        } else {
            "".to_string()
        };
        format!(
            "{}fn {}({}){} {}",
            self.visibility.unparse(),
            self.name.value,
            params,
            ret_ty,
            self.body.unparse()
        )
    }
}

impl Parse for FnParam {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let name = cursor.consume_ident()?.clone();
        cursor.consume_symbol(Symbol::Colon)?;
        let ty = ConcreteType::parse(cursor)?;
        let span = span.join(cursor.prev_span());
        Ok(FnParam { name, ty, span })
    }

    fn unparse(&self) -> String {
        format!("{}: {}", self.name.value, self.ty.unparse())
    }
}

impl Parse for StructItem {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let visibility = Option::<Visibility>::parse(cursor)?;
        cursor.consume_keyword(Keyword::Struct)?;
        let name = cursor.consume_ident()?.clone();
        let fields = StructFields::parse(cursor)?;
        if let StructFields::Unnamed { .. } | StructFields::Unit = &fields {
            // if it's a tuple or unit struct, it must end with a semicolon
            cursor.consume_symbol(Symbol::Semicolon)?;
        }
        let span = span.join(cursor.prev_span());
        Ok(StructItem {
            visibility,
            name,
            fields,
            span,
        })
    }

    fn unparse(&self) -> String {
        match &self.fields {
            StructFields::Named { fields } => {
                let fields = fields
                    .iter()
                    .map(|f| f.unparse())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "{}struct {} {{ {} }}",
                    self.visibility.unparse(),
                    self.name.value,
                    fields
                )
            },
            StructFields::Unnamed { fields } => {
                let fields = fields
                    .iter()
                    .map(|f| f.unparse())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "{}struct {}({});",
                    self.visibility.unparse(),
                    self.name.value,
                    fields
                )
            },
            StructFields::Unit => {
                format!("{}struct {};", self.visibility.unparse(), self.name.value)
            },
        }
    }
}

impl Parse for StructFields {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        if cursor.consume_symbol(Symbol::OpenBrace).is_ok() {
            // it's a named struct
            let mut fields = Vec::new();
            while cursor.consume_symbol(Symbol::CloseBrace).is_err() {
                let field = NamedStructField::parse(cursor)?;
                fields.push(field);
                if cursor.consume_symbol(Symbol::Comma).is_ok() {
                    continue;
                } else if cursor.consume_symbol(Symbol::CloseBrace).is_ok() {
                    break;
                } else {
                    return if cursor.is_eof() {
                        cursor.unexpected_end_of_input()
                    } else {
                        Err(CompileError::UnexpectedToken(
                            cursor.peek().unwrap().span(),
                            Some("`,` or `}`".to_string()),
                        ))
                    };
                }
            }
            Ok(StructFields::Named { fields })
        } else if cursor.consume_symbol(Symbol::OpenParen).is_ok() {
            // it's a tuple struct
            let mut fields = Vec::new();
            while cursor.consume_symbol(Symbol::CloseParen).is_err() {
                let field = UnnamedStructField::parse(cursor)?;
                fields.push(field);
                if cursor.consume_symbol(Symbol::Comma).is_ok() {
                    continue;
                } else if cursor.consume_symbol(Symbol::CloseParen).is_ok() {
                    break;
                } else {
                    return if cursor.is_eof() {
                        cursor.unexpected_end_of_input()
                    } else {
                        Err(CompileError::UnexpectedToken(
                            cursor.peek().unwrap().span(),
                            Some("`,` or `)`".to_string()),
                        ))
                    };
                }
            }
            Ok(StructFields::Unnamed { fields })
        } else {
            // it's a unit struct
            Ok(StructFields::Unit)
        }
    }

    fn unparse(&self) -> String {
        match self {
            StructFields::Named { fields } => {
                let fields = fields
                    .iter()
                    .map(|f| f.unparse())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{ {} }}", fields)
            },
            StructFields::Unnamed { fields } => {
                let fields = fields
                    .iter()
                    .map(|f| f.unparse())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", fields)
            },
            StructFields::Unit => "".to_string(),
        }
    }
}

impl Parse for NamedStructField {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let visibility = Option::<Visibility>::parse(cursor)?;
        let name = cursor.consume_ident()?.clone();
        cursor.consume_symbol(Symbol::Colon)?;
        let ty = ConcreteType::parse(cursor)?;
        let span = span.join(cursor.prev_span());
        Ok(NamedStructField {
            visibility,
            name,
            ty,
            span,
        })
    }

    fn unparse(&self) -> String {
        format!(
            "{}{}: {}",
            self.visibility.unparse(),
            self.name.value,
            self.ty.unparse()
        )
    }
}

impl Parse for UnnamedStructField {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let visibility = Option::<Visibility>::parse(cursor)?;
        let ty = ConcreteType::parse(cursor)?;
        let span = span.join(cursor.prev_span());
        Ok(UnnamedStructField {
            visibility,
            ty,
            span,
        })
    }

    fn unparse(&self) -> String {
        format!("{}{}", self.visibility.unparse(), self.ty.unparse())
    }
}

impl Parse for EnumItem {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let visibility = Option::<Visibility>::parse(cursor)?;
        cursor.consume_keyword(Keyword::Enum)?;
        let name = cursor.consume_ident()?.clone();
        cursor.consume_symbol(Symbol::OpenBrace)?;
        let mut variants = Vec::new();
        while cursor.consume_symbol(Symbol::CloseBrace).is_err() {
            let variant = EnumVariant::parse(cursor)?;
            variants.push(variant);
            if cursor.consume_symbol(Symbol::Comma).is_ok() {
                continue;
            } else if cursor.consume_symbol(Symbol::CloseBrace).is_ok() {
                break;
            } else {
                return if cursor.is_eof() {
                    cursor.unexpected_end_of_input()
                } else {
                    Err(CompileError::UnexpectedToken(
                        cursor.peek().unwrap().span(),
                        Some("`,` or `}`".to_string()),
                    ))
                };
            }
        }
        let span = span.join(variants.last().map(|v| v.span).unwrap_or(span));
        Ok(EnumItem {
            visibility,
            name,
            variants,
            span,
        })
    }

    fn unparse(&self) -> String {
        let variants = self
            .variants
            .iter()
            .map(|v| v.unparse())
            .collect::<Vec<_>>()
            .join(", ");
        format!(
            "{}enum {} {{ {} }}",
            self.visibility.unparse(),
            self.name.value,
            variants
        )
    }
}

impl Parse for EnumVariant {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let name = cursor.consume_ident()?.clone();
        let fields = StructFields::parse(cursor)?;
        let disc = if cursor.consume_symbol(Symbol::Equal).is_ok() {
            Some(cursor.consume_int_literal()?.clone())
        } else {
            None
        };
        let span = span.join(cursor.prev_span());
        Ok(EnumVariant {
            name,
            fields,
            disc,
            span,
        })
    }

    fn unparse(&self) -> String {
        let disc = if let Some(disc) = &self.disc {
            format!(" = {}", disc.value)
        } else {
            "".to_string()
        };
        if let StructFields::Named { .. } = &self.fields {
            format!("{} {}{}", self.name.value, self.fields.unparse(), disc)
        } else {
            format!("{}{}{}", self.name.value, self.fields.unparse(), disc)
        }
    }
}

impl Parse for ConstItem {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let visibility = Option::<Visibility>::parse(cursor)?;
        cursor.consume_keyword(Keyword::Const)?;
        let name = cursor.consume_ident()?.clone();
        cursor.consume_symbol(Symbol::Colon)?;
        let ty = ConcreteType::parse(cursor)?;
        cursor.consume_symbol(Symbol::Equal)?;
        let expr = Expr::parse(cursor)?;
        let token = cursor.consume_symbol(Symbol::Semicolon)?;
        let span = span.join(token.span);
        Ok(ConstItem {
            visibility,
            name,
            ty,
            expr,
            span,
        })
    }

    fn unparse(&self) -> String {
        format!(
            "{}const {}: {} = {};",
            self.visibility.unparse(),
            self.name.value,
            self.ty.unparse(),
            self.expr.unparse()
        )
    }
}

impl Parse for ModItem {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let visibility = Option::<Visibility>::parse(cursor)?;
        cursor.consume_keyword(Keyword::Mod)?;
        let name = cursor.consume_ident()?.clone();
        let file_name = name.value;
        if cursor.consume_symbol(Symbol::Semicolon).is_ok() {
            // we need to load the module from a file
            if cfg!(test) {
                return Ok(ModItem {
                    visibility,
                    name,
                    items: Vec::new(),
                    span,
                    inline: false,
                });
            }
            let parent = store::get_file_path(span.file).parent().unwrap();
            // first we try `name.bay`
            let file_path = parent.join(format!("{}.bay", file_name));
            let file = if file_path.exists() {
                file_path
            } else {
                // then we try `name/mod.bay`
                let mod_path = parent.join(file_name).join("mod.bay");
                if mod_path.exists() {
                    mod_path
                } else {
                    return Err(CompileError::Error(
                        span,
                        format!(
                            "file not found for module `{}`, create `{}` or `{}`",
                            file_name,
                            file_path
                                .strip_prefix(config().root())
                                .unwrap_or(&file_path)
                                .display(),
                            mod_path
                                .strip_prefix(config().root())
                                .unwrap_or(&mod_path)
                                .display(),
                        ),
                    ));
                }
            };
            let (file, content) = add_file(&file)?;
            match crate::tokenizer::tokenize_str(content, file) {
                Ok(tokens) => {
                    let Source { items } = crate::parser::parse(&tokens)?;
                    Ok(ModItem {
                        visibility,
                        name,
                        items,
                        span: span.join(cursor.prev_span()),
                        inline: false,
                    })
                },
                Err(e) => {
                    e.log();
                    std::process::exit(1);
                },
            }
        } else {
            // the module is defined inline
            cursor.consume_symbol(Symbol::OpenBrace)?;
            let mut items = Vec::new();
            while cursor.consume_symbol(Symbol::CloseBrace).is_err() {
                let item = Item::parse(cursor)?;
                items.push(item);
            }
            let span = span.join(cursor.prev_span());
            Ok(ModItem {
                visibility,
                name,
                items,
                span,
                inline: true,
            })
        }
    }

    fn unparse(&self) -> String {
        if self.inline {
            let items = self
                .items
                .iter()
                .map(|i| i.unparse())
                .collect::<Vec<_>>()
                .join("\n");
            format!(
                "{}mod {} {{\n{}\n}}",
                self.visibility.unparse(),
                self.name.value,
                items
            )
        } else {
            format!("{}mod {};", self.visibility.unparse(), self.name.value)
        }
    }
}

impl Parse for Block {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        cursor.consume_symbol(Symbol::OpenBrace)?;
        let mut stmts = Vec::new();
        while cursor.consume_symbol(Symbol::CloseBrace).is_err() {
            let span = cursor.peek().map(|t| t.span()).unwrap_or(span);
            let stmt = Stmt::parse(cursor)?;
            let span = span.join(cursor.prev_span());
            stmts.push((stmt, span));
            if cursor.consume_symbol(Symbol::CloseBrace).is_ok() {
                break;
            } else if cursor.is_eof() {
                return cursor.unexpected_end_of_input();
            }
        }
        // now we want to double check that none of the statements are non-block
        // expressions that are missing semicolons
        if stmts.len() > 1 {
            for (stmt, span) in &stmts[..stmts.len() - 1] {
                if let Stmt::Expr(expr) = stmt {
                    if !expr.is_block_expr() {
                        return Err(CompileError::Error(
                            *span,
                            "missing semicolon, or wrap this expression in a block `{}`"
                                .to_string(),
                        ));
                    }
                }
            }
        }
        let stmts = stmts.into_iter().map(|(s, _)| s).collect();
        let span = span.join(cursor.prev_span());
        Ok(Block { stmts, span })
    }

    fn unparse(&self) -> String {
        if self.stmts.is_empty() {
            return "{}".to_string();
        }
        let stmts = self
            .stmts
            .iter()
            .map(|s| s.unparse())
            .collect::<Vec<_>>()
            .join(" ");
        format!("{{ {} }}", stmts)
    }
}

impl Parse for Stmt {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let mut fork = cursor.fork();
        if fork.consume_keyword(Keyword::Let).is_ok() {
            LetStmt::parse(cursor).map(Stmt::Let)
        } else {
            // try parsing a use item first
            Option::<Visibility>::parse(&mut fork)?;
            if fork.consume_keyword(Keyword::Use).is_ok() {
                UseItem::parse(cursor).map(Item::Use).map(Stmt::Item)
            } else if fork.consume_keyword(Keyword::Fn).is_ok() {
                FnItem::parse(cursor).map(Item::Fn).map(Stmt::Item)
            } else if fork.consume_keyword(Keyword::Struct).is_ok() {
                StructItem::parse(cursor).map(Item::Struct).map(Stmt::Item)
            } else if fork.consume_keyword(Keyword::Enum).is_ok() {
                EnumItem::parse(cursor).map(Item::Enum).map(Stmt::Item)
            } else if fork.consume_keyword(Keyword::Const).is_ok() {
                ConstItem::parse(cursor).map(Item::Const).map(Stmt::Item)
            } else if fork.consume_keyword(Keyword::Mod).is_ok() {
                ModItem::parse(cursor).map(Item::Mod).map(Stmt::Item)
            } else {
                let expr = Expr::parse(cursor)?;
                if cursor.consume_symbol(Symbol::Semicolon).is_ok() {
                    Ok(Stmt::Semi(expr))
                } else {
                    Ok(Stmt::Expr(expr))
                }
            }
        }
    }

    fn unparse(&self) -> String {
        match self {
            Stmt::Let(stmt) => stmt.unparse(),
            Stmt::Semi(expr) => format!("{};", expr.unparse()),
            Stmt::Expr(expr) => expr.unparse(),
            Stmt::Item(item) => item.unparse(),
        }
    }
}

impl Parse for LetStmt {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        cursor.consume_keyword(Keyword::Let)?;
        let pattern = IdentPattern::parse(cursor)?;
        // optional type annotation
        let ty = if cursor.consume_symbol(Symbol::Colon).is_ok() {
            Some(ConcreteType::parse(cursor)?)
        } else {
            None
        };
        // optional initializer
        let expr = if cursor.consume_symbol(Symbol::Equal).is_ok() {
            Some(Expr::parse(cursor)?)
        } else {
            None
        };
        // optional else clause for destructuring
        let else_expr = if cursor.consume_keyword(Keyword::Else).is_ok() {
            Some(Block::parse(cursor)?)
        } else {
            None
        };
        // required semicolon
        cursor.consume_symbol(Symbol::Semicolon)?;
        let span = span.join(cursor.prev_span());
        Ok(LetStmt {
            pattern,
            ty,
            expr,
            else_block: else_expr,
            span,
        })
    }

    fn unparse(&self) -> String {
        let ty = if let Some(ty) = &self.ty {
            format!(": {}", ty.unparse())
        } else {
            "".to_string()
        };
        let expr = if let Some(expr) = &self.expr {
            format!(" = {}", expr.unparse())
        } else {
            "".to_string()
        };
        let else_expr = if let Some(else_expr) = &self.else_block {
            format!(" else {}", else_expr.unparse())
        } else {
            "".to_string()
        };
        format!("let {}{}{}{};", self.pattern.unparse(), ty, expr, else_expr)
    }
}

impl Parse for Path {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let mut segments = Vec::new();
        let root = cursor.consume_symbol(Symbol::ColonColon).is_ok();
        let pkg = cursor.consume_keyword(Keyword::Pkg).is_ok();
        if pkg {
            cursor.consume_symbol(Symbol::ColonColon)?;
        }
        loop {
            let ident = cursor.consume_ident()?.clone();
            segments.push(ident);
            if cursor.consume_symbol(Symbol::ColonColon).is_ok() {
                continue;
            } else {
                break;
            }
        }
        let span = span.join(cursor.prev_span());
        Ok(Path {
            root,
            pkg,
            segments,
            span,
        })
    }

    fn unparse(&self) -> String {
        let mut path = String::new();
        if self.root {
            path.push_str("::");
        }
        path.push_str(
            &self
                .segments
                .iter()
                .map(|s| s.value.to_string())
                .collect::<Vec<_>>()
                .join("::"),
        );
        path
    }
}

impl Parse for Pattern {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        if cursor.consume_symbol(Symbol::Underscore).is_ok() {
            Ok(Pattern::Wildcard)
        } else if let Ok(path) = Path::parse(cursor) {
            Ok(Pattern::Path(path.clone()))
        } else if cursor.consume_symbol(Symbol::OpenParen).is_ok() {
            // it's a tuple pattern
            let mut elements = Vec::new();
            while cursor.consume_symbol(Symbol::CloseParen).is_err() {
                let element = Pattern::parse(cursor)?;
                elements.push(element);
                if cursor.consume_symbol(Symbol::Comma).is_ok() {
                    continue;
                } else if cursor.consume_symbol(Symbol::CloseParen).is_ok() {
                    break;
                } else {
                    return if cursor.is_eof() {
                        cursor.unexpected_end_of_input()
                    } else {
                        Err(CompileError::UnexpectedToken(
                            cursor.peek().unwrap().span(),
                            Some("`,` or `)`".to_string()),
                        ))
                    };
                }
            }
            Ok(Pattern::Tuple(elements))
        } else if let Some(token) = cursor.peek() {
            token.unexpected_token(Some("a pattern"))
        } else {
            cursor.unexpected_end_of_input()
        }
    }

    fn unparse(&self) -> String {
        match self {
            Pattern::Wildcard => "_".to_string(),
            Pattern::Path(path) => path.unparse(),
            Pattern::Tuple(elements) => {
                let elements = elements
                    .iter()
                    .map(|e| e.unparse())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", elements)
            },
        }
    }
}

impl Parse for IdentPattern {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        if cursor.consume_symbol(Symbol::Underscore).is_ok() {
            Ok(IdentPattern::Wildcard)
        } else if let Ok(ident) = cursor.consume_ident() {
            Ok(IdentPattern::Ident(ident.clone()))
        } else if cursor.consume_symbol(Symbol::OpenParen).is_ok() {
            // it's a tuple pattern
            let mut elements = Vec::new();
            while cursor.consume_symbol(Symbol::CloseParen).is_err() {
                let element = IdentPattern::parse(cursor)?;
                elements.push(element);
                if cursor.consume_symbol(Symbol::Comma).is_ok() {
                    continue;
                } else if cursor.consume_symbol(Symbol::CloseParen).is_ok() {
                    break;
                } else {
                    return if cursor.is_eof() {
                        cursor.unexpected_end_of_input()
                    } else {
                        Err(CompileError::UnexpectedToken(
                            cursor.peek().unwrap().span(),
                            Some("`,` or `)`".to_string()),
                        ))
                    };
                }
            }
            Ok(IdentPattern::Tuple(elements))
        } else if let Some(token) = cursor.peek() {
            token.unexpected_token(Some("a pattern"))
        } else {
            cursor.unexpected_end_of_input()
        }
    }

    fn unparse(&self) -> String {
        match self {
            IdentPattern::Wildcard => "_".to_string(),
            IdentPattern::Ident(ident) => ident.value.to_string(),
            IdentPattern::Tuple(elements) => {
                let elements = elements
                    .iter()
                    .map(|e| e.unparse())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", elements)
            },
        }
    }
}

impl Parse for FieldPattern {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        if cursor.consume_symbol(Symbol::Underscore).is_ok() {
            Ok(FieldPattern::Wildcard)
        } else if let Ok(ident) = cursor.consume_ident() {
            let mut field = Vec::new();
            field.push(ident.clone());
            while cursor.consume_symbol(Symbol::Dot).is_ok() {
                let ident = cursor.consume_ident()?;
                field.push(ident.clone());
            }
            Ok(FieldPattern::Field(field))
        } else if cursor.consume_symbol(Symbol::OpenParen).is_ok() {
            // it's a tuple pattern
            let mut elements = Vec::new();
            while cursor.consume_symbol(Symbol::CloseParen).is_err() {
                let element = IdentPattern::parse(cursor)?;
                elements.push(element);
                if cursor.consume_symbol(Symbol::Comma).is_ok() {
                    continue;
                } else if cursor.consume_symbol(Symbol::CloseParen).is_ok() {
                    break;
                } else {
                    return if cursor.is_eof() {
                        cursor.unexpected_end_of_input()
                    } else {
                        Err(CompileError::UnexpectedToken(
                            cursor.peek().unwrap().span(),
                            Some("`,` or `)`".to_string()),
                        ))
                    };
                }
            }
            Ok(FieldPattern::Tuple(elements))
        } else if let Some(token) = cursor.peek() {
            token.unexpected_token(Some("a pattern"))
        } else {
            cursor.unexpected_end_of_input()
        }
    }

    fn unparse(&self) -> String {
        match self {
            FieldPattern::Wildcard => "_".to_string(),
            FieldPattern::Field(field) => {
                field
                    .iter()
                    .map(|ident| ident.value.to_string())
                    .collect::<Vec<_>>()
                    .join(".")
            },
            FieldPattern::Tuple(elements) => {
                let elements = elements
                    .iter()
                    .map(|e| e.unparse())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", elements)
            },
        }
    }
}

impl Parse for ConcreteType {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        // ref, slice, array, tuple, path
        let ty = if cursor.consume_symbol(Symbol::And).is_ok() {
            // reference type
            let ty = ConcreteType::parse(cursor)?;
            ConcreteType::Ref(Box::new(ty))
        } else if cursor.consume_symbol(Symbol::OpenBracket).is_ok() {
            // slice or array type
            let ty = ConcreteType::parse(cursor)?;
            if cursor.consume_symbol(Symbol::Semicolon).is_ok() {
                // array type
                let len = cursor.consume_int_literal()?.clone();
                cursor.consume_symbol(Symbol::CloseBracket)?;
                ConcreteType::Array {
                    ty: Box::new(ty),
                    len,
                }
            } else {
                // slice type
                cursor.consume_symbol(Symbol::CloseBracket)?;
                ConcreteType::Slice(Box::new(ty))
            }
        } else if cursor.consume_symbol(Symbol::OpenParen).is_ok() {
            // tuple type
            let mut elements = Vec::new();
            while cursor.consume_symbol(Symbol::CloseParen).is_err() {
                let element = ConcreteType::parse(cursor)?;
                elements.push(element);
                if cursor.consume_symbol(Symbol::Comma).is_ok() {
                    continue;
                } else if cursor.consume_symbol(Symbol::CloseParen).is_ok() {
                    break;
                } else {
                    return if cursor.is_eof() {
                        cursor.unexpected_end_of_input()
                    } else {
                        Err(CompileError::UnexpectedToken(
                            cursor.peek().unwrap().span(),
                            Some("`,` or `)`".to_string()),
                        ))
                    };
                }
            }
            ConcreteType::Tuple(elements)
        } else {
            // path type
            let path = Path::parse(cursor)?;
            ConcreteType::Path(path)
        };
        if cursor.consume_symbol(Symbol::Question).is_ok() {
            Ok(ConcreteType::Optional(Box::new(ty)))
        } else {
            Ok(ty)
        }
    }

    fn unparse(&self) -> String {
        match self {
            ConcreteType::Ref(ty) => format!("&{}", ty.unparse()),
            ConcreteType::Slice(ty) => format!("[{}]", ty.unparse()),
            ConcreteType::Array { ty, len } => format!("[{}; {}]", ty.unparse(), len.value),
            ConcreteType::Tuple(elements) => {
                let elements = elements
                    .iter()
                    .map(|e| e.unparse())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", elements)
            },
            ConcreteType::Path(path) => path.unparse(),
            ConcreteType::Optional(ty) => format!("{}?", ty.unparse()),
        }
    }
}

impl Parse for Expr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        if let Ok(kind) = ControlFlowKind::parse(cursor) {
            let mut fork = cursor.fork();
            let expr = if fork.consume_symbol(Symbol::Semicolon).is_ok() {
                None
            } else {
                let expr = RangeExpr::parse(cursor)?;
                let mut fork = cursor.fork();
                if fork.consume_symbol(Symbol::Semicolon).is_err() {
                    return Err(CompileError::Error(
                        span.join(cursor.prev_span()),
                        "missing semicolon after control flow expression".to_string(),
                    ));
                }
                Some(Box::new(expr))
            };
            let span = span.join(cursor.prev_span());
            Ok(Expr::ControlFlow { kind, expr, span })
        } else {
            AssignExpr::parse(cursor).map(Box::new).map(Expr::Assign)
        }
    }

    fn unparse(&self) -> String {
        match self {
            Expr::Assign(assign) => assign.unparse(),
            Expr::ControlFlow { kind, expr, .. } => {
                let expr = if let Some(expr) = expr {
                    format!(" {}", expr.unparse())
                } else {
                    "".to_string()
                };
                format!("{}{}", kind.unparse(), expr)
            },
        }
    }
}

impl Parse for AssignExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let mut fork = cursor.fork();
        if let Ok(pattern) = FieldPattern::parse(&mut fork) {
            if let Ok(op) = AssignOp::parse(&mut fork) {
                cursor.unfork(fork);
                let expr = RangeExpr::parse(cursor)?;
                let span = span.join(cursor.prev_span());
                Ok(AssignExpr::Assign {
                    pattern,
                    op,
                    expr: Box::new(expr),
                    span,
                })
            } else {
                RangeExpr::parse(cursor)
                    .map(Box::new)
                    .map(AssignExpr::Range)
            }
        } else {
            RangeExpr::parse(cursor)
                .map(Box::new)
                .map(AssignExpr::Range)
        }
    }

    fn unparse(&self) -> String {
        match self {
            AssignExpr::Range(expr) => expr.unparse(),
            AssignExpr::Assign {
                pattern, op, expr, ..
            } => format!("{} {} {}", pattern.unparse(), op.unparse(), expr.unparse()),
        }
    }
}

impl Parse for AssignOp {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        if cursor.consume_symbol(Symbol::Equal).is_ok() {
            Ok(AssignOp::Assign)
        } else if cursor.consume_symbol(Symbol::PlusEqual).is_ok() {
            Ok(AssignOp::AddAssign)
        } else if cursor.consume_symbol(Symbol::MinusEqual).is_ok() {
            Ok(AssignOp::SubAssign)
        } else if cursor.consume_symbol(Symbol::StarEqual).is_ok() {
            Ok(AssignOp::MulAssign)
        } else if cursor.consume_symbol(Symbol::SlashEqual).is_ok() {
            Ok(AssignOp::DivAssign)
        } else if cursor.consume_symbol(Symbol::PercentEqual).is_ok() {
            Ok(AssignOp::ModAssign)
        } else if cursor.consume_symbol(Symbol::CaretEqual).is_ok() {
            Ok(AssignOp::BitXorAssign)
        } else if cursor.consume_symbol(Symbol::AndEqual).is_ok() {
            Ok(AssignOp::BitAndAssign)
        } else if cursor.consume_symbol(Symbol::PipeEqual).is_ok() {
            Ok(AssignOp::BitOrAssign)
        } else if cursor.consume_symbol(Symbol::ShiftLeftEqual).is_ok() {
            Ok(AssignOp::BitShlAssign)
        } else if cursor.consume_symbol(Symbol::ShiftRightEqual).is_ok() {
            Ok(AssignOp::BitShrAssign)
        } else {
            let Some(token) = cursor.peek() else {
                return cursor.unexpected_end_of_input();
            };
            token.unexpected_token(Some("an assignment operator".to_string()))
        }
    }

    fn unparse(&self) -> String {
        match self {
            AssignOp::Assign => "=".to_string(),
            AssignOp::AddAssign => "+=".to_string(),
            AssignOp::SubAssign => "-=".to_string(),
            AssignOp::MulAssign => "*=".to_string(),
            AssignOp::DivAssign => "/=".to_string(),
            AssignOp::ModAssign => "%=".to_string(),
            AssignOp::BitXorAssign => "^=".to_string(),
            AssignOp::BitAndAssign => "&=".to_string(),
            AssignOp::BitOrAssign => "|=".to_string(),
            AssignOp::BitShlAssign => "<<=".to_string(),
            AssignOp::BitShrAssign => ">>=".to_string(),
        }
    }
}

impl Parse for ControlFlowKind {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        if cursor.consume_keyword(Keyword::Return).is_ok() {
            Ok(ControlFlowKind::Return)
        } else if cursor.consume_keyword(Keyword::Break).is_ok() {
            Ok(ControlFlowKind::Break)
        } else {
            let Some(token) = cursor.peek() else {
                return cursor.unexpected_end_of_input();
            };
            token.unexpected_token(Some("`return` or `break`".to_string()))
        }
    }

    fn unparse(&self) -> String {
        match self {
            ControlFlowKind::Return => "return".to_string(),
            ControlFlowKind::Break => "break".to_string(),
            ControlFlowKind::Continue => "continue".to_string(),
        }
    }
}

impl Parse for RangeExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        // optional start
        let mut fork = cursor.fork();
        let start = if fork.consume_symbol(Symbol::DotDot).is_ok()
            || fork.consume_symbol(Symbol::DotDotEqual).is_ok()
        {
            None
        } else {
            Some(Box::new(LogicalOrExpr::parse(cursor)?))
        };
        if cursor.consume_symbol(Symbol::DotDot).is_ok() {
            let end = if let Ok(expr) = LogicalOrExpr::parse(cursor) {
                Some(Box::new(expr))
            } else {
                None
            };
            Ok(RangeExpr::Range {
                start,
                end,
                inclusive: false,
                span: span.join(cursor.prev_span()),
            })
        } else if cursor.consume_symbol(Symbol::DotDotEqual).is_ok() {
            let end = if let Ok(expr) = LogicalOrExpr::parse(cursor) {
                Some(Box::new(expr))
            } else {
                None
            };
            Ok(RangeExpr::Range {
                start,
                end,
                inclusive: true,
                span: span.join(cursor.prev_span()),
            })
        } else {
            // otherwise, it's just a logical or expression
            Ok(RangeExpr::LogicalOr(start.unwrap()))
        }
    }

    fn unparse(&self) -> String {
        match self {
            RangeExpr::LogicalOr(expr) => expr.unparse(),
            RangeExpr::Range {
                start,
                end,
                inclusive,
                ..
            } => {
                let start = if let Some(start) = start {
                    start.unparse()
                } else {
                    "".to_string()
                };
                let end = if let Some(end) = end {
                    end.unparse()
                } else {
                    "".to_string()
                };
                if *inclusive {
                    format!("{}..={}", start, end)
                } else {
                    format!("{}..{}", start, end)
                }
            },
        }
    }
}

impl Parse for LogicalOrExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let mut left = LogicalOrExpr::LogicalAnd(Box::new(LogicalAndExpr::parse(cursor)?));
        while cursor.consume_symbol(Symbol::PipePipe).is_ok() {
            let right = LogicalAndExpr::parse(cursor)?;
            let span = span.join(cursor.prev_span());
            left = LogicalOrExpr::LogicalOr {
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }
        Ok(left)
    }

    fn unparse(&self) -> String {
        match self {
            LogicalOrExpr::LogicalAnd(expr) => expr.unparse(),
            LogicalOrExpr::LogicalOr { left, right, .. } => {
                format!("{} || {}", left.unparse(), right.unparse())
            },
        }
    }
}

impl Parse for LogicalAndExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let mut left = LogicalAndExpr::Comparison(Box::new(ComparisonExpr::parse(cursor)?));
        while cursor.consume_symbol(Symbol::AndAnd).is_ok() {
            let right = ComparisonExpr::parse(cursor)?;
            let span = span.join(cursor.prev_span());
            left = LogicalAndExpr::LogicalAnd {
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }
        Ok(left)
    }

    fn unparse(&self) -> String {
        match self {
            LogicalAndExpr::Comparison(expr) => expr.unparse(),
            LogicalAndExpr::LogicalAnd { left, right, .. } => {
                format!("{} && {}", left.unparse(), right.unparse())
            },
        }
    }
}

impl Parse for ComparisonExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let left = BitwiseOrExpr::parse(cursor)?;
        let Ok(op) = ComparisonOp::parse(cursor) else {
            return Ok(ComparisonExpr::BitwiseOr(Box::new(left)));
        };
        let right = BitwiseOrExpr::parse(cursor)?;
        // if the next token is another equality operator, raise an error saying
        // equality must be wrapped in parentheses
        if ComparisonOp::parse(cursor).is_ok() {
            return Err(CompileError::Error(
                cursor.prev_span(),
                "chained equality comparisons must be wrapped in parentheses".to_string(),
            ));
        }
        let span = span.join(cursor.prev_span());
        Ok(ComparisonExpr::Comparison {
            left: Box::new(left),
            op,
            right: Box::new(right),
            span,
        })
    }

    fn unparse(&self) -> String {
        match self {
            ComparisonExpr::BitwiseOr(expr) => expr.unparse(),
            ComparisonExpr::Comparison {
                left, op, right, ..
            } => {
                format!("{} {} {}", left.unparse(), op.unparse(), right.unparse())
            },
        }
    }
}

impl Parse for ComparisonOp {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        if cursor.consume_symbol(Symbol::EqualEqual).is_ok() {
            Ok(ComparisonOp::Eq)
        } else if cursor.consume_symbol(Symbol::BangEqual).is_ok() {
            Ok(ComparisonOp::Neq)
        } else if cursor.consume_symbol(Symbol::Less).is_ok() {
            Ok(ComparisonOp::Lt)
        } else if cursor.consume_symbol(Symbol::Greater).is_ok() {
            Ok(ComparisonOp::Gt)
        } else if cursor.consume_symbol(Symbol::LessEqual).is_ok() {
            Ok(ComparisonOp::Leq)
        } else if cursor.consume_symbol(Symbol::GreaterEqual).is_ok() {
            Ok(ComparisonOp::Geq)
        } else {
            let Some(token) = cursor.peek() else {
                return cursor.unexpected_end_of_input();
            };
            token.unexpected_token(Some("a comparison operator".to_string()))
        }
    }

    fn unparse(&self) -> String {
        match self {
            ComparisonOp::Eq => "==".to_string(),
            ComparisonOp::Neq => "!=".to_string(),
            ComparisonOp::Lt => "<".to_string(),
            ComparisonOp::Gt => ">".to_string(),
            ComparisonOp::Leq => "<=".to_string(),
            ComparisonOp::Geq => ">=".to_string(),
        }
    }
}

impl Parse for BitwiseOrExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let mut left = BitwiseOrExpr::BitwiseXor(Box::new(BitwiseXorExpr::parse(cursor)?));
        while cursor.consume_symbol(Symbol::Pipe).is_ok() {
            let right = BitwiseXorExpr::parse(cursor)?;
            let span = span.join(cursor.prev_span());
            left = BitwiseOrExpr::BitwiseOr {
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }
        Ok(left)
    }

    fn unparse(&self) -> String {
        match self {
            BitwiseOrExpr::BitwiseXor(expr) => expr.unparse(),
            BitwiseOrExpr::BitwiseOr { left, right, .. } => {
                format!("{} | {}", left.unparse(), right.unparse())
            },
        }
    }
}

impl Parse for BitwiseXorExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let mut left = BitwiseXorExpr::BitwiseAnd(Box::new(BitwiseAndExpr::parse(cursor)?));
        while cursor.consume_symbol(Symbol::Caret).is_ok() {
            let right = BitwiseAndExpr::parse(cursor)?;
            let span = span.join(cursor.prev_span());
            left = BitwiseXorExpr::BitwiseXor {
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }
        Ok(left)
    }

    fn unparse(&self) -> String {
        match self {
            BitwiseXorExpr::BitwiseAnd(expr) => expr.unparse(),
            BitwiseXorExpr::BitwiseXor { left, right, .. } => {
                format!("{} ^ {}", left.unparse(), right.unparse())
            },
        }
    }
}

impl Parse for BitwiseAndExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let mut left = BitwiseAndExpr::Shift(Box::new(ShiftExpr::parse(cursor)?));
        while cursor.consume_symbol(Symbol::And).is_ok() {
            let right = ShiftExpr::parse(cursor)?;
            let span = span.join(cursor.prev_span());
            left = BitwiseAndExpr::BitwiseAnd {
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }
        Ok(left)
    }

    fn unparse(&self) -> String {
        match self {
            BitwiseAndExpr::Shift(expr) => expr.unparse(),
            BitwiseAndExpr::BitwiseAnd { left, right, .. } => {
                format!("{} & {}", left.unparse(), right.unparse())
            },
        }
    }
}

impl Parse for ShiftExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let mut left = ShiftExpr::AddSub(Box::new(AddSubExpr::parse(cursor)?));
        loop {
            if let Ok(op) = ShiftOp::parse(cursor) {
                let right = AddSubExpr::parse(cursor)?;
                let span = span.join(cursor.prev_span());
                left = ShiftExpr::Shift {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                    span,
                };
            } else {
                break Ok(left);
            }
        }
    }

    fn unparse(&self) -> String {
        match self {
            ShiftExpr::AddSub(expr) => expr.unparse(),
            ShiftExpr::Shift {
                left, op, right, ..
            } => {
                format!("{} {} {}", left.unparse(), op.unparse(), right.unparse())
            },
        }
    }
}

impl Parse for ShiftOp {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        if cursor.consume_symbol(Symbol::ShiftLeft).is_ok() {
            Ok(ShiftOp::Left)
        } else if cursor.consume_symbol(Symbol::ShiftRight).is_ok() {
            Ok(ShiftOp::Right)
        } else {
            let Some(token) = cursor.peek() else {
                return cursor.unexpected_end_of_input();
            };
            token.unexpected_token(Some("a shift operator".to_string()))
        }
    }

    fn unparse(&self) -> String {
        match self {
            ShiftOp::Left => "<<".to_string(),
            ShiftOp::Right => ">>".to_string(),
        }
    }
}

impl Parse for AddSubExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let mut left = AddSubExpr::MulDivMod(Box::new(MulDivModExpr::parse(cursor)?));
        loop {
            if let Ok(op) = AddSubOp::parse(cursor) {
                let right = MulDivModExpr::parse(cursor)?;
                let span = span.join(cursor.prev_span());
                left = AddSubExpr::AddSub {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                    span,
                };
            } else {
                break Ok(left);
            }
        }
    }

    fn unparse(&self) -> String {
        match self {
            AddSubExpr::MulDivMod(expr) => expr.unparse(),
            AddSubExpr::AddSub {
                left, op, right, ..
            } => {
                let op_str = match op {
                    AddSubOp::Add => "+",
                    AddSubOp::Sub => "-",
                };
                format!("{} {} {}", left.unparse(), op_str, right.unparse())
            },
        }
    }
}

impl Parse for AddSubOp {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        if cursor.consume_symbol(Symbol::Plus).is_ok() {
            Ok(AddSubOp::Add)
        } else if cursor.consume_symbol(Symbol::Minus).is_ok() {
            Ok(AddSubOp::Sub)
        } else {
            let Some(token) = cursor.peek() else {
                return cursor.unexpected_end_of_input();
            };
            token.unexpected_token(Some("`+` or `-`".to_string()))
        }
    }

    fn unparse(&self) -> String {
        match self {
            AddSubOp::Add => "+".to_string(),
            AddSubOp::Sub => "-".to_string(),
        }
    }
}

impl Parse for MulDivModExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let mut left = MulDivModExpr::Cast(Box::new(CastExpr::parse(cursor)?));
        loop {
            if let Ok(op) = MulDivModOp::parse(cursor) {
                let right = CastExpr::parse(cursor)?;
                let span = span.join(cursor.prev_span());
                left = MulDivModExpr::MulDivMod {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                    span,
                };
            } else {
                break Ok(left);
            }
        }
    }

    fn unparse(&self) -> String {
        match self {
            MulDivModExpr::Cast(expr) => expr.unparse(),
            MulDivModExpr::MulDivMod {
                left, op, right, ..
            } => {
                let op_str = match op {
                    MulDivModOp::Mul => "*",
                    MulDivModOp::Div => "/",
                    MulDivModOp::Mod => "%",
                };
                format!("{} {} {}", left.unparse(), op_str, right.unparse())
            },
        }
    }
}

impl Parse for MulDivModOp {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        if cursor.consume_symbol(Symbol::Star).is_ok() {
            Ok(MulDivModOp::Mul)
        } else if cursor.consume_symbol(Symbol::Slash).is_ok() {
            Ok(MulDivModOp::Div)
        } else if cursor.consume_symbol(Symbol::Percent).is_ok() {
            Ok(MulDivModOp::Mod)
        } else {
            let Some(token) = cursor.peek() else {
                return cursor.unexpected_end_of_input();
            };
            token.unexpected_token(Some("`*`, `/`, or `%`".to_string()))
        }
    }

    fn unparse(&self) -> String {
        match self {
            MulDivModOp::Mul => "*".to_string(),
            MulDivModOp::Div => "/".to_string(),
            MulDivModOp::Mod => "%".to_string(),
        }
    }
}

impl Parse for CastExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let mut unary = CastExpr::Unary(Box::new(UnaryExpr::parse(cursor)?));
        loop {
            if cursor.consume_keyword(Keyword::As).is_ok() {
                let ty = ConcreteType::parse(cursor)?;
                let span = span.join(cursor.prev_span());
                unary = CastExpr::Cast {
                    expr: Box::new(unary),
                    ty,
                    span,
                };
            } else {
                break Ok(unary);
            }
        }
    }

    fn unparse(&self) -> String {
        match self {
            CastExpr::Unary(expr) => expr.unparse(),
            CastExpr::Cast { expr, ty, .. } => format!("{} as {}", expr.unparse(), ty.unparse()),
        }
    }
}

impl Parse for UnaryExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        if cursor.consume_symbol(Symbol::AndAnd).is_ok() {
            let expr = UnaryExpr::parse(cursor)?;
            let span = span.join(cursor.prev_span());
            Ok(UnaryExpr::Unary {
                op: UnaryOp::Ref,
                expr: Box::new(UnaryExpr::Unary {
                    op: UnaryOp::Ref,
                    expr: Box::new(expr),
                    span,
                }),
                span,
            })
        } else if let Ok(op) = UnaryOp::parse(cursor) {
            let expr = UnaryExpr::parse(cursor)?;
            let span = span.join(cursor.prev_span());
            Ok(UnaryExpr::Unary {
                op,
                expr: Box::new(expr),
                span,
            })
        } else {
            FieldAccessExpr::parse(cursor)
                .map(Box::new)
                .map(UnaryExpr::FieldAccess)
        }
    }

    fn unparse(&self) -> String {
        match self {
            UnaryExpr::FieldAccess(expr) => expr.unparse(),
            UnaryExpr::Unary { op, expr, .. } => {
                format!("{}{}", op.unparse(), expr.unparse())
            },
        }
    }
}

impl Parse for UnaryOp {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        if cursor.consume_symbol(Symbol::Minus).is_ok() {
            Ok(UnaryOp::Neg)
        } else if cursor.consume_symbol(Symbol::Bang).is_ok() {
            Ok(UnaryOp::Not)
        } else if cursor.consume_symbol(Symbol::And).is_ok() {
            Ok(UnaryOp::Ref)
        } else {
            let Some(token) = cursor.peek() else {
                return cursor.unexpected_end_of_input();
            };
            token.unexpected_token(Some("a unary operator".to_string()))
        }
    }

    fn unparse(&self) -> String {
        match self {
            UnaryOp::Neg => "-".to_string(),
            UnaryOp::Not => "!".to_string(),
            UnaryOp::Ref => "&".to_string(),
        }
    }
}

impl Parse for FieldAccessExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let mut expr = FieldAccessExpr::Primary(Box::new(PrimaryExpr::parse(cursor)?));
        loop {
            if cursor.consume_symbol(Symbol::OpenBracket).is_ok() {
                // indexing
                let index = Expr::parse(cursor)?;
                cursor.consume_symbol(Symbol::CloseBracket)?;
                let span = span.join(cursor.prev_span());
                expr = FieldAccessExpr::Index {
                    array: Box::new(expr),
                    index: Box::new(index),
                    span,
                };
            } else if cursor.consume_symbol(Symbol::OpenParen).is_ok() {
                // function call
                let mut args = Vec::new();
                while cursor.consume_symbol(Symbol::CloseParen).is_err() {
                    let arg = Expr::parse(cursor)?;
                    args.push(arg);
                    if cursor.consume_symbol(Symbol::Comma).is_ok() {
                        continue;
                    } else if cursor.consume_symbol(Symbol::CloseParen).is_ok() {
                        break;
                    } else {
                        return if cursor.is_eof() {
                            cursor.unexpected_end_of_input()
                        } else {
                            Err(CompileError::UnexpectedToken(
                                cursor.peek().unwrap().span(),
                                Some("`,` or `)`".to_string()),
                            ))
                        };
                    }
                }
                let span = span.join(cursor.prev_span());
                expr = FieldAccessExpr::FunctionCall {
                    func: Box::new(expr),
                    args,
                    span,
                };
            } else if cursor.consume_symbol(Symbol::Dot).is_ok() {
                // method call or field access
                let method_or_field = cursor.consume_ident()?;
                if cursor.consume_symbol(Symbol::OpenParen).is_ok() {
                    // method call
                    let mut args = Vec::new();
                    while cursor.consume_symbol(Symbol::CloseParen).is_err() {
                        let arg = Expr::parse(cursor)?;
                        args.push(arg);
                        if cursor.consume_symbol(Symbol::Comma).is_ok() {
                            continue;
                        } else if cursor.consume_symbol(Symbol::CloseParen).is_ok() {
                            break;
                        } else {
                            return if cursor.is_eof() {
                                cursor.unexpected_end_of_input()
                            } else {
                                Err(CompileError::UnexpectedToken(
                                    cursor.peek().unwrap().span(),
                                    Some("`,` or `)`".to_string()),
                                ))
                            };
                        }
                    }
                    let span = span.join(cursor.prev_span());
                    expr = FieldAccessExpr::MethodCall {
                        expr: Box::new(expr),
                        method: method_or_field.clone(),
                        args,
                        span,
                    };
                } else {
                    // field access
                    let span = span.join(cursor.prev_span());
                    expr = FieldAccessExpr::FieldAccess {
                        expr: Box::new(expr),
                        field: method_or_field.clone(),
                        span,
                    };
                }
            } else {
                break Ok(expr);
            }
        }
    }

    fn unparse(&self) -> String {
        match self {
            FieldAccessExpr::Index { array, index, .. } => {
                format!("{}[{}]", array.unparse(), index.unparse())
            },
            FieldAccessExpr::FunctionCall { func, args, .. } => {
                let args = args
                    .iter()
                    .map(|a| a.unparse())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", func.unparse(), args)
            },
            FieldAccessExpr::MethodCall {
                expr, method, args, ..
            } => {
                let args = args
                    .iter()
                    .map(|a| a.unparse())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}.{}({})", expr.unparse(), method.value, args)
            },
            FieldAccessExpr::FieldAccess { expr, field, .. } => {
                format!("{}.{}", expr.unparse(), field.value)
            },
            FieldAccessExpr::Primary(expr) => expr.unparse(),
        }
    }
}

impl Parse for PrimaryExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let mut fork = cursor.fork();
        // if we have a label, must be loop, for, or while
        if Label::parse(&mut fork).is_ok() {
            if fork.consume_keyword(Keyword::Loop).is_ok() {
                LoopExpr::parse(cursor).map(PrimaryExpr::Loop)
            } else if fork.consume_keyword(Keyword::For).is_ok() {
                ForExpr::parse(cursor).map(PrimaryExpr::For)
            } else if fork.consume_keyword(Keyword::While).is_ok() {
                WhileExpr::parse(cursor).map(PrimaryExpr::While)
            } else if let Some(token) = cursor.peek() {
                token.unexpected_token(Some("an expression".to_string()))
            } else {
                cursor.unexpected_end_of_input()
            }
        } else if fork.consume_keyword(Keyword::If).is_ok() {
            IfExpr::parse(cursor).map(PrimaryExpr::If)
        } else if fork.consume_keyword(Keyword::Match).is_ok() {
            MatchExpr::parse(cursor).map(PrimaryExpr::Match)
        } else if fork.consume_keyword(Keyword::Loop).is_ok() {
            LoopExpr::parse(cursor).map(PrimaryExpr::Loop)
        } else if fork.consume_keyword(Keyword::For).is_ok() {
            ForExpr::parse(cursor).map(PrimaryExpr::For)
        } else if fork.consume_keyword(Keyword::While).is_ok() {
            WhileExpr::parse(cursor).map(PrimaryExpr::While)
        } else if fork.consume_symbol(Symbol::OpenBrace).is_ok() {
            Block::parse(cursor).map(PrimaryExpr::Block)
        } else if fork.consume_keyword(Keyword::New).is_ok() {
            // struct expression
            StructExpr::parse(cursor).map(PrimaryExpr::Struct)
        } else if fork.consume_symbol(Symbol::OpenParen).is_ok() {
            // group expression
            cursor.consume_symbol(Symbol::OpenParen)?;
            if cursor.consume_symbol(Symbol::CloseParen).is_ok() {
                let span = span.join(cursor.prev_span());
                return Ok(PrimaryExpr::Unit(span));
            }
            let expr = Expr::parse(cursor)?;
            // if we have a comma, it's a tuple
            if cursor.consume_symbol(Symbol::Comma).is_ok() {
                let mut elems = vec![expr];
                while cursor.consume_symbol(Symbol::CloseParen).is_err() {
                    let expr = Expr::parse(cursor)?;
                    elems.push(expr);
                    if cursor.consume_symbol(Symbol::Comma).is_ok() {
                        continue;
                    } else if cursor.consume_symbol(Symbol::CloseParen).is_ok() {
                        break;
                    } else {
                        return if cursor.is_eof() {
                            cursor.unexpected_end_of_input()
                        } else {
                            Err(CompileError::UnexpectedToken(
                                cursor.peek().unwrap().span(),
                                Some("`,` or `)`".to_string()),
                            ))
                        };
                    }
                }
                let span = span.join(cursor.prev_span());
                return Ok(PrimaryExpr::Tuple { elems, span });
            }
            cursor.consume_symbol(Symbol::CloseParen)?;
            let span = span.join(cursor.prev_span());
            Ok(PrimaryExpr::Group {
                expr: Box::new(expr),
                span,
            })
        } else if fork.consume_symbol(Symbol::OpenBracket).is_ok() {
            // array literal
            cursor.consume_symbol(Symbol::OpenBracket)?;
            let mut elems = Vec::new();
            while cursor.consume_symbol(Symbol::CloseBracket).is_err() {
                let expr = Expr::parse(cursor)?;
                elems.push(expr);
                if cursor.consume_symbol(Symbol::Comma).is_ok() {
                    continue;
                } else if cursor.consume_symbol(Symbol::CloseBracket).is_ok() {
                    break;
                } else {
                    return if cursor.is_eof() {
                        cursor.unexpected_end_of_input()
                    } else {
                        Err(CompileError::UnexpectedToken(
                            cursor.peek().unwrap().span(),
                            Some("`,` or `]`".to_string()),
                        ))
                    };
                }
            }
            let span = span.join(cursor.prev_span());
            Ok(PrimaryExpr::Array { elems, span })
        } else if let Ok(lit) = cursor.consume_literal() {
            Ok(PrimaryExpr::Literal(lit.clone()))
        } else if let Ok(path) = Path::parse(cursor) {
            Ok(PrimaryExpr::Path(path))
        } else if let Some(token) = cursor.peek() {
            token.unexpected_token(Some("an expression".to_string()))
        } else {
            cursor.unexpected_end_of_input()
        }
    }

    fn unparse(&self) -> String {
        match self {
            PrimaryExpr::Literal(lit) => {
                match lit {
                    TokenLiteral::Int(s) => {
                        let radix = match s.radix {
                            Radix::Decimal => "",
                            Radix::Hex => "0x",
                            Radix::Octal => "0o",
                            Radix::Binary => "0b",
                        };
                        format!("{}{}{}", radix, s.value, s.suffix.unwrap_or(""))
                    },
                    TokenLiteral::Float(s) => {
                        format!("{}{}", s.value, s.suffix.unwrap_or(""))
                    },
                    TokenLiteral::String(s) => format!("\"{}\"", s.value),
                    TokenLiteral::Char(s) => format!("'{}'", s.value),
                    TokenLiteral::Bool(s) => s.value.to_string(),
                    TokenLiteral::Nil(_) => "nil".to_string(),
                }
            },
            PrimaryExpr::Path(path) => path.unparse(),
            PrimaryExpr::Group { expr, .. } => format!("({})", expr.unparse()),
            PrimaryExpr::Block(block) => block.unparse(),
            PrimaryExpr::If(if_expr) => if_expr.unparse(),
            PrimaryExpr::Match(match_expr) => match_expr.unparse(),
            PrimaryExpr::Loop(loop_expr) => loop_expr.unparse(),
            PrimaryExpr::For(for_expr) => for_expr.unparse(),
            PrimaryExpr::While(while_expr) => while_expr.unparse(),
            PrimaryExpr::Tuple { elems, .. } => {
                if elems.len() == 1 {
                    return format!("({},)", elems[0].unparse());
                }
                let elems = elems
                    .iter()
                    .map(|e| e.unparse())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", elems)
            },
            PrimaryExpr::Array { elems, .. } => {
                let elems = elems
                    .iter()
                    .map(|e| e.unparse())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{}]", elems)
            },
            PrimaryExpr::Struct(expr) => expr.unparse(),
            PrimaryExpr::Unit(_) => "()".to_string(),
        }
    }
}

impl Parse for StructExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        cursor.consume_keyword(Keyword::New)?;
        let path = Path::parse(cursor)?;
        if cursor.consume_symbol(Symbol::OpenBrace).is_ok() {
            // named struct
            let mut fields = Vec::new();
            while cursor.consume_symbol(Symbol::CloseBrace).is_err() {
                if cursor.consume_symbol(Symbol::DotDot).is_ok() {
                    // rest
                    let rest = Expr::parse(cursor)?;
                    cursor.consume_symbol(Symbol::CloseBrace)?;
                    let span = span.join(cursor.prev_span());
                    return Ok(StructExpr::Named {
                        path,
                        fields,
                        rest: Some(rest),
                        span,
                    });
                }
                fields.push(StructFieldExpr::parse(cursor)?);
                if cursor.consume_symbol(Symbol::Comma).is_ok() {
                    continue;
                } else if cursor.consume_symbol(Symbol::CloseBrace).is_ok() {
                    break;
                } else {
                    return if cursor.is_eof() {
                        cursor.unexpected_end_of_input()
                    } else {
                        Err(CompileError::UnexpectedToken(
                            cursor.peek().unwrap().span(),
                            Some("`,` or `}`".to_string()),
                        ))
                    };
                }
            }
            let span = span.join(cursor.prev_span());
            Ok(StructExpr::Named {
                path,
                fields,
                rest: None,
                span,
            })
        } else if cursor.consume_symbol(Symbol::OpenParen).is_ok() {
            // tuple struct
            let mut fields = Vec::new();
            while cursor.consume_symbol(Symbol::CloseParen).is_err() {
                let expr = Expr::parse(cursor)?;
                fields.push(expr);
                if cursor.consume_symbol(Symbol::Comma).is_ok() {
                    continue;
                } else if cursor.consume_symbol(Symbol::CloseParen).is_ok() {
                    break;
                } else {
                    return if cursor.is_eof() {
                        cursor.unexpected_end_of_input()
                    } else {
                        Err(CompileError::UnexpectedToken(
                            cursor.peek().unwrap().span(),
                            Some("`,` or `)`".to_string()),
                        ))
                    };
                }
            }
            let span = span.join(cursor.prev_span());
            Ok(StructExpr::Unnamed { path, fields, span })
        } else {
            // unit struct
            let span = span.join(cursor.prev_span());
            Ok(StructExpr::Unit { path, span })
        }
    }

    fn unparse(&self) -> String {
        match self {
            StructExpr::Named {
                path, fields, rest, ..
            } => {
                let fields = fields
                    .iter()
                    .map(|f| f.unparse())
                    .collect::<Vec<_>>()
                    .join(", ");
                if let Some(rest) = rest {
                    if fields.is_empty() {
                        format!("new {} {{ ..{} }}", path.unparse(), rest.unparse())
                    } else {
                        format!(
                            "new {} {{ {}, ..{} }}",
                            path.unparse(),
                            fields,
                            rest.unparse()
                        )
                    }
                } else if fields.is_empty() {
                    format!("new {} {{}}", path.unparse())
                } else {
                    format!("new {} {{ {} }}", path.unparse(), fields)
                }
            },
            StructExpr::Unnamed { path, fields, .. } => {
                let fields = fields
                    .iter()
                    .map(|e| e.unparse())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("new {}({})", path.unparse(), fields)
            },
            StructExpr::Unit { path, .. } => {
                format!("new {}", path.unparse())
            },
        }
    }
}

impl Parse for StructFieldExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let name = cursor.consume_ident()?;
        if cursor.consume_symbol(Symbol::Colon).is_ok() {
            let expr = Some(Expr::parse(cursor)?);
            let span = span.join(cursor.prev_span());
            Ok(StructFieldExpr {
                name: name.clone(),
                expr,
                span,
            })
        } else {
            let span = span.join(cursor.prev_span());
            Ok(StructFieldExpr {
                name: name.clone(),
                expr: None,
                span,
            })
        }
    }

    fn unparse(&self) -> String {
        if let Some(expr) = &self.expr {
            format!("{}: {}", self.name.value, expr.unparse())
        } else {
            self.name.value.to_string()
        }
    }
}

impl Parse for IfExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        cursor.consume_keyword(Keyword::If)?;
        let cond = Expr::parse(cursor)?;
        let then = Block::parse(cursor)?;
        let otherwise = if cursor.consume_keyword(Keyword::Else).is_ok() {
            Some(ElseExpr::parse(cursor)?)
        } else {
            None
        };
        let span = span.join(cursor.prev_span());
        Ok(IfExpr {
            cond: Box::new(cond),
            then,
            otherwise,
            span,
        })
    }

    fn unparse(&self) -> String {
        let mut result = format!("if {} {}", self.cond.unparse(), self.then.unparse());
        if let Some(otherwise) = &self.otherwise {
            result.push_str(&format!(" else {}", otherwise.unparse()));
        }
        result
    }
}

impl Parse for ElseExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let mut fork = cursor.fork();
        if fork.consume_keyword(Keyword::If).is_ok() {
            IfExpr::parse(cursor).map(Box::new).map(ElseExpr::If)
        } else if fork.consume_symbol(Symbol::OpenBrace).is_ok() {
            Block::parse(cursor).map(ElseExpr::Block)
        } else if let Some(token) = cursor.peek() {
            token.unexpected_token(Some("`if` or `{`".to_string()))
        } else {
            cursor.unexpected_end_of_input()
        }
    }

    fn unparse(&self) -> String {
        match self {
            ElseExpr::If(if_expr) => if_expr.unparse(),
            ElseExpr::Block(block) => block.unparse(),
        }
    }
}

impl Parse for MatchExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        cursor.consume_keyword(Keyword::Match)?;
        let expr = Expr::parse(cursor)?;
        cursor.consume_symbol(Symbol::OpenBrace)?;
        let mut arms = Vec::new();
        if cursor.consume_symbol(Symbol::CloseBrace).is_err() {
            loop {
                let arm = MatchArm::parse(cursor)?;
                let is_block = matches!(arm, MatchArm::Block { .. });
                arms.push(arm);
                if cursor.consume_symbol(Symbol::Comma).is_ok() {
                    continue;
                } else if cursor.consume_symbol(Symbol::CloseBrace).is_ok() {
                    break;
                } else if is_block {
                    // we don't require a comma after a block arm
                    continue;
                } else {
                    return if cursor.is_eof() {
                        cursor.unexpected_end_of_input()
                    } else {
                        Err(CompileError::UnexpectedToken(
                            cursor.peek().unwrap().span(),
                            Some("`,` or `}`".to_string()),
                        ))
                    };
                }
            }
        }
        let span = span.join(cursor.prev_span());
        Ok(MatchExpr {
            expr: Box::new(expr),
            arms,
            span,
        })
    }

    fn unparse(&self) -> String {
        let arms = self
            .arms
            .iter()
            .map(|arm| arm.unparse())
            .collect::<Vec<_>>()
            .join(", ");
        format!("match {} {{ {} }}", self.expr.unparse(), arms)
    }
}

impl Parse for MatchArm {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let pattern = Pattern::parse(cursor)?;
        let guard = if cursor.consume_keyword(Keyword::If).is_ok() {
            Some(Expr::parse(cursor)?)
        } else {
            None
        };
        cursor.consume_symbol(Symbol::FatArrow)?;
        if cursor.fork().consume_symbol(Symbol::OpenBrace).is_ok() {
            // block arm
            let block = Block::parse(cursor)?;
            let span = span.join(cursor.prev_span());
            Ok(MatchArm::Block {
                pattern,
                guard,
                block,
                span,
            })
        } else {
            // expression arm
            let expr = Expr::parse(cursor)?;
            let span = span.join(cursor.prev_span());
            Ok(MatchArm::Expr {
                pattern,
                guard,
                expr,
                span,
            })
        }
    }

    fn unparse(&self) -> String {
        match self {
            MatchArm::Block {
                pattern,
                guard,
                block,
                ..
            } => {
                let guard_str = if let Some(guard) = guard {
                    format!(" if {}", guard.unparse())
                } else {
                    "".to_string()
                };
                format!("{}{} => {}", pattern.unparse(), guard_str, block.unparse())
            },
            MatchArm::Expr {
                pattern,
                guard,
                expr,
                ..
            } => {
                let guard_str = if let Some(guard) = guard {
                    format!(" if {}", guard.unparse())
                } else {
                    "".to_string()
                };
                format!("{}{} => {}", pattern.unparse(), guard_str, expr.unparse())
            },
        }
    }
}

impl Parse for LoopExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let label = Label::parse(cursor).ok();
        if label.is_some() {
            // if we have a label, must be followed by a colon
            cursor.consume_symbol(Symbol::Colon)?;
        }
        cursor.consume_keyword(Keyword::Loop)?;
        let body = Block::parse(cursor)?;
        let span = span.join(cursor.prev_span());
        Ok(LoopExpr { label, body, span })
    }

    fn unparse(&self) -> String {
        let label_str = if let Some(label) = &self.label {
            format!("{}: ", label.unparse())
        } else {
            "".to_string()
        };
        format!("{}loop {}", label_str, self.body.unparse())
    }
}

impl Parse for ForExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let label = Label::parse(cursor).ok();
        if label.is_some() {
            // if we have a label, must be followed by a colon
            cursor.consume_symbol(Symbol::Colon)?;
        }
        cursor.consume_keyword(Keyword::For)?;
        let pattern = Pattern::parse(cursor)?;
        cursor.consume_keyword(Keyword::In)?;
        let expr = Expr::parse(cursor)?;
        let body = Block::parse(cursor)?;
        let span = span.join(cursor.prev_span());
        Ok(ForExpr {
            label,
            pattern,
            expr: Box::new(expr),
            body,
            span,
        })
    }

    fn unparse(&self) -> String {
        let label_str = if let Some(label) = &self.label {
            format!("{}: ", label.unparse())
        } else {
            "".to_string()
        };
        format!(
            "{}for {} in {} {}",
            label_str,
            self.pattern.unparse(),
            self.expr.unparse(),
            self.body.unparse()
        )
    }
}

impl Parse for WhileExpr {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        let label = Label::parse(cursor).ok();
        if label.is_some() {
            // if we have a label, must be followed by a colon
            cursor.consume_symbol(Symbol::Colon)?;
        }
        cursor.consume_keyword(Keyword::While)?;
        let cond = Expr::parse(cursor)?;
        let body = Block::parse(cursor)?;
        let span = span.join(cursor.prev_span());
        Ok(WhileExpr {
            label,
            cond: Box::new(cond),
            body,
            span,
        })
    }

    fn unparse(&self) -> String {
        let label_str = if let Some(label) = &self.label {
            format!("{}: ", label.unparse())
        } else {
            "".to_string()
        };
        format!(
            "{}while {} {}",
            label_str,
            self.cond.unparse(),
            self.body.unparse()
        )
    }
}

impl Parse for Label {
    fn parse(cursor: &mut Cursor) -> Result<Self> {
        let Some(span) = cursor.peek().map(|t| t.span()) else {
            return cursor.unexpected_end_of_input();
        };
        if cursor.consume_symbol(Symbol::Dollar).is_ok() {
            let name = cursor.consume_ident()?.clone();
            let span = span.join(cursor.prev_span());
            Ok(Label { name, span })
        } else {
            let Some(token) = cursor.peek() else {
                return cursor.unexpected_end_of_input();
            };
            token.unexpected_token(Some("a label".to_string()))
        }
    }

    fn unparse(&self) -> String {
        format!("${}", self.name.value)
    }
}

#[cfg(test)]
#[path = "test_parser.rs"]
mod tests;
