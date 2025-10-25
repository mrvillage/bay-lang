use crate::prelude::*;

#[derive(Debug, Clone)]
pub struct Source {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub enum Item {
    Use(UseItem),
    Fn(FnItem),
    Struct(StructItem),
    Enum(EnumItem),
    // Impl(ImplItem),
    Const(ConstItem),
    // Static(StaticItem),
    // Trait(TraitItem),
    // TypeAlias(TypeAliasItem),
    Mod(ModItem),
}

#[derive(Debug, Clone)]
pub struct UseItem {
    pub visibility: Option<Visibility>,
    pub tree:       UseTree,
    pub span:       Span,
}

#[derive(Debug, Clone)]
pub enum UseTree {
    Root {
        tree: Box<UseTree>,
    },
    Pkg {
        tree: Box<UseTree>,
    },
    Path {
        ident: token::Ident,
        tree:  Box<UseTree>,
    },
    Name {
        ident: token::Ident,
    },
    Rename {
        ident: token::Ident,
        alias: token::Ident,
    },
    Glob,
    Group {
        items: Vec<UseTree>,
    },
}

#[derive(Debug, Clone)]
pub struct FnItem {
    pub visibility: Option<Visibility>,
    pub name:       token::Ident,
    pub params:     Vec<FnParam>,
    pub ret_ty:     Option<ConcreteType>,
    pub body:       Block,
    pub span:       Span,
}

#[derive(Debug, Clone)]
pub struct FnParam {
    pub name: token::Ident,
    pub ty:   ConcreteType,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructItem {
    pub visibility: Option<Visibility>,
    pub name:       token::Ident,
    pub fields:     StructFields,
    pub span:       Span,
}

#[derive(Debug, Clone)]
pub enum StructFields {
    Named { fields: Vec<NamedStructField> },
    Unnamed { fields: Vec<UnnamedStructField> },
    Unit,
}

#[derive(Debug, Clone)]
pub struct NamedStructField {
    pub visibility: Option<Visibility>,
    pub name:       token::Ident,
    pub ty:         ConcreteType,
    pub span:       Span,
}

#[derive(Debug, Clone)]
pub struct UnnamedStructField {
    pub visibility: Option<Visibility>,
    pub ty:         ConcreteType,
    pub span:       Span,
}

#[derive(Debug, Clone)]
pub struct EnumItem {
    pub visibility: Option<Visibility>,
    pub name:       token::Ident,
    pub variants:   Vec<EnumVariant>,
    pub span:       Span,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name:   token::Ident,
    pub fields: StructFields,
    pub disc:   Option<token::IntLiteral>,
    pub span:   Span,
}

#[derive(Debug, Clone)]
pub struct ConstItem {
    pub visibility: Option<Visibility>,
    pub name:       token::Ident,
    pub ty:         ConcreteType,
    pub expr:       Expr,
    pub span:       Span,
}

#[derive(Debug, Clone)]
pub struct ModItem {
    pub visibility: Option<Visibility>,
    pub name:       token::Ident,
    pub items:      Vec<Item>,
    pub span:       Span,
    pub inline:     bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone)]
pub struct Path {
    pub root:     bool,
    pub pkg:      bool,
    pub segments: Vec<token::Ident>,
    pub span:     Span,
}

#[derive(Debug, Clone)]
pub enum ConcreteType {
    Path(Path),
    Tuple(Vec<ConcreteType>),
    Array {
        ty:  Box<ConcreteType>,
        len: token::IntLiteral,
    },
    Slice(Box<ConcreteType>),
    Ref(Box<ConcreteType>),
    Optional(Box<ConcreteType>),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span:  Span,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let(LetStmt),
    Expr(Expr),
    Semi(Expr),
    Item(Item),
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub pattern:    IdentPattern,
    pub ty:         Option<ConcreteType>,
    pub expr:       Option<Expr>,
    pub else_block: Option<Block>,
    pub span:       Span,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Path(Path),
    Tuple(Vec<Pattern>),
}

#[derive(Debug, Clone)]
pub enum IdentPattern {
    Wildcard,
    Ident(token::Ident),
    Tuple(Vec<IdentPattern>),
}

#[derive(Debug, Clone)]
pub enum FieldPattern {
    Wildcard,
    // something like a.b.c
    Field(Vec<token::Ident>),
    Tuple(Vec<IdentPattern>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    ControlFlow {
        kind:  ControlFlowKind,
        label: Option<Label>,
        expr:  Option<Box<RangeExpr>>,
        span:  Span,
    },
    Assign(Box<AssignExpr>),
}

#[derive(Debug, Clone)]
pub enum AssignExpr {
    Assign {
        pattern: FieldPattern,
        op:      AssignOp,
        expr:    Box<RangeExpr>,
        span:    Span,
    },
    Range(Box<RangeExpr>),
}

#[derive(Debug, Clone)]
pub enum RangeExpr {
    Range {
        // requires parentheses to chain, as such both arguments must be the
        // next tier
        start:     Option<Box<LogicalOrExpr>>,
        end:       Option<Box<LogicalOrExpr>>,
        inclusive: bool,
        span:      Span,
    },
    LogicalOr(Box<LogicalOrExpr>),
}

#[derive(Debug, Clone)]
pub enum LogicalOrExpr {
    LogicalOr {
        left:  Box<LogicalOrExpr>,
        right: Box<LogicalAndExpr>,
        span:  Span,
    },
    LogicalAnd(Box<LogicalAndExpr>),
}

#[derive(Debug, Clone)]
pub enum LogicalAndExpr {
    LogicalAnd {
        left:  Box<LogicalAndExpr>,
        right: Box<ComparisonExpr>,
        span:  Span,
    },
    Comparison(Box<ComparisonExpr>),
}

#[derive(Debug, Clone)]
pub enum ComparisonExpr {
    Comparison {
        // requires parentheses to chain, as such both arguments must be the
        // next tier
        left:  Box<BitwiseOrExpr>,
        op:    ComparisonOp,
        right: Box<BitwiseOrExpr>,
        span:  Span,
    },
    BitwiseOr(Box<BitwiseOrExpr>),
}

#[derive(Debug, Clone)]
pub enum BitwiseOrExpr {
    BitwiseOr {
        left:  Box<BitwiseOrExpr>,
        right: Box<BitwiseXorExpr>,
        span:  Span,
    },
    BitwiseXor(Box<BitwiseXorExpr>),
}

#[derive(Debug, Clone)]
pub enum BitwiseXorExpr {
    BitwiseXor {
        left:  Box<BitwiseXorExpr>,
        right: Box<BitwiseAndExpr>,
        span:  Span,
    },
    BitwiseAnd(Box<BitwiseAndExpr>),
}

#[derive(Debug, Clone)]
pub enum BitwiseAndExpr {
    BitwiseAnd {
        left:  Box<BitwiseAndExpr>,
        right: Box<ShiftExpr>,
        span:  Span,
    },
    Shift(Box<ShiftExpr>),
}

#[derive(Debug, Clone)]
pub enum ShiftExpr {
    Shift {
        left:  Box<ShiftExpr>,
        op:    ShiftOp,
        right: Box<AddSubExpr>,
        span:  Span,
    },
    AddSub(Box<AddSubExpr>),
}

#[derive(Debug, Clone)]
pub enum AddSubExpr {
    AddSub {
        left:  Box<AddSubExpr>,
        op:    AddSubOp,
        right: Box<MulDivModExpr>,
        span:  Span,
    },
    MulDivMod(Box<MulDivModExpr>),
}

#[derive(Debug, Clone)]
pub enum MulDivModExpr {
    MulDivMod {
        left:  Box<MulDivModExpr>,
        op:    MulDivModOp,
        right: Box<CastExpr>,
        span:  Span,
    },
    Cast(Box<CastExpr>),
}

#[derive(Debug, Clone)]
pub enum CastExpr {
    Cast {
        expr: Box<CastExpr>,
        ty:   ConcreteType,
        span: Span,
    },
    Unary(Box<UnaryExpr>),
}

#[derive(Debug, Clone)]
pub enum UnaryExpr {
    Unary {
        op:   UnaryOp,
        expr: Box<UnaryExpr>,
        span: Span,
    },
    FieldAccess(Box<FieldAccessExpr>),
}

#[derive(Debug, Clone)]
pub enum FieldAccessExpr {
    Index {
        array: Box<FieldAccessExpr>,
        index: Box<Expr>,
        span:  Span,
    },
    FunctionCall {
        func: Box<FieldAccessExpr>,
        args: Vec<Expr>,
        span: Span,
    },
    FieldAccess {
        expr:  Box<FieldAccessExpr>,
        field: token::Ident,
        span:  Span,
    },
    MethodCall {
        expr:   Box<FieldAccessExpr>,
        method: token::Ident,
        args:   Vec<Expr>,
        span:   Span,
    },
    Primary(Box<PrimaryExpr>),
}

#[derive(Debug, Clone)]
pub enum PrimaryExpr {
    Literal(token::TokenLiteral),
    Path(Path),
    Group { expr: Box<Expr>, span: Span },
    Tuple { elems: Vec<Expr>, span: Span },
    Array { elems: Vec<Expr>, span: Span },
    Struct(StructExpr),
    Block(Block),
    If(IfExpr),
    Match(MatchExpr),
    Loop(LoopExpr),
    For(ForExpr),
    While(WhileExpr),
    Unit(Span),
}

#[derive(Debug, Clone)]
pub enum StructExpr {
    Named {
        path:   Path,
        fields: Vec<StructFieldExpr>,
        rest:   Option<Expr>,
        span:   Span,
    },
    Unnamed {
        path:   Path,
        fields: Vec<Expr>,
        span:   Span,
    },
    Unit {
        path: Path,
        span: Span,
    },
}

#[derive(Debug, Clone)]
pub struct StructFieldExpr {
    pub name: token::Ident,
    pub expr: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub cond:      Box<Expr>,
    pub then:      Block,
    pub otherwise: Option<ElseExpr>,
    pub span:      Span,
}

#[derive(Debug, Clone)]
pub enum ElseExpr {
    If(Box<IfExpr>),
    Block(Block),
}

#[derive(Debug, Clone)]
pub struct MatchExpr {
    pub expr: Box<Expr>,
    pub arms: Vec<MatchArm>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum MatchArm {
    Block {
        pattern: Pattern,
        guard:   Option<Expr>,
        block:   Block,
        span:    Span,
    },
    Expr {
        pattern: Pattern,
        guard:   Option<Expr>,
        expr:    Expr,
        span:    Span,
    },
}

#[derive(Debug, Clone)]
pub struct LoopExpr {
    pub label: Option<Label>,
    pub body:  Block,
    pub span:  Span,
}

#[derive(Debug, Clone)]
pub struct ForExpr {
    pub label:   Option<Label>,
    pub pattern: Pattern,
    pub expr:    Box<Expr>,
    pub body:    Block,
    pub span:    Span,
}

#[derive(Debug, Clone)]
pub struct WhileExpr {
    pub label: Option<Label>,
    pub cond:  Box<Expr>,
    pub body:  Block,
    pub span:  Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label {
    pub name: token::Ident,
    pub span: Span,
}

impl std::hash::Hash for Label {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.value.hash(state);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ControlFlowKind {
    Return,
    Break,
    Continue,
}

#[derive(Debug, Clone, Copy)]
pub enum AssignOp {
    Assign,       // =
    AddAssign,    // +=
    SubAssign,    // -=
    MulAssign,    // *=
    DivAssign,    // /=
    ModAssign,    // %=
    BitAndAssign, // &=
    BitOrAssign,  // |=
    BitXorAssign, // ^=
    BitShlAssign, // <<=
    BitShrAssign, // >>=
}

#[derive(Debug, Clone, Copy)]
pub enum ComparisonOp {
    Eq,  // ==
    Neq, // !=
    Lt,  // <
    Gt,  // >
    Leq, // <=
    Geq, // >=
}

#[derive(Debug, Clone, Copy)]
pub enum ShiftOp {
    Left,  // <<
    Right, // >>
}

#[derive(Debug, Clone, Copy)]
pub enum AddSubOp {
    Add, // +
    Sub, // -
}

#[derive(Debug, Clone, Copy)]
pub enum MulDivModOp {
    Mul, // *
    Div, // /
    Mod, // %
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg, // -
    Not, // !
    Ref, // &
}

impl Expr {
    // this is a war crime
    pub fn is_block_expr(&self) -> bool {
        if let Expr::Assign(assign) = self {
            if let AssignExpr::Range(range) = &**assign {
                if let RangeExpr::LogicalOr(logical_or) = &**range {
                    if let LogicalOrExpr::LogicalAnd(logical_and) = &**logical_or {
                        if let LogicalAndExpr::Comparison(comparison) = &**logical_and {
                            if let ComparisonExpr::BitwiseOr(bitwise_or) = &**comparison {
                                if let BitwiseOrExpr::BitwiseXor(bitwise_xor) = &**bitwise_or {
                                    if let BitwiseXorExpr::BitwiseAnd(bitwise_and) = &**bitwise_xor
                                    {
                                        if let BitwiseAndExpr::Shift(shift) = &**bitwise_and {
                                            if let ShiftExpr::AddSub(add_sub) = &**shift {
                                                if let AddSubExpr::MulDivMod(mul_div_mod) =
                                                    &**add_sub
                                                {
                                                    if let MulDivModExpr::Cast(cast) =
                                                        &**mul_div_mod
                                                    {
                                                        if let CastExpr::Unary(unary) = &**cast {
                                                            if let UnaryExpr::FieldAccess(
                                                                field_access,
                                                            ) = &**unary
                                                            {
                                                                if let FieldAccessExpr::Primary(
                                                                    primary,
                                                                ) = &**field_access
                                                                {
                                                                    if let PrimaryExpr::Block(_)
                                                                    | PrimaryExpr::If(_)
                                                                    | PrimaryExpr::Match(_)
                                                                    | PrimaryExpr::Loop(_)
                                                                    | PrimaryExpr::For(_)
                                                                    | PrimaryExpr::While(_) =
                                                                        &**primary
                                                                    {
                                                                        return true;
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        false
    }
}
