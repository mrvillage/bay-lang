#![allow(unused)]
use std::{
    collections::{HashMap, HashSet},
    fmt::Binary,
    io::Write,
};

use crate::{
    concrete::{UseTree, Visibility},
    prelude::*,
};

#[derive(Debug)]
pub struct Hir {
    pub items: Vec<HirItem>,
}

#[derive(Debug)]
pub enum HirItem {
    Use {
        visibility: Visibility,
        tree:       UseTree,
        span:       Span,
    },
    Fn {
        visibility: Visibility,
        name:       token::Ident,
        params:     Vec<(token::Ident, MaybeType)>,
        ret_type:   MaybeType,
        body:       Block,
        span:       Span,
    },
    Struct {
        visibility: Visibility,
        name:       token::Ident,
        fields:     StructFields,
        type_id:    MaybeType,
        span:       Span,
    },
    Enum {
        visibility: Visibility,
        name:       token::Ident,
        variants:   Vec<EnumVariant>,
        type_id:    MaybeType,
        span:       Span,
    },
    Const {
        visibility: Visibility,
        name:       token::Ident,
        ty:         MaybeType,
        value:      Box<Expr>,
        span:       Span,
    },
    Mod {
        visibility: Visibility,
        name:       token::Ident,
        items:      Vec<HirItem>,
        span:       Span,
    },
}

#[derive(Debug)]
pub enum StructFields {
    Named { fields: Vec<NamedStructField> },
    Unnamed { fields: Vec<UnnamedStructField> },
    Unit,
}

#[derive(Debug)]
pub struct NamedStructField {
    pub visibility: Visibility,
    pub name:       token::Ident,
    pub ty:         MaybeType,
    pub span:       Span,
}

#[derive(Debug)]
pub struct UnnamedStructField {
    pub visibility: Visibility,
    pub ty:         MaybeType,
    pub span:       Span,
}

#[derive(Debug)]
pub struct EnumVariant {
    pub name:   token::Ident,
    pub fields: StructFields,
    pub disc:   Option<Expr>,
    pub span:   Span,
}

#[derive(Debug)]
// all statements have scopes, inheriting from the previous scope (this allows
// overriding variables)
pub enum HirStmt {
    Let {
        pattern:   concrete::IdentPattern,
        value:     &'static Value,
        ty:        MaybeType,
        expr:      Option<Box<Expr>>,
        else_expr: Option<Block>,
        scope:     &'static Scope,
        span:      Span,
    },
    Expr(Expr),
    Semi(Expr),
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Path {
        path: concrete::Path,
        ty:   MaybeType,
        span: Span,
    },
    Value {
        value: &'static Value,
        span:  Span,
    },
    Tuple {
        expr: Vec<Expr>,
        ty:   MaybeType,
        span: Span,
    },
    Array {
        elems: Vec<Expr>,
        ty:    MaybeType,
        span:  Span,
    },
    Struct(StructExpr),
    Block(Block),
    If(If),
    Match {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
        ty:   MaybeType,
        span: Span,
    },
    Loop {
        label: Option<concrete::Label>,
        body:  Block,
        ty:    MaybeType,
        span:  Span,
    },
    For {
        label:   Option<concrete::Label>,
        pattern: concrete::Pattern,
        expr:    Box<Expr>,
        body:    Block,
        ty:      MaybeType,
        span:    Span,
    },
    While {
        label:     Option<concrete::Label>,
        condition: Box<Expr>,
        body:      Block,
        ty:        MaybeType,
        span:      Span,
    },
    ControlFlow {
        kind:  ControlFlowKind,
        label: Option<concrete::Label>,
        expr:  Option<Box<Expr>>,
        scope: &'static Scope,
        span:  Span,
    },
    Assign {
        var:             token::Ident,
        field:           Option<token::Ident>,
        idx:             Option<Box<Expr>>,
        op:              concrete::AssignOp,
        expr:            Box<Expr>,
        span:            Span,
        // these fields are used for dealing with drops when we lower to IR, they indicate the
        // state of the value BEFORE this assignment
        value:           Option<&'static Value>,
        // if this is a field assignment, then moved indicates whether the field was moved, if it's
        // simple variable assignment, then moved indicates whether the whole variable was moved
        moved:           bool,
        partially_moved: Vec<token::Ident>,
    },
    Range {
        start:     Option<Box<Expr>>,
        end:       Option<Box<Expr>>,
        inclusive: bool,
        ty:        MaybeType,
        span:      Span,
    },
    BinaryOp {
        left:  Box<Expr>,
        op:    BinaryOp,
        right: Box<Expr>,
        ty:    MaybeType,
        span:  Span,
    },
    UnaryOp {
        op:   UnaryOp,
        expr: Box<Expr>,
        ty:   MaybeType,
        span: Span,
    },
    Unwrap {
        expr: Box<Expr>,
        ty:   MaybeType,
        span: Span,
    },
    Wrap {
        expr: Box<Expr>,
        ty:   MaybeType,
        span: Span,
    },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
        ty:   MaybeType,
        span: Span,
    },
    FieldAccess {
        expr:  Box<Expr>,
        field: token::Ident,
        ty:    MaybeType,
        span:  Span,
    },
    MethodCall {
        expr:   Box<Expr>,
        method: token::Ident,
        args:   Vec<Expr>,
        ty:     MaybeType,
        span:   Span,
    },
    Index {
        expr:  Box<Expr>,
        index: Box<Expr>,
        ty:    MaybeType,
        span:  Span,
    },
    Cast {
        expr: Box<Expr>,
        ty:   MaybeType,
        span: Span,
    },
}

#[derive(Debug)]
pub enum ControlFlowKind {
    Return,
    Break,
    Continue,
}

#[derive(Debug)]
pub enum BinaryOp {
    Or,     // ||
    And,    // &&
    Eq,     // ==
    Neq,    // !=
    Lt,     // <
    Gt,     // >
    Leq,    // <=
    Geq,    // >=
    BitOr,  // |
    BitXor, // ^
    BitAnd, // &
    Shl,    // <<
    Shr,    // >>
    Add,    // +
    Sub,    // -
    Mul,    // *
    Div,    // /
    Mod,    // %
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op_str = match self {
            BinaryOp::Or => "||",
            BinaryOp::And => "&&",
            BinaryOp::Eq => "==",
            BinaryOp::Neq => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Gt => ">",
            BinaryOp::Leq => "<=",
            BinaryOp::Geq => ">=",
            BinaryOp::BitOr => "|",
            BinaryOp::BitXor => "^",
            BinaryOp::BitAnd => "&",
            BinaryOp::Shl => "<<",
            BinaryOp::Shr => ">>",
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
        };
        write!(f, "{}", op_str)
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Not, // !
    Neg, // -
    Ref, // &
}

#[derive(Debug)]
pub enum StructExpr {
    Named {
        path:   concrete::Path,
        fields: Vec<StructFieldExpr>,
        rest:   Option<Box<Expr>>,
        ty:     MaybeType,
        span:   Span,
    },
    Unnamed {
        path:   concrete::Path,
        fields: Vec<Expr>,
        ty:     MaybeType,
        span:   Span,
    },
    Unit {
        path: concrete::Path,
        ty:   MaybeType,
        span: Span,
    },
}

#[derive(Debug)]
pub struct StructFieldExpr {
    pub name: token::Ident,
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<HirStmt>,
    pub ty:    MaybeType,
    pub scope: &'static Scope,
    pub span:  Span,
}

#[derive(Debug)]
pub struct If {
    pub condition:  Box<Expr>,
    pub then_block: Block,
    pub else_block: Option<ElseBlock>,
    pub ty:         MaybeType,
    pub span:       Span,
}

#[derive(Debug)]
pub enum ElseBlock {
    If(Box<If>),
    Block(Block),
}

#[derive(Debug)]
pub enum MatchArm {
    Block {
        pattern: concrete::Pattern,
        guard:   Option<Box<Expr>>,
        block:   Block,
        span:    Span,
    },
    Expr {
        pattern: concrete::Pattern,
        guard:   Option<Box<Expr>>,
        expr:    Box<Expr>,
        span:    Span,
    },
}

#[derive(Debug)]
pub enum Literal {
    Unit(Span),
    Int {
        value: token::IntLiteral,
        ty:    MaybeType,
        span:  Span,
    },
    Float {
        value: token::FloatLiteral,
        ty:    MaybeType,
        span:  Span,
    },
    String {
        value: token::StringLiteral,
        ty:    MaybeType,
        span:  Span,
    },
    Char {
        value: token::CharLiteral,
        ty:    MaybeType,
        span:  Span,
    },
    Bool {
        value: token::BoolLiteral,
        ty:    MaybeType,
        span:  Span,
    },
    Nil {
        ty:   MaybeType,
        span: Span,
    },
}

pub trait IntoHir {
    type Output;
    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output>;
}

impl IntoHir for concrete::StructFields {
    type Output = StructFields;

    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output> {
        match self {
            concrete::StructFields::Named { fields } => {
                let mut hir_fields = Vec::new();
                for field in fields {
                    hir_fields.push(NamedStructField {
                        visibility: field.visibility.unwrap_or(Visibility::Private),
                        name:       field.name,
                        ty:         MaybeType::Unresolved(field.ty, scope),
                        span:       field.span,
                    });
                }
                Ok(StructFields::Named { fields: hir_fields })
            },
            concrete::StructFields::Unnamed { fields } => {
                let mut hir_fields = Vec::new();
                for field in fields {
                    hir_fields.push(UnnamedStructField {
                        visibility: field.visibility.unwrap_or(Visibility::Private),
                        ty:         MaybeType::Unresolved(field.ty, scope),
                        span:       field.span,
                    });
                }
                Ok(StructFields::Unnamed { fields: hir_fields })
            },
            concrete::StructFields::Unit => Ok(StructFields::Unit),
        }
    }
}

impl IntoHir for concrete::Block {
    type Output = Block;

    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output> {
        let mut block_scope = scope.inherit_all()?;
        let mut stmts = Vec::new();
        for stmt in self.stmts {
            match stmt {
                concrete::Stmt::Let(stmt) => {
                    let expr = if let Some(expr) = stmt.expr {
                        Some(Box::new(expr.into_hir(block_scope)?))
                    } else {
                        None
                    };
                    let else_expr = if let Some(else_block) = stmt.else_block {
                        Some(else_block.into_hir(block_scope)?)
                    } else {
                        None
                    };
                    if else_expr.is_some() {
                        todo!("else expressions not supposed in let statements yet");
                    }
                    let ident = match stmt.pattern.clone() {
                        concrete::IdentPattern::Ident(ident) => ident,
                        _ => {
                            todo!("only simple ident patterns supported in let statements for now")
                        },
                    };
                    let value = block_scope.new_value(ident.clone(), Value::Var {
                        id:              0,
                        name:            ident,
                        ty:              if let Some(ty) = stmt.ty.clone() {
                            MaybeType::Unresolved(ty, block_scope)
                        } else {
                            MaybeType::Inferred
                        },
                        moved:           false,
                        partially_moved: Vec::new(),
                    })?;
                    let hir_stmt = HirStmt::Let {
                        pattern: stmt.pattern.clone(),
                        ty: if let Some(ty) = stmt.ty {
                            MaybeType::Unresolved(ty, block_scope)
                        } else {
                            MaybeType::Inferred
                        },
                        expr,
                        else_expr,
                        scope: block_scope,
                        span: stmt.span,
                        value,
                    };
                    // since let statements can introduce new variables, we need to create a new
                    // scope for the next statement
                    block_scope = block_scope.inherit_all()?;
                    stmts.push(hir_stmt);
                },
                concrete::Stmt::Expr(expr) => {
                    stmts.push(HirStmt::Expr(expr.into_hir(block_scope)?));
                },
                concrete::Stmt::Semi(expr) => {
                    stmts.push(HirStmt::Semi(expr.into_hir(block_scope)?));
                },
                concrete::Stmt::Item(_) => {
                    todo!("items in blocks not supported yet")
                },
            }
        }
        Ok(Block {
            stmts,
            ty: MaybeType::Inferred,
            scope: block_scope,
            span: self.span,
        })
    }
}

impl IntoHir for concrete::Expr {
    type Output = Expr;

    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output> {
        match self {
            concrete::Expr::ControlFlow {
                kind,
                label,
                expr,
                span,
            } => {
                Ok(Expr::ControlFlow {
                    kind: match kind {
                        concrete::ControlFlowKind::Return => ControlFlowKind::Return,
                        concrete::ControlFlowKind::Break => ControlFlowKind::Break,
                        concrete::ControlFlowKind::Continue => ControlFlowKind::Continue,
                    },
                    label,
                    expr: match expr {
                        Some(e) => Some(Box::new(e.into_hir(scope)?)),
                        None => None,
                    },
                    scope,
                    span,
                })
            },
            concrete::Expr::Assign(expr) => expr.into_hir(scope),
        }
    }
}

impl IntoHir for concrete::AssignExpr {
    type Output = Expr;

    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output> {
        match self {
            concrete::AssignExpr::Assign {
                var,
                field,
                idx,
                op,
                expr,
                span,
            } => {
                Ok(Expr::Assign {
                    var,
                    field,
                    idx: match idx {
                        Some(e) => Some(Box::new(e.into_hir(scope)?)),
                        None => None,
                    },
                    op,
                    expr: Box::new(expr.into_hir(scope)?),
                    span,
                    value: None,
                    moved: false,
                    partially_moved: Vec::new(),
                })
            },
            concrete::AssignExpr::Range(expr) => expr.into_hir(scope),
        }
    }
}

impl IntoHir for concrete::RangeExpr {
    type Output = Expr;

    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output> {
        match self {
            concrete::RangeExpr::Range {
                start,
                end,
                inclusive,
                span,
            } => {
                Ok(Expr::Range {
                    start: match start {
                        Some(e) => Some(Box::new(e.into_hir(scope)?)),
                        None => None,
                    },
                    end: match end {
                        Some(e) => Some(Box::new(e.into_hir(scope)?)),
                        None => None,
                    },
                    inclusive,
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::RangeExpr::LogicalOr(expr) => expr.into_hir(scope),
        }
    }
}

impl IntoHir for concrete::LogicalOrExpr {
    type Output = Expr;

    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output> {
        match self {
            concrete::LogicalOrExpr::LogicalOr { left, right, span } => {
                Ok(Expr::BinaryOp {
                    left: Box::new(left.into_hir(scope)?),
                    op: BinaryOp::Or,
                    right: Box::new(right.into_hir(scope)?),
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::LogicalOrExpr::LogicalAnd(expr) => expr.into_hir(scope),
        }
    }
}

impl IntoHir for concrete::LogicalAndExpr {
    type Output = Expr;

    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output> {
        match self {
            concrete::LogicalAndExpr::LogicalAnd { left, right, span } => {
                Ok(Expr::BinaryOp {
                    left: Box::new(left.into_hir(scope)?),
                    op: BinaryOp::And,
                    right: Box::new(right.into_hir(scope)?),
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::LogicalAndExpr::Comparison(expr) => expr.into_hir(scope),
        }
    }
}

impl IntoHir for concrete::ComparisonExpr {
    type Output = Expr;

    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output> {
        match self {
            concrete::ComparisonExpr::Comparison {
                left,
                op,
                right,
                span,
            } => {
                let bin_op = match op {
                    concrete::ComparisonOp::Eq => BinaryOp::Eq,
                    concrete::ComparisonOp::Neq => BinaryOp::Neq,
                    concrete::ComparisonOp::Lt => BinaryOp::Lt,
                    concrete::ComparisonOp::Gt => BinaryOp::Gt,
                    concrete::ComparisonOp::Leq => BinaryOp::Leq,
                    concrete::ComparisonOp::Geq => BinaryOp::Geq,
                };
                Ok(Expr::BinaryOp {
                    left: Box::new(left.into_hir(scope)?),
                    op: bin_op,
                    right: Box::new(right.into_hir(scope)?),
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::ComparisonExpr::BitwiseOr(expr) => expr.into_hir(scope),
        }
    }
}

impl IntoHir for concrete::BitwiseOrExpr {
    type Output = Expr;

    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output> {
        match self {
            concrete::BitwiseOrExpr::BitwiseOr { left, right, span } => {
                Ok(Expr::BinaryOp {
                    left: Box::new(left.into_hir(scope)?),
                    op: BinaryOp::BitOr,
                    right: Box::new(right.into_hir(scope)?),
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::BitwiseOrExpr::BitwiseXor(expr) => expr.into_hir(scope),
        }
    }
}

impl IntoHir for concrete::BitwiseXorExpr {
    type Output = Expr;

    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output> {
        match self {
            concrete::BitwiseXorExpr::BitwiseXor { left, right, span } => {
                Ok(Expr::BinaryOp {
                    left: Box::new(left.into_hir(scope)?),
                    op: BinaryOp::BitXor,
                    right: Box::new(right.into_hir(scope)?),
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::BitwiseXorExpr::BitwiseAnd(expr) => expr.into_hir(scope),
        }
    }
}

impl IntoHir for concrete::BitwiseAndExpr {
    type Output = Expr;

    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output> {
        match self {
            concrete::BitwiseAndExpr::BitwiseAnd { left, right, span } => {
                Ok(Expr::BinaryOp {
                    left: Box::new(left.into_hir(scope)?),
                    op: BinaryOp::BitAnd,
                    right: Box::new(right.into_hir(scope)?),
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::BitwiseAndExpr::Shift(expr) => expr.into_hir(scope),
        }
    }
}

impl IntoHir for concrete::ShiftExpr {
    type Output = Expr;

    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output> {
        match self {
            concrete::ShiftExpr::Shift {
                left,
                op,
                right,
                span,
            } => {
                let bin_op = match op {
                    concrete::ShiftOp::Left => BinaryOp::Shl,
                    concrete::ShiftOp::Right => BinaryOp::Shr,
                };
                Ok(Expr::BinaryOp {
                    left: Box::new(left.into_hir(scope)?),
                    op: bin_op,
                    right: Box::new(right.into_hir(scope)?),
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::ShiftExpr::AddSub(expr) => expr.into_hir(scope),
        }
    }
}

impl IntoHir for concrete::AddSubExpr {
    type Output = Expr;

    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output> {
        match self {
            concrete::AddSubExpr::AddSub {
                left,
                op,
                right,
                span,
            } => {
                let bin_op = match op {
                    concrete::AddSubOp::Add => BinaryOp::Add,
                    concrete::AddSubOp::Sub => BinaryOp::Sub,
                };
                Ok(Expr::BinaryOp {
                    left: Box::new(left.into_hir(scope)?),
                    op: bin_op,
                    right: Box::new(right.into_hir(scope)?),
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::AddSubExpr::MulDivMod(expr) => expr.into_hir(scope),
        }
    }
}

impl IntoHir for concrete::MulDivModExpr {
    type Output = Expr;

    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output> {
        match self {
            concrete::MulDivModExpr::MulDivMod {
                left,
                op,
                right,
                span,
            } => {
                let bin_op = match op {
                    concrete::MulDivModOp::Mul => BinaryOp::Mul,
                    concrete::MulDivModOp::Div => BinaryOp::Div,
                    concrete::MulDivModOp::Mod => BinaryOp::Mod,
                };
                Ok(Expr::BinaryOp {
                    left: Box::new(left.into_hir(scope)?),
                    op: bin_op,
                    right: Box::new(right.into_hir(scope)?),
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::MulDivModExpr::Cast(expr) => expr.into_hir(scope),
        }
    }
}

impl IntoHir for concrete::CastExpr {
    type Output = Expr;

    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output> {
        match self {
            concrete::CastExpr::Cast { expr, ty, span } => {
                Ok(Expr::Cast {
                    expr: Box::new(expr.into_hir(scope)?),
                    ty: MaybeType::Unresolved(ty, scope),
                    span,
                })
            },
            concrete::CastExpr::Unary(expr) => expr.into_hir(scope),
        }
    }
}

impl IntoHir for concrete::UnaryExpr {
    type Output = Expr;

    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output> {
        match self {
            concrete::UnaryExpr::Unary { op, expr, span } => {
                let un_op = match op {
                    concrete::UnaryOp::Not => UnaryOp::Not,
                    concrete::UnaryOp::Neg => UnaryOp::Neg,
                    concrete::UnaryOp::Ref => UnaryOp::Ref,
                };
                Ok(Expr::UnaryOp {
                    op: un_op,
                    expr: Box::new(expr.into_hir(scope)?),
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::UnaryExpr::FieldAccess(expr) => expr.into_hir(scope),
        }
    }
}

impl IntoHir for concrete::FieldAccessExpr {
    type Output = Expr;

    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output> {
        match self {
            concrete::FieldAccessExpr::Unwrap { expr, span } => {
                Ok(Expr::Unwrap {
                    expr: Box::new(expr.into_hir(scope)?),
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::FieldAccessExpr::Wrap { expr, span } => {
                Ok(Expr::Wrap {
                    expr: Box::new(expr.into_hir(scope)?),
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::FieldAccessExpr::Index { array, index, span } => {
                Ok(Expr::Index {
                    expr: Box::new(array.into_hir(scope)?),
                    index: Box::new(index.into_hir(scope)?),
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::FieldAccessExpr::FunctionCall { func, args, span } => {
                let mut hir_args = Vec::new();
                for arg in args {
                    hir_args.push(arg.into_hir(scope)?);
                }
                Ok(Expr::Call {
                    func: Box::new(func.into_hir(scope)?),
                    args: hir_args,
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::FieldAccessExpr::FieldAccess { expr, field, span } => {
                Ok(Expr::FieldAccess {
                    expr: Box::new(expr.into_hir(scope)?),
                    field,
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::FieldAccessExpr::MethodCall {
                expr,
                method,
                args,
                span,
            } => {
                let mut hir_args = Vec::new();
                for arg in args {
                    hir_args.push(arg.into_hir(scope)?);
                }
                Ok(Expr::MethodCall {
                    expr: Box::new(expr.into_hir(scope)?),
                    method,
                    args: hir_args,
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::FieldAccessExpr::Primary(expr) => expr.into_hir(scope),
        }
    }
}

impl IntoHir for concrete::PrimaryExpr {
    type Output = Expr;

    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output> {
        match self {
            concrete::PrimaryExpr::Unit(span) => Ok(Expr::Literal(Literal::Unit(span))),
            concrete::PrimaryExpr::Literal(lit) => Ok(Expr::Literal(lit.into_hir(scope)?)),
            concrete::PrimaryExpr::Path(path) => {
                Ok(Expr::Path {
                    span: path.span,
                    path,
                    ty: MaybeType::Inferred,
                })
            },
            concrete::PrimaryExpr::Group { expr, span } => expr.into_hir(scope),
            concrete::PrimaryExpr::Tuple { elems, span } => {
                let mut hir_elems = Vec::new();
                for elem in elems {
                    hir_elems.push(elem.into_hir(scope)?);
                }
                Ok(Expr::Tuple {
                    expr: hir_elems,
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::PrimaryExpr::Array { elems, span } => {
                let mut hir_elems = Vec::new();
                for elem in elems {
                    hir_elems.push(elem.into_hir(scope)?);
                }
                Ok(Expr::Array {
                    elems: hir_elems,
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::PrimaryExpr::Struct(expr) => Ok(Expr::Struct(expr.into_hir(scope)?)),
            concrete::PrimaryExpr::Block(expr) => Ok(Expr::Block(expr.into_hir(scope)?)),
            concrete::PrimaryExpr::If(expr) => Ok(Expr::If(expr.into_hir(scope)?)),
            concrete::PrimaryExpr::Match(expr) => {
                let mut hir_arms = Vec::new();
                for arm in expr.arms {
                    match arm {
                        concrete::MatchArm::Block {
                            pattern,
                            guard,
                            block,
                            span,
                        } => {
                            hir_arms.push(MatchArm::Block {
                                pattern,
                                guard: match guard {
                                    Some(g) => Some(Box::new(g.into_hir(scope)?)),
                                    None => None,
                                },
                                block: block.into_hir(scope)?,
                                span,
                            });
                        },
                        concrete::MatchArm::Expr {
                            pattern,
                            guard,
                            expr,
                            span,
                        } => {
                            hir_arms.push(MatchArm::Expr {
                                pattern,
                                guard: match guard {
                                    Some(g) => Some(Box::new(g.into_hir(scope)?)),
                                    None => None,
                                },
                                expr: Box::new(expr.into_hir(scope)?),
                                span,
                            });
                        },
                    }
                }
                Ok(Expr::Match {
                    expr: Box::new(expr.expr.into_hir(scope)?),
                    arms: hir_arms,
                    ty:   MaybeType::Inferred,
                    span: expr.span,
                })
            },
            concrete::PrimaryExpr::Loop(expr) => {
                Ok(Expr::Loop {
                    label: expr.label,
                    body:  expr.body.into_hir(scope)?,
                    ty:    MaybeType::Inferred,
                    span:  expr.span,
                })
            },
            concrete::PrimaryExpr::For(expr) => {
                Ok(Expr::For {
                    label:   expr.label,
                    pattern: expr.pattern,
                    expr:    Box::new(expr.expr.into_hir(scope)?),
                    body:    expr.body.into_hir(scope)?,
                    ty:      MaybeType::Inferred,
                    span:    expr.span,
                })
            },
            concrete::PrimaryExpr::While(expr) => {
                Ok(Expr::While {
                    label:     expr.label,
                    condition: Box::new(expr.cond.into_hir(scope)?),
                    body:      expr.body.into_hir(scope)?,
                    ty:        MaybeType::Inferred,
                    span:      expr.span,
                })
            },
        }
    }
}

impl IntoHir for concrete::StructExpr {
    type Output = StructExpr;

    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output> {
        match self {
            concrete::StructExpr::Named {
                path,
                fields,
                rest,
                span,
            } => {
                let mut hir_fields = Vec::new();
                for field in fields {
                    hir_fields.push(StructFieldExpr {
                        name: field.name.clone(),
                        expr: match field.expr {
                            Some(e) => e.into_hir(scope)?,
                            None => {
                                Expr::Path {
                                    path: concrete::Path {
                                        root:     false,
                                        pkg:      false,
                                        segments: vec![field.name.clone()],
                                        span:     field.span,
                                    },
                                    ty:   MaybeType::Inferred,
                                    span: field.span,
                                }
                            },
                        },
                        span: field.span,
                    });
                }
                Ok(StructExpr::Named {
                    path,
                    fields: hir_fields,
                    rest: match rest {
                        Some(e) => Some(Box::new(e.into_hir(scope)?)),
                        None => None,
                    },
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::StructExpr::Unnamed { path, fields, span } => {
                let mut hir_fields = Vec::new();
                for field in fields {
                    hir_fields.push(field.into_hir(scope)?);
                }
                Ok(StructExpr::Unnamed {
                    path,
                    fields: hir_fields,
                    ty: MaybeType::Inferred,
                    span,
                })
            },
            concrete::StructExpr::Unit { path, span } => {
                Ok(StructExpr::Unit {
                    path,
                    ty: MaybeType::Inferred,
                    span,
                })
            },
        }
    }
}

impl IntoHir for token::TokenLiteral {
    type Output = Literal;

    fn into_hir(self, _: &'static Scope) -> Result<Self::Output> {
        match self {
            token::TokenLiteral::Int(value) => {
                Ok(Literal::Int {
                    span: value.span,
                    value,
                    ty: MaybeType::Inferred,
                })
            },
            token::TokenLiteral::Float(value) => {
                Ok(Literal::Float {
                    span: value.span,
                    value,
                    ty: MaybeType::Inferred,
                })
            },
            token::TokenLiteral::String(value) => {
                Ok(Literal::String {
                    span: value.span,
                    value,
                    ty: MaybeType::Inferred,
                })
            },
            token::TokenLiteral::Char(value) => {
                Ok(Literal::Char {
                    span: value.span,
                    value,
                    ty: MaybeType::Inferred,
                })
            },
            token::TokenLiteral::Bool(value) => {
                Ok(Literal::Bool {
                    span: value.span,
                    value,
                    ty: MaybeType::Inferred,
                })
            },
            token::TokenLiteral::Nil(span) => {
                Ok(Literal::Nil {
                    ty:   MaybeType::Inferred,
                    span: span.span,
                })
            },
        }
    }
}

impl IntoHir for concrete::IfExpr {
    type Output = If;

    fn into_hir(self, scope: &'static Scope) -> Result<Self::Output> {
        Ok(If {
            condition:  Box::new(self.cond.into_hir(scope)?),
            then_block: self.then.into_hir(scope)?,
            else_block: match self.otherwise {
                Some(else_block) => {
                    Some(match else_block {
                        concrete::ElseExpr::If(ie) => ElseBlock::If(Box::new(ie.into_hir(scope)?)),
                        concrete::ElseExpr::Block(b) => ElseBlock::Block(b.into_hir(scope)?),
                    })
                },
                None => None,
            },
            ty:         MaybeType::Inferred,
            span:       self.span,
        })
    }
}

impl Block {
    // this makes a single inference pass, these continue until nothing further can
    // be inferred
    pub fn infer(&mut self, expected: Option<&'static Type>) -> Result<bool> {
        let mut inferred = false;
        let len = self.stmts.len();
        for (i, stmt) in self.stmts.iter_mut().enumerate() {
            match stmt {
                HirStmt::Semi(expr) => {
                    inferred |= expr.infer(self.scope, None)?;
                },
                HirStmt::Expr(expr) => {
                    if i == len - 1 {
                        // last expr in block, use expected type
                        inferred |= expr.infer(self.scope, expected)?;
                    } else {
                        inferred |= expr.infer(self.scope, None)?;
                    }
                },
                HirStmt::Let {
                    pattern,
                    ty,
                    expr,
                    scope,
                    span,
                    ..
                } => {
                    let ident = match pattern {
                        concrete::IdentPattern::Ident(ident) => ident,
                        _ => {
                            todo!("only simple ident patterns supported in let statements for now")
                        },
                    };
                    let value = scope.resolve_name_value_mut(ident)?;
                    if let MaybeType::Unresolved(..) = ty {
                        // the explicit type annotation is already on the value
                        *ty = MaybeType::Inferred;
                    }
                    match value {
                        Value::Var { ty: val_ty, .. } => {
                            ty.reconcile_type(val_ty, *span)?;
                        },
                        _ => unreachable!(),
                    }
                    // if we have an expr, then we can use that to find the type of the let
                    // statement and update its type and the associated value's type
                    if let Some(expr) = expr {
                        inferred |= expr.infer(scope, ty.ty_opt())?;
                        if let MaybeType::Resolved(expr_ty) = expr.ty() {
                            *ty = MaybeType::Resolved(expr_ty);
                        }
                    }
                },
            }
        }
        if self.ty.is_inferred() {
            if let Some(last_stmt) = self.stmts.last_mut() {
                let return_ty = match last_stmt {
                    HirStmt::Expr(expr) => expr.ty(),
                    HirStmt::Semi(_) | HirStmt::Let { .. } => &MaybeType::Resolved(&Type::Unit),
                };
                if let MaybeType::Resolved(ret_ty) = return_ty {
                    self.ty = MaybeType::Resolved(ret_ty);
                    inferred = true;
                }
            } else {
                // empty block, return unit
                self.ty = MaybeType::Resolved(&Type::Unit);
                inferred = true;
            }
            // if any of the block's return the never type (meaning they are guaranteed to
            // return/break/continue to a higher scope rather than return a value)
            // then the block's type is also never
            for stmt in &self.stmts {
                let stmt_ty = match stmt {
                    HirStmt::Expr(expr) => expr.ty(),
                    HirStmt::Semi(expr) => {
                        match expr.ty() {
                            // semi statements always return unit, unless they are never
                            MaybeType::Resolved(Type::Never) => &MaybeType::Resolved(&Type::Never),
                            _ => &MaybeType::Resolved(&Type::Unit),
                        }
                    },
                    HirStmt::Let { .. } => &MaybeType::Resolved(&Type::Unit),
                };
                if let MaybeType::Resolved(Type::Never) = stmt_ty {
                    self.ty = MaybeType::Resolved(&Type::Never);
                    inferred = true;
                    break;
                }
            }
            if let Some(expected_ty) = expected {
                self.ty
                    .reconcile_type(&mut MaybeType::Resolved(expected_ty), self.span)?;
            }
        }
        Ok(inferred)
    }

    pub fn check_fully_resolved(&self) -> Result<()> {
        for stmt in &self.stmts {
            match stmt {
                HirStmt::Expr(expr) | HirStmt::Semi(expr) => expr.check_fully_resolved()?,
                HirStmt::Let { ty, span, expr, .. } => {
                    if !ty.is_resolved() {
                        return Err(CompileError::Error(
                            *span,
                            "the type of this let statement could not be fully resolved"
                                .to_string(),
                        ));
                    }
                    if let Some(e) = expr {
                        e.check_fully_resolved()?
                    }
                },
            }
        }
        if !self.ty.is_resolved() {
            return Err(CompileError::Error(
                self.span,
                "the type of this block could not be fully resolved".to_string(),
            ));
        }
        Ok(())
    }

    pub fn check_ownership(&mut self, mut moveset: Moveset) -> Result<Moveset> {
        for stmt in &mut self.stmts {
            moveset.new_value();
            moveset = match stmt {
                HirStmt::Expr(expr) | HirStmt::Semi(expr) => expr.check_ownership(moveset, true)?,
                HirStmt::Let { expr, .. } => {
                    if let Some(e) = expr {
                        e.check_ownership(moveset, true)?
                    } else {
                        continue;
                    }
                },
            };
        }
        #[allow(clippy::mutable_key_type)]
        let scope_values = if let Some(values) = self.scope.value_ns.as_ref() {
            values.iter().map(|x| *x).collect::<HashSet<_>>()
        } else {
            HashSet::new()
        };
        for value in &moveset.references {
            if scope_values.contains(value.0) {
                return Err(CompileError::Error(
                    self.span,
                    format!(
                        "cannot return a reference to value '{}' defined in this scope",
                        value.0.name().value
                    ),
                ));
            }
        }
        Ok(moveset)
    }
}

#[derive(Debug, Clone, Default)]
pub struct Moveset {
    // the following fields store the changes made to values in a given scope
    // these are primarily used to join scopes after branching (if)
    // a value can only be moved once
    pub moved:           HashSet<&'static Value>,
    // a value can only be partially moved once per field
    pub partially_moved: HashMap<&'static Value, HashSet<token::Ident>>,
    // the set of values referencing the given value/field
    // true = valid reference, false = invalid reference (referred value has been moved)
    #[allow(clippy::type_complexity)]
    pub referencing:
        HashMap<(&'static Value, Option<token::Ident>), HashSet<(&'static Value, bool)>>,

    // the following fields store information about the value being returned from the current scope
    pub possible_values: HashSet<(&'static Value, Option<token::Ident>)>,
    pub references:      HashSet<(&'static Value, Option<token::Ident>)>,
}

impl Moveset {
    pub fn new() -> Self {
        Moveset::default()
    }

    // if either moved or partially moved differ, then we moved at least one value
    pub fn any_moved(&self, other: &Moveset) -> bool {
        self.moved != other.moved || self.partially_moved != other.partially_moved
    }

    pub fn fork(&self) -> Self {
        Moveset {
            moved:           self.moved.clone(),
            partially_moved: self.partially_moved.clone(),
            referencing:     self.referencing.clone(),
            possible_values: HashSet::new(),
            references:      HashSet::new(),
        }
    }

    pub fn join(&mut self, other: &Moveset) {
        for value in &other.moved {
            self.moved.insert(value);
        }
        for (value, fields) in &other.partially_moved {
            let entry = self.partially_moved.entry(value).or_default();
            for field in fields {
                entry.insert(field.clone());
            }
        }
        for (key, referencing_values) in &other.referencing {
            #[allow(clippy::mutable_key_type)]
            let entry = self.referencing.entry(key.clone()).or_default();
            for value in referencing_values {
                entry.insert(*value);
            }
        }
        for reference in &other.references {
            self.references.insert(reference.clone());
        }
        for value in &other.possible_values {
            self.possible_values.insert(value.clone());
        }
        for reference in &other.references {
            self.references.insert(reference.clone());
        }
    }

    // apply the moveset to the various values
    pub fn apply(&self) {
        for value in &self.moved {
            value.mark_move();
        }
        for (value, fields) in &self.partially_moved {
            for field in fields {
                value.mark_partial_move(field.clone());
            }
        }
    }

    pub fn check_if_value_accessible(
        &mut self,
        value: &'static Value,
        allow_partial_move: bool,
        span: Span,
    ) -> Result<()> {
        if self.moved.contains(value) {
            return Err(CompileError::Error(
                span,
                format!("value '{}' has already been moved", value.name().value),
            ));
        }
        // if the value has been partially moved, it cannot be fully moved
        if !allow_partial_move {
            if let Some(partial_moves) = self.partially_moved.get(value) {
                if !partial_moves.is_empty() {
                    return Err(CompileError::Error(
                        span,
                        format!(
                            "value '{}' has already been partially moved",
                            value.name().value
                        ),
                    ));
                }
            }
        }
        // check if any references from this value are invalid
        for (_, values) in self.referencing.iter() {
            if values.contains(&(value, false)) {
                return Err(CompileError::Error(
                    span,
                    format!(
                        "value '{}' cannot be accessed because it contains a reference to a moved \
                         value",
                        value.name().value
                    ),
                ));
            }
        }
        Ok(())
    }

    pub fn mark_move(&mut self, value: &'static Value, span: Span) -> Result<()> {
        self.check_if_value_accessible(value, false, span)?;
        self.moved.insert(value);
        // we also need to remove any references this value may have held
        for referencing_values in self.referencing.values_mut() {
            referencing_values.remove(&(value, true));
            referencing_values.remove(&(value, false));
        }
        // invalidate all references to this value
        for (key, values) in self.referencing.iter_mut() {
            if key.0 == value {
                *values = values.iter().map(|(v, _)| (*v, false)).collect();
            }
        }
        Ok(())
    }

    pub fn check_if_field_accessible(
        &mut self,
        value: &'static Value,
        field: &token::Ident,
        span: Span,
    ) -> Result<()> {
        if self.moved.contains(value) {
            return Err(CompileError::Error(
                span,
                format!("value '{}' has already been moved", value.name().value),
            ));
        }
        if let Some(partial_moves) = self.partially_moved.get(value) {
            if partial_moves.contains(field) {
                return Err(CompileError::Error(
                    span,
                    format!(
                        "field '{}' of value '{}' has already been moved",
                        field.value,
                        value.name().value
                    ),
                ));
            }
        }
        // check if any references from this field are invalid
        for (_, values) in self.referencing.iter() {
            if values.contains(&(value, false)) {
                return Err(CompileError::Error(
                    span,
                    format!(
                        "field '{}' of value '{}' cannot be accessed because '{}' contains a \
                         reference to a moved value",
                        field.value,
                        value.name().value,
                        value.name().value
                    ),
                ));
            }
        }
        Ok(())
    }

    pub fn mark_partial_move(
        &mut self,
        value: &'static Value,
        field: token::Ident,
        span: Span,
    ) -> Result<()> {
        // invalidate all references to this field
        for (key, values) in self.referencing.iter_mut() {
            if key.0 == value && key.1.as_ref() == Some(&field) {
                *values = values.iter().map(|(v, _)| (*v, false)).collect();
            }
        }
        self.partially_moved.entry(value).or_default().insert(field);
        Ok(())
    }

    pub fn mark_reference(
        &mut self,
        value: &'static Value,
        referenced_value: &'static Value,
        field: Option<token::Ident>,
        span: Span,
    ) -> Result<()> {
        if self.moved.contains(referenced_value) {
            return Err(CompileError::Error(
                span,
                format!(
                    "cannot reference value '{}' because it has been moved",
                    referenced_value.name().value
                ),
            ));
        }
        if let Some(partial_moves) = self.partially_moved.get(referenced_value) {
            if let Some(field) = &field {
                if partial_moves.contains(field) {
                    return Err(CompileError::Error(
                        span,
                        format!(
                            "cannot reference field '{}' of value '{}' because it has been moved",
                            field.value,
                            referenced_value.name().value
                        ),
                    ));
                }
            }
        }
        self.referencing
            .entry((referenced_value, field))
            .or_default()
            .insert((value, true));
        Ok(())
    }

    // value is now being returned from an expression so we need to track its
    // references
    pub fn returning(&mut self, value: &'static Value) {
        self.possible_values.insert((value, None));
        for (key, referencing_values) in &self.referencing {
            if key.0 == value {
                for v in referencing_values {
                    self.references.insert(key.clone());
                }
            }
        }
    }

    pub fn add_reference(&mut self, value: &'static Value, field: Option<token::Ident>) {
        self.references.insert((value, field));
    }

    pub fn unmove(&mut self, value: &'static Value) -> (bool, Vec<token::Ident>) {
        (
            self.moved.remove(value),
            self.partially_moved
                .remove(value)
                .map(|x| x.into_iter().collect())
                .unwrap_or_default(),
        )
    }

    pub fn unmove_field(&mut self, value: &'static Value, field: &token::Ident) -> bool {
        if let Some(partial_moves) = self.partially_moved.get_mut(value) {
            return partial_moves.remove(field);
        }
        false
    }

    // we're starting a new value being returned from an expression
    pub fn new_value(&mut self) {
        self.possible_values.clear();
        self.references.clear();
    }

    pub fn values_to_refs(&mut self) {
        for value in &self.possible_values {
            self.references.insert(value.clone());
        }
        self.possible_values.clear();
    }

    pub fn move_in(&mut self, value: &'static Value, invalidate: bool) {
        if invalidate {
            // invalidate all references to this value
            for (key, values) in self.referencing.iter_mut() {
                if key.0 == value {
                    *values = values.iter().map(|(v, _)| (*v, false)).collect();
                }
            }
            // remove all references from this value
            for (key, values) in &mut self.referencing {
                values.remove(&(value, true));
                values.remove(&(value, false));
            }
        }
        // insert all new references from this value
        for reference in &self.references {
            self.referencing
                .entry(reference.clone())
                .or_default()
                .insert((value, true));
        }
    }
}

impl Literal {
    pub fn infer(&mut self, expected: Option<&'static Type>) -> Result<bool> {
        match self {
            Literal::Unit(span) => {
                if let Some(expected_ty) = expected {
                    if *expected_ty != Type::Unit {
                        return Err(CompileError::Error(
                            *span,
                            format!("type mismatch: expected '{}', found '()'", expected_ty),
                        ));
                    }
                }
                Ok(false)
            },
            Literal::Int { value, ty, span } => {
                // this can only be an integer type, i32, i64, u32, u64, etc.
                // the suffux could indicate the type (this does not use a scope lookup)
                // otherwise, we rely on the expected type
                if !ty.is_inferred() {
                    return Ok(false);
                }
                let suffix_ty = match value.suffix {
                    Some("i32") => Some(&Type::I32),
                    Some(_) => {
                        return Err(CompileError::Error(
                            *span,
                            format!("unknown integer suffix '{}'", value.suffix.unwrap()),
                        ))
                    },
                    _ => None,
                };
                if let Some(suffix_ty) = suffix_ty {
                    ty.reconcile_type(&mut MaybeType::Resolved(suffix_ty), *span)?;
                }
                if let Some(expected_ty) = expected {
                    ty.reconcile_type(&mut MaybeType::Resolved(expected_ty), *span)?;
                }
                if let MaybeType::Resolved(t) = ty {
                    match t {
                        Type::I32 => {},
                        _ => {
                            return Err(CompileError::Error(
                                *span,
                                format!("type mismatch: expected '{}', found integer type", t),
                            ))
                        },
                    }
                }
                Ok(!ty.is_inferred())
            },
            Literal::Float { value, ty, span } => {
                if !ty.is_inferred() {
                    return Ok(false);
                }
                // similar to int, but for floats
                let suffix_ty = match value.suffix {
                    Some("f64") => Some(&Type::F64),
                    Some(_) => {
                        return Err(CompileError::Error(
                            *span,
                            format!("unknown float suffix '{}'", value.suffix.unwrap()),
                        ))
                    },
                    _ => None,
                };
                if let Some(suffix_ty) = suffix_ty {
                    ty.reconcile_type(&mut MaybeType::Resolved(suffix_ty), *span)?;
                }
                if let Some(expected_ty) = expected {
                    ty.reconcile_type(&mut MaybeType::Resolved(expected_ty), *span)?;
                }
                if let MaybeType::Resolved(t) = ty {
                    match t {
                        Type::F64 => {},
                        _ => {
                            return Err(CompileError::Error(
                                *span,
                                format!("type mismatch: expected '{}', found float", t),
                            ))
                        },
                    }
                }
                Ok(!ty.is_inferred())
            },
            Literal::Bool { ty, span, .. } => {
                if !ty.is_inferred() {
                    return Ok(false);
                }
                ty.reconcile_type(&mut MaybeType::Resolved(&Type::Bool), *span)?;
                if let Some(expected_ty) = expected {
                    ty.reconcile_type(&mut MaybeType::Resolved(expected_ty), *span)?;
                }
                Ok(!ty.is_inferred())
            },
            Literal::Nil { ty, span } => {
                if !ty.is_inferred() {
                    return Ok(false);
                }
                // nil relies entirely on the expected type
                if let Some(expected_ty) = expected {
                    match expected_ty {
                        Type::Optional { .. } => {},
                        _ => {
                            return Err(CompileError::Error(
                                *span,
                                format!("type mismatch: expected '{}', found 'nil'", expected_ty),
                            ))
                        },
                    }
                    ty.reconcile_type(&mut MaybeType::Resolved(expected_ty), *span)?;
                }
                Ok(!ty.is_inferred())
            },
            Literal::String { span, .. } => Err(CompileError::Unimplemented(*span, "strings")),
            Literal::Char { span, .. } => Err(CompileError::Unimplemented(*span, "chars")),
        }
    }

    pub fn check_fully_resolved(&self) -> Result<()> {
        match self {
            Literal::Unit(_) => Ok(()),
            Literal::Int { ty, span, .. }
            | Literal::Float { ty, span, .. }
            | Literal::String { ty, span, .. }
            | Literal::Char { ty, span, .. }
            | Literal::Bool { ty, span, .. }
            | Literal::Nil { ty, span, .. } => {
                if !ty.is_resolved() {
                    return Err(CompileError::Error(
                        *span,
                        "the type of this literal could not be fully resolved".to_string(),
                    ));
                }
                Ok(())
            },
        }
    }

    pub fn ty(&self) -> &MaybeType {
        match self {
            Literal::Unit(_) => &MaybeType::Resolved(&Type::Unit),
            Literal::Int { ty, .. }
            | Literal::Float { ty, .. }
            | Literal::String { ty, .. }
            | Literal::Char { ty, .. }
            | Literal::Bool { ty, .. }
            | Literal::Nil { ty, .. } => ty,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Literal::Unit(span)
            | Literal::Int { span, .. }
            | Literal::Float { span, .. }
            | Literal::String { span, .. }
            | Literal::Char { span, .. }
            | Literal::Bool { span, .. }
            | Literal::Nil { span, .. } => *span,
        }
    }
}

impl Expr {
    pub fn infer(
        &mut self,
        scope: &'static Scope,
        expected: Option<&'static Type>,
    ) -> Result<bool> {
        let mut inferred = false;
        match self {
            Expr::Literal(literal) => {
                inferred |= literal.infer(expected)?;
            },
            Expr::Path { path, ty, span } => {
                // resolve the path to a value in the scope
                let value = scope.resolve_path_value_mut(path)?;
                ty.reconcile_type(value.ty_mut(), *span)?;
                if let Some(expected_ty) = expected {
                    ty.reconcile_type(&mut MaybeType::Resolved(expected_ty), *span)?;
                }
                *self = Expr::Value { value, span: *span };
            },
            Expr::Value { value, span } => {
                if let Value::Var { ty: val_ty, .. } = value {
                    if let Some(expected_ty) = expected {
                        let val_ty =
                            unsafe { &mut *(val_ty as *const MaybeType as *mut MaybeType) };
                        val_ty.reconcile_type(&mut MaybeType::Resolved(expected_ty), *span)?;
                    }
                }
            },
            Expr::Tuple { expr, ty, span } => {
                // if i have an expected type, it must be a tuple type of the same length
                if let Some(expected_ty) = expected {
                    if let Type::Tuple { fields, .. } = expected_ty {
                        if fields.len() != expr.len() {
                            return Err(CompileError::Error(
                                *span,
                                format!(
                                    "type mismatch: expected tuple of length {}, found tuple of \
                                     length {}",
                                    fields.len(),
                                    expr.len()
                                ),
                            ));
                        }
                        for (e, t) in expr.iter_mut().zip(fields.iter()) {
                            inferred |= e.infer(scope, Some(*t))?;
                        }
                        ty.reconcile_type(&mut MaybeType::Resolved(expected_ty), *span)?;
                    } else {
                        return Err(CompileError::Error(
                            *span,
                            format!("type mismatch: expected '{}', found tuple", expected_ty),
                        ));
                    }
                } else {
                    let mut all_resolved = true;
                    for e in expr.iter_mut() {
                        inferred |= e.infer(scope, None)?;
                        if let MaybeType::Inferred | MaybeType::Unresolved(..) = e.ty() {
                            all_resolved = false;
                        }
                    }
                    if all_resolved {
                        // if all elements are resolved, we can create the tuple type
                        let mut fields = Vec::new();
                        for e in expr.iter() {
                            if let MaybeType::Resolved(e_ty) = e.ty() {
                                fields.push(*e_ty);
                            } else {
                                return Err(CompileError::Error(
                                    *span,
                                    "could not infer type of tuple element".to_string(),
                                ));
                            }
                        }
                        let tuple_ty = scope.new_type(None, Type::Tuple {
                            id: 0,
                            fields,
                            scope,
                        })?;
                        ty.reconcile_type(&mut MaybeType::Resolved(tuple_ty), *span)?;
                    }
                }
            },
            // ty should be the array type
            Expr::Array { elems, ty, span } => {
                // if we have an expected type, it must be an array type
                if let Some(expected_ty) = expected {
                    if let Type::Array {
                        ty: elem_ty,
                        length,
                        ..
                    } = expected_ty
                    {
                        if *length != elems.len() {
                            return Err(CompileError::Error(
                                *span,
                                format!(
                                    "type mismatch: expected array of size {}, found array of \
                                     size {}",
                                    length,
                                    elems.len()
                                ),
                            ));
                        }
                        for e in elems.iter_mut() {
                            inferred |= e.infer(scope, Some(*elem_ty))?;
                        }
                        ty.reconcile_type(&mut MaybeType::Resolved(expected_ty), *span)?;
                    } else {
                        return Err(CompileError::Error(
                            *span,
                            format!("type mismatch: expected '{}', found array", expected_ty),
                        ));
                    }
                } else {
                    for e in elems.iter_mut() {
                        inferred |= e.infer(scope, None)?;
                    }
                    // now, we need to see if we can infer the element type based on the first
                    // element
                    if elems.is_empty() {
                        return Err(CompileError::Error(
                            *span,
                            "cannot infer type of empty array".to_string(),
                        ));
                    }
                    if let MaybeType::Resolved(elem_ty) = elems[0].ty() {
                        let elem_ty = *elem_ty;
                        for e in elems.iter_mut().skip(1) {
                            inferred |= e.infer(scope, Some(elem_ty))?;
                        }
                        let array_ty = scope.new_type(None, Type::Array {
                            id: 0,
                            ty: elem_ty,
                            length: elems.len(),
                            scope,
                        })?;
                        ty.reconcile_type(&mut MaybeType::Resolved(array_ty), *span)?;
                    }
                }
            },
            Expr::Struct(expr) => {
                match expr {
                    StructExpr::Named {
                        path,
                        fields,
                        rest,
                        ty,
                        span,
                    } => {
                        // resolve the struct type
                        let struct_ty = scope.resolve_path_type(path)?;
                        if let Some(rest) = rest {
                            rest.infer(scope, Some(struct_ty))?;
                        }
                        if let Type::Struct {
                            fields: struct_fields,
                            ..
                        } = struct_ty
                        {
                            match struct_fields {
                                StructFields::Named { fields: field_tys } => {
                                    for field_expr in fields.iter_mut() {
                                        // find the field type
                                        let field_ty = field_tys.iter().find_map(|field| {
                                            if field.name.value == field_expr.name.value {
                                                Some(field.ty.ty())
                                            } else {
                                                None
                                            }
                                        });
                                        if let Some(field_ty) = field_ty {
                                            inferred |=
                                                field_expr.expr.infer(scope, Some(field_ty))?;
                                        } else {
                                            return Err(CompileError::Error(
                                                *span,
                                                format!(
                                                    "struct '{}' has no field named '{}'",
                                                    struct_ty, field_expr.name.value
                                                ),
                                            ));
                                        }
                                    }
                                    // now we need to ensure that every field is initialized
                                    // fields that are optional types can be omitted and will be
                                    // set to nil
                                    for field_ty in field_tys.iter() {
                                        if !fields
                                            .iter()
                                            .any(|f| f.name.value == field_ty.name.value)
                                        {
                                            // field not initialized
                                            if !matches!(field_ty.ty.ty(), Type::Optional { .. }) {
                                                return Err(CompileError::Error(
                                                    *span,
                                                    format!(
                                                        "field '{}' of struct '{}' is not \
                                                         initialized",
                                                        field_ty.name.value, struct_ty
                                                    ),
                                                ));
                                            }
                                        }
                                    }
                                    ty.reconcile_type(&mut MaybeType::Resolved(struct_ty), *span)?;
                                    if let Some(expected_ty) = expected {
                                        ty.reconcile_type(
                                            &mut MaybeType::Resolved(expected_ty),
                                            *span,
                                        )?;
                                    }
                                },
                                StructFields::Unnamed { .. } => {
                                    return Err(CompileError::Error(
                                        *span,
                                        format!(
                                            "cannot use named struct expression for unnamed \
                                             struct '{}'",
                                            struct_ty
                                        ),
                                    ));
                                },
                                StructFields::Unit => {
                                    return Err(CompileError::Error(
                                        *span,
                                        format!(
                                            "cannot use named struct expression for unit struct \
                                             '{}'",
                                            struct_ty
                                        ),
                                    ));
                                },
                            }
                        }
                    },
                    StructExpr::Unnamed {
                        path,
                        fields,
                        ty,
                        span,
                    } => {
                        return Err(CompileError::Unimplemented(
                            *span,
                            "unnamed struct expressions",
                        ));
                    },
                    StructExpr::Unit { path, ty, span } => {
                        return Err(CompileError::Unimplemented(
                            *span,
                            "unit struct expressions",
                        ));
                    },
                }
            },
            Expr::Block(block) => {
                inferred |= block.infer(expected)?;
            },
            Expr::If(expr) => {
                inferred |= expr.infer(scope, expected)?;
            },
            Expr::Match {
                expr,
                arms,
                ty,
                span,
            } => {
                return Err(CompileError::Unimplemented(*span, "match expressions"));
            },
            Expr::Loop {
                label,
                body,
                ty,
                span,
            } => {
                // for loops, the expected type is the break type, the actual body should return
                // the unit type
                if let Some(label) = label {
                    if body.scope.label_exists(label) {
                        return Err(CompileError::Error(
                            *span,
                            format!("label '{}' already exists in this scope", label.name.value),
                        ));
                    }
                }
                body.scope.set_break_ty(label.clone(), match expected {
                    Some(t) => MaybeType::Resolved(t),
                    None => MaybeType::Inferred,
                });
                body.ty
                    .reconcile_type(&mut MaybeType::Resolved(&Type::Unit), *span)?;
                inferred |= body.infer(expected)?;
                // now, we can reconcile the break type to the body's type
                ty.reconcile_type(body.scope.break_ty(label.clone()).unwrap(), *span)?;
            },
            Expr::For {
                label,
                pattern,
                expr,
                body,
                ty,
                span,
            } => {
                return Err(CompileError::Unimplemented(*span, "for loops"));
            },
            Expr::While {
                label,
                condition,
                body,
                ty,
                span,
            } => {
                inferred |= condition.infer(scope, Some(&Type::Bool))?;
                if let Some(label) = label {
                    if body.scope.label_exists(label) {
                        return Err(CompileError::Error(
                            *span,
                            format!("label '{}' already exists in this scope", label.name.value),
                        ));
                    }
                }
                body.scope.set_break_ty(label.clone(), match expected {
                    Some(t) => MaybeType::Resolved(t),
                    None => MaybeType::Resolved(&Type::Unit),
                });
                body.ty
                    .reconcile_type(&mut MaybeType::Resolved(&Type::Unit), *span)?;
                inferred |= body.infer(expected)?;
                // now, we can reconcile the break type to the body's type
                ty.reconcile_type(body.scope.break_ty(label.clone()).unwrap(), *span)?;
            },
            Expr::ControlFlow {
                expr,
                kind,
                label,
                span,
                ..
            } => {
                // control flow expressions always have the never type, since they do not return
                // a value, just influence control flow
                match kind {
                    ControlFlowKind::Continue => {
                        if label.is_some() {
                            // we do want to make sure there is a loop with this label
                            if scope.break_ty(label.clone()).is_none() {
                                return Err(CompileError::Error(
                                    *span,
                                    format!(
                                        "no loop found with label '{}'",
                                        label.as_ref().unwrap().name.value,
                                    ),
                                ));
                            }
                        }
                        if expr.is_some() {
                            return Err(CompileError::Error(
                                *span,
                                "continue cannot have an expression".to_string(),
                            ));
                        }
                    },
                    ControlFlowKind::Break => {
                        if let Some(break_ty) = scope.break_ty(label.clone()) {
                            if let Some(e) = expr {
                                inferred |= e.infer(scope, break_ty.ty_opt())?;
                            } else {
                                // break with no expression has unit type
                                break_ty
                                    .reconcile_type(&mut MaybeType::Resolved(&Type::Unit), *span)?;
                            }
                        } else {
                            return Err(CompileError::Error(
                                *span,
                                if let Some(label) = label {
                                    format!("no loop found with label '{}'", label.name.value)
                                } else {
                                    "no enclosing loop found for break".to_string()
                                },
                            ));
                        }
                    },
                    ControlFlowKind::Return => {
                        if label.is_some() {
                            return Err(CompileError::Error(
                                *span,
                                "return cannot have a label".to_string(),
                            ));
                        }
                        if let Some(return_ty) = scope.return_ty() {
                            if let Some(e) = expr {
                                inferred |= e.infer(scope, Some(return_ty))?;
                            } else {
                                // return with no expression has unit type
                                if return_ty != &Type::Unit {
                                    return Err(CompileError::Error(
                                        *span,
                                        format!(
                                            "type mismatch: expected '{}', found '()'",
                                            return_ty
                                        ),
                                    ));
                                }
                            }
                        } else {
                            return Err(CompileError::Error(
                                *span,
                                "no enclosing function found for return".to_string(),
                            ));
                        }
                    },
                }
            },
            Expr::Assign {
                var,
                field,
                idx,
                op,
                expr,
                span,
                value,
                ..
            } => {
                *value = Some(scope.resolve_name_value(var)?);
                let val = scope.resolve_name_value_mut(var)?;
                match val {
                    Value::Var { ty: val_ty, .. } => {
                        if let MaybeType::Resolved(mut current_ty) = val_ty {
                            // now, we need to perform a series of field accesses to get the final
                            // type
                            if let Some(f) = field {
                                current_ty = match current_ty {
                                    Type::Ref { ty, .. } => ty,
                                    _ => current_ty,
                                };
                                match current_ty {
                                    Type::Struct { fields, .. } => {
                                        match fields {
                                            StructFields::Named { fields } => {
                                                if let Some(field_ty) = fields
                                                    .iter()
                                                    .find(|field| field.name.value == f.value)
                                                {
                                                    match field_ty.ty {
                                                        MaybeType::Resolved(fty) => {
                                                            current_ty = fty;
                                                        },
                                                        _ => {
                                                            unreachable!(
                                                                "struct field types are always \
                                                                 resolved at this point"
                                                            );
                                                        },
                                                    }
                                                } else {
                                                    return Err(CompileError::Error(
                                                        *span,
                                                        format!(
                                                            "type '{}' has no field named '{}'",
                                                            current_ty, f.value
                                                        ),
                                                    ));
                                                }
                                            },
                                            _ => {
                                                return Err(CompileError::Error(
                                                    *span,
                                                    format!(
                                                        "type '{}' has no named fields",
                                                        current_ty
                                                    ),
                                                ));
                                            },
                                        }
                                    },
                                    _ => {
                                        return Err(CompileError::Error(
                                            *span,
                                            format!("type '{}' has no fields", current_ty),
                                        ));
                                    },
                                }
                            }
                            if let Some(index_expr) = idx {
                                inferred |= index_expr.infer(scope, Some(&Type::I32))?;
                                // indexing into the current type
                                current_ty = match current_ty {
                                    Type::Ref { ty, .. } => ty,
                                    _ => current_ty,
                                };
                                match current_ty {
                                    Type::Array { ty: elem_ty, .. } => {
                                        current_ty = elem_ty;
                                    },
                                    _ => {
                                        return Err(CompileError::Error(
                                            *span,
                                            format!("type '{}' is not indexable", current_ty),
                                        ));
                                    },
                                }
                            }
                            match op {
                                concrete::AssignOp::Assign => {
                                    inferred |= expr.infer(scope, Some(current_ty))?;
                                },
                                op => {
                                    let (op, msg) = match op {
                                        concrete::AssignOp::Assign => {
                                            unreachable!("handled above");
                                        },
                                        concrete::AssignOp::AddAssign => {
                                            (BinaryOp::Add, "addition")
                                        },
                                        concrete::AssignOp::SubAssign => {
                                            (BinaryOp::Sub, "subtraction")
                                        },
                                        concrete::AssignOp::MulAssign => {
                                            (BinaryOp::Mul, "multiplication")
                                        },
                                        concrete::AssignOp::DivAssign => {
                                            (BinaryOp::Div, "division")
                                        },
                                        concrete::AssignOp::ModAssign => (BinaryOp::Mod, "modulus"),
                                        concrete::AssignOp::BitAndAssign => {
                                            (BinaryOp::BitAnd, "bitwise AND")
                                        },
                                        concrete::AssignOp::BitOrAssign => {
                                            (BinaryOp::BitOr, "bitwise OR")
                                        },
                                        concrete::AssignOp::BitXorAssign => {
                                            (BinaryOp::BitXor, "logical XOR")
                                        },
                                        concrete::AssignOp::BitShlAssign => {
                                            (BinaryOp::Shl, "bitwise left shift")
                                        },
                                        concrete::AssignOp::BitShrAssign => {
                                            (BinaryOp::Shr, "bitwise right shift")
                                        },
                                    };
                                    let hint = current_ty.binary_op_hint(&op);
                                    if let Some(hint) = hint {
                                        inferred |= expr.infer(scope, Some(hint.1))?;
                                    } else {
                                        return Err(CompileError::Error(
                                            *span,
                                            format!(
                                                "type '{}' does not support {}",
                                                current_ty, msg
                                            ),
                                        ));
                                    }
                                },
                            }
                        }
                    },
                    _ => {
                        return Err(CompileError::Error(
                            *span,
                            format!("cannot assign to non-variable '{}'", var.value),
                        ));
                    },
                }
            },
            Expr::Range {
                start,
                end,
                inclusive,
                ty,
                span,
            } => {
                return Err(CompileError::Unimplemented(*span, "range expressions"));
            },
            Expr::BinaryOp {
                left,
                op,
                right,
                ty,
                span,
            } => {
                inferred |= left.infer(scope, None)?;
                // if this is Some, then this will be the type of right AND the result type
                let hint = if let MaybeType::Resolved(left_ty) = left.ty() {
                    left_ty.binary_op_hint(op)
                } else {
                    None
                };
                inferred |= right.infer(scope, hint.map(|hint| hint.0))?;
                // now, we can determine the result type
                if let (MaybeType::Resolved(left_ty), MaybeType::Resolved(right_ty)) =
                    (left.ty(), right.ty())
                {
                    ty.reconcile_type(
                        &mut MaybeType::Resolved(left_ty.binary_op(right_ty, op, *span)?),
                        *span,
                    )?;
                } else {
                    // otherwise, we can use the hint if it exists
                    if let Some(hint_ty) = hint {
                        ty.reconcile_type(&mut MaybeType::Resolved(hint_ty.1), *span)?;
                    }
                }
                if let Some(expected_ty) = expected {
                    ty.reconcile_type(&mut MaybeType::Resolved(expected_ty), *span)?;
                }
            },
            Expr::UnaryOp { op, expr, ty, span } => {
                inferred |= expr.infer(scope, expected.and_then(|t| t.unary_op_hint(op)))?;
                if let MaybeType::Resolved(expr_ty) = expr.ty() {
                    ty.reconcile_type(
                        &mut MaybeType::Resolved(expr_ty.unary_op(op, *span)?),
                        *span,
                    )?;
                }
                if let Some(expected_ty) = expected {
                    ty.reconcile_type(&mut MaybeType::Resolved(expected_ty), *span)?;
                }
            },
            Expr::Unwrap { expr, ty, span } => {
                if let Some(expected_ty) = expected {
                    ty.reconcile_type(&mut MaybeType::Resolved(expected_ty), *span)?;
                }
                inferred |= expr.infer(scope, None)?;
                if let MaybeType::Resolved(expr_ty) = expr.ty() {
                    match expr_ty {
                        Type::Optional { ty: inner_ty, .. } => {
                            ty.reconcile_type(&mut MaybeType::Resolved(inner_ty), *span)?;
                        },
                        _ => {
                            return Err(CompileError::Error(
                                *span,
                                format!("type '{}' is not an optional type", expr_ty),
                            ));
                        },
                    }
                }
            },
            Expr::Wrap { expr, ty, span } => {
                if let Some(expected_ty) = expected {
                    match expected_ty {
                        Type::Optional { ty: inner_ty, .. } => {
                            ty.reconcile_type(&mut MaybeType::Resolved(expected_ty), *span)?;
                        },
                        _ => {
                            return Err(CompileError::Error(
                                *span,
                                format!("type '{}' is not an optional type", expected_ty),
                            ));
                        },
                    }
                }
                let expected_inner_ty = if let MaybeType::Resolved(expected_ty) = ty {
                    match expected_ty {
                        Type::Optional { ty: inner_ty, .. } => Some(*inner_ty),
                        _ => None,
                    }
                } else {
                    None
                };
                inferred |= expr.infer(scope, expected_inner_ty)?;
                if let MaybeType::Resolved(expr_ty) = expr.ty() {
                    let optional_ty = Type::Optional {
                        id: 0,
                        ty: expr_ty,
                        scope,
                    }
                    .store();
                    ty.reconcile_type(&mut MaybeType::Resolved(optional_ty), *span)?;
                }
            },
            Expr::FieldAccess {
                expr,
                field,
                ty,
                span,
            } => {
                inferred |= expr.infer(scope, None)?;
                // now, we need to see if we can infer the field's type based on the expr's type
                if let MaybeType::Resolved(expr_ty) = expr.ty_mut() {
                    let expr_ty = match expr_ty {
                        Type::Ref { ty, .. } => ty,
                        _ => expr_ty,
                    };
                    match expr_ty {
                        Type::Struct { fields, .. } => {
                            match fields {
                                StructFields::Named { fields } => {
                                    if let Some(field_ty) =
                                        fields.iter().find(|f| f.name.value == field.value)
                                    {
                                        ty.reconcile_type(
                                            &mut MaybeType::Resolved(field_ty.ty.ty()),
                                            *span,
                                        )?;
                                    } else {
                                        return Err(CompileError::Error(
                                            *span,
                                            format!(
                                                "type '{}' has no field named '{}'",
                                                expr_ty, field.value
                                            ),
                                        ));
                                    }
                                },
                                StructFields::Unnamed { fields: _ } => {
                                    return Err(CompileError::Error(
                                        *span,
                                        format!(
                                            "cannot access field '{}' on unnamed struct '{}'",
                                            field.value, expr_ty
                                        ),
                                    ));
                                },
                                StructFields::Unit => {
                                    return Err(CompileError::Error(
                                        *span,
                                        format!("type '{}' has no fields", expr_ty),
                                    ));
                                },
                            }
                        },
                        _ => {
                            return Err(CompileError::Error(
                                *span,
                                format!("type '{}' has no fields", expr_ty),
                            ));
                        },
                    }
                }
            },
            Expr::MethodCall {
                expr,
                method,
                args,
                ty,
                span,
            } => {
                return Err(CompileError::Unimplemented(*span, "method calls"));
            },
            Expr::Call {
                func,
                args,
                ty,
                span,
            } => {
                inferred |= func.infer(scope, None)?;
                for arg in args.iter_mut() {
                    inferred |= arg.infer(scope, None)?;
                }
                // now, we need to see if we can infer the function's type
                if let MaybeType::Resolved(func_ty) = func.ty() {
                    if let Type::Fn {
                        param_tys, ret_ty, ..
                    } = func_ty
                    {
                        if param_tys.len() != args.len() {
                            return Err(CompileError::Error(
                                *span,
                                format!(
                                    "argument count mismatch: expected {}, found {}",
                                    param_tys.len(),
                                    args.len()
                                ),
                            ));
                        }
                        for (arg, param_ty) in args.iter_mut().zip(param_tys.iter()) {
                            inferred |= arg.infer(scope, Some(*param_ty))?;
                            let span = arg.span();
                            MaybeType::Resolved(param_ty).reconcile_type(arg.ty_mut(), span)?;
                        }
                        ty.reconcile_type(&mut MaybeType::Resolved(ret_ty), *span)?;
                        if let Some(expected_ty) = expected {
                            ty.reconcile_type(&mut MaybeType::Resolved(expected_ty), *span)?;
                        }
                    } else {
                        return Err(CompileError::Error(
                            *span,
                            format!("type '{}' is not callable", func_ty),
                        ));
                    }
                }
            },
            Expr::Index {
                expr,
                index,
                ty,
                span,
            } => {
                // infer the expr and index
                inferred |= expr.infer(scope, None)?;
                inferred |= index.infer(scope, Some(&Type::I32))?;
                // now, we need to see if we can infer the index's type based on the expr's type
                if let MaybeType::Resolved(expr_ty) = expr.ty_mut() {
                    match expr_ty {
                        Type::Array { ty: elem_ty, .. } => {
                            ty.reconcile_type(&mut MaybeType::Resolved(elem_ty), *span)?;
                            if let Some(expected_ty) = expected {
                                ty.reconcile_type(&mut MaybeType::Resolved(expected_ty), *span)?;
                            }
                        },
                        _ => {
                            return Err(CompileError::Error(
                                *span,
                                format!("type '{}' is not indexable", expr_ty),
                            ));
                        },
                    }
                }
            },
            Expr::Cast { expr, ty, span } => {
                ty.resolve_explicit()?;
                inferred |= expr.infer(scope, None)?;
                match ty {
                    MaybeType::Resolved(Type::Optional { ty, .. }) => {
                        if let MaybeType::Resolved(expr_ty) = expr.ty() {
                            if *ty == *expr_ty {
                                // casting to the same type, allow it
                            } else {
                                return Err(CompileError::Error(
                                    *span,
                                    format!(
                                        "cannot cast type '{}' to optional type '{}?'",
                                        expr_ty, ty
                                    ),
                                ));
                            }
                        }
                    },
                    _ => {
                        // for now, we only allow casting nil to optional types
                        if let MaybeType::Resolved(expr_ty) = expr.ty() {
                            return Err(CompileError::Error(
                                *span,
                                format!("cannot cast type '{}' to type '{}'", expr_ty, ty.ty()),
                            ));
                        }
                    },
                }
                // right now this will only work for nil casts, nothing else can be casted
                if let Some(expected_ty) = expected {
                    ty.reconcile_type(&mut MaybeType::Resolved(expected_ty), *span)?;
                }
            },
        }
        Ok(inferred)
    }

    pub fn check_fully_resolved(&self) -> Result<()> {
        match self {
            Expr::Literal(lit) => lit.check_fully_resolved()?,
            Expr::Path { ty, span, .. } => {
                if !ty.is_resolved() {
                    return Err(CompileError::Error(
                        *span,
                        "the type of this path could not be fully resolved".to_string(),
                    ));
                }
            },
            Expr::Value { value, span, .. } => {
                if !value.ty().is_resolved() {
                    return Err(CompileError::Error(
                        *span,
                        "the type of this value could not be fully resolved".to_string(),
                    ));
                }
            },
            Expr::Tuple { expr, span, .. } => {
                for e in expr.iter() {
                    e.check_fully_resolved()?;
                }
                if !self.ty().is_resolved() {
                    return Err(CompileError::Error(
                        *span,
                        "the type of this tuple could not be fully resolved".to_string(),
                    ));
                }
            },
            Expr::Array { elems, span, .. } => {
                for e in elems.iter() {
                    e.check_fully_resolved()?;
                }
                if !self.ty().is_resolved() {
                    return Err(CompileError::Error(
                        *span,
                        "the type of this array could not be fully resolved".to_string(),
                    ));
                }
            },
            Expr::Struct(expr) => {
                match expr {
                    StructExpr::Named {
                        fields,
                        ty,
                        span,
                        rest,
                        ..
                    } => {
                        for field in fields.iter() {
                            field.expr.check_fully_resolved()?;
                        }
                        if !ty.is_resolved() {
                            return Err(CompileError::Error(
                                *span,
                                "the type of this struct expression could not be fully resolved"
                                    .to_string(),
                            ));
                        }
                        if let Some(rest_expr) = rest {
                            rest_expr.check_fully_resolved()?;
                        }
                    },
                    StructExpr::Unnamed { span, .. } => {
                        return Err(CompileError::Unimplemented(
                            *span,
                            "unnamed struct expressions",
                        ));
                    },
                    StructExpr::Unit { span, .. } => {
                        return Err(CompileError::Unimplemented(
                            *span,
                            "unit struct expressions",
                        ));
                    },
                }
            },
            Expr::Block(block) => block.check_fully_resolved()?,
            Expr::If(expr) => expr.check_fully_resolved()?,
            Expr::Match { arms, span, .. } => {
                return Err(CompileError::Unimplemented(*span, "match expressions"));
            },
            Expr::Loop { body, span, .. } => body.check_fully_resolved()?,
            Expr::For { body, span, .. } => {
                return Err(CompileError::Unimplemented(*span, "for loops"));
            },
            Expr::While { body, span, .. } => body.check_fully_resolved()?,
            Expr::ControlFlow { span, expr, .. } => {
                if let Some(e) = expr {
                    e.check_fully_resolved()?
                }
            },
            Expr::Assign { expr, span, .. } => expr.check_fully_resolved()?,
            Expr::Range { ty, span, .. } => {
                return Err(CompileError::Unimplemented(*span, "range expressions"));
            },
            Expr::BinaryOp {
                left,
                right,
                ty,
                span,
                ..
            } => {
                left.check_fully_resolved()?;
                right.check_fully_resolved()?;
                if !ty.is_resolved() {
                    return Err(CompileError::Error(
                        *span,
                        "the type of this binary operation could not be fully resolved".to_string(),
                    ));
                }
            },
            Expr::UnaryOp { expr, ty, span, .. } => {
                expr.check_fully_resolved()?;
                if !ty.is_resolved() {
                    return Err(CompileError::Error(
                        *span,
                        "the type of this unary operation could not be fully resolved".to_string(),
                    ));
                }
            },
            Expr::Unwrap { expr, ty, span, .. } => {
                expr.check_fully_resolved()?;
                if !ty.is_resolved() {
                    return Err(CompileError::Error(
                        *span,
                        "the type of this postfix operation could not be fully resolved"
                            .to_string(),
                    ));
                }
            },
            Expr::Wrap { expr, ty, span, .. } => {
                expr.check_fully_resolved()?;
                if !ty.is_resolved() {
                    return Err(CompileError::Error(
                        *span,
                        "the type of this wrap operation could not be fully resolved".to_string(),
                    ));
                }
            },
            Expr::FieldAccess { expr, ty, span, .. } => {
                expr.check_fully_resolved()?;
                if !ty.is_resolved() {
                    return Err(CompileError::Error(
                        *span,
                        "the type of this field access could not be fully resolved".to_string(),
                    ));
                }
            },
            Expr::MethodCall {
                expr,
                args,
                ty,
                span,
                ..
            } => {
                expr.check_fully_resolved()?;
                for arg in args.iter() {
                    arg.check_fully_resolved()?;
                }
                if !ty.is_resolved() {
                    return Err(CompileError::Error(
                        *span,
                        "the type of this method call could not be fully resolved".to_string(),
                    ));
                }
            },
            Expr::Call {
                func,
                args,
                ty,
                span,
                ..
            } => {
                func.check_fully_resolved()?;
                for arg in args.iter() {
                    arg.check_fully_resolved()?;
                }
                if !ty.is_resolved() {
                    return Err(CompileError::Error(
                        *span,
                        "the type of this function call could not be fully resolved".to_string(),
                    ));
                }
            },
            Expr::Index {
                expr,
                index,
                ty,
                span,
                ..
            } => {
                expr.check_fully_resolved()?;
                index.check_fully_resolved()?;
                if !ty.is_resolved() {
                    return Err(CompileError::Error(
                        *span,
                        "the type of this index expression could not be fully resolved".to_string(),
                    ));
                }
            },
            Expr::Cast { expr, ty, span } => {
                expr.check_fully_resolved()?;
                if !ty.is_resolved() {
                    return Err(CompileError::Error(
                        *span,
                        "the type of this cast expression could not be fully resolved".to_string(),
                    ));
                }
            },
        }
        Ok(())
    }

    pub fn ty_mut(&mut self) -> &mut MaybeType {
        unsafe { &mut *(self.ty() as *const MaybeType as *mut MaybeType) }
    }

    pub fn ty(&self) -> &MaybeType {
        match self {
            Expr::ControlFlow { .. } => &MaybeType::Resolved(&Type::Never),
            Expr::Assign { .. } => &MaybeType::Resolved(&Type::Unit),
            Expr::Range { ty, .. } => ty,
            Expr::BinaryOp { ty, .. } => ty,
            Expr::UnaryOp { ty, .. } => ty,
            Expr::Unwrap { ty, .. } => ty,
            Expr::Wrap { ty, .. } => ty,
            Expr::Literal(lit) => lit.ty(),
            Expr::Path { ty, .. } => ty,
            Expr::Tuple { ty, .. } => ty,
            Expr::Array { ty, .. } => ty,
            Expr::Struct(expr) => {
                match expr {
                    StructExpr::Named { ty, .. }
                    | StructExpr::Unnamed { ty, .. }
                    | StructExpr::Unit { ty, .. } => ty,
                }
            },
            Expr::Block(block) => &block.ty,
            Expr::If(expr) => &expr.ty,
            Expr::Match { ty, .. } => ty,
            Expr::Loop { ty, .. } => ty,
            Expr::For { ty, .. } => ty,
            Expr::While { ty, .. } => ty,
            Expr::FieldAccess { ty, .. } => ty,
            Expr::MethodCall { ty, .. } => ty,
            Expr::Call { ty, .. } => ty,
            Expr::Index { ty, .. } => ty,
            Expr::Cast { ty, .. } => ty,
            Expr::Value { value, .. } => {
                match value {
                    Value::Var { ty, .. } | Value::Const { ty, .. } | Value::Fn { ty, .. } => ty,
                }
            },
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Expr::ControlFlow { span, .. } => *span,
            Expr::Assign { span, .. } => *span,
            Expr::Range { span, .. } => *span,
            Expr::BinaryOp { span, .. } => *span,
            Expr::UnaryOp { span, .. } => *span,
            Expr::Unwrap { span, .. } => *span,
            Expr::Wrap { span, .. } => *span,
            Expr::Literal(lit) => lit.span(),
            Expr::Path { span, .. } => *span,
            Expr::Tuple { span, .. } => *span,
            Expr::Array { span, .. } => *span,
            Expr::Struct(expr) => {
                match expr {
                    StructExpr::Named { span, .. }
                    | StructExpr::Unnamed { span, .. }
                    | StructExpr::Unit { span, .. } => *span,
                }
            },
            Expr::Block(block) => block.span,
            Expr::If(expr) => expr.span,
            Expr::Match { span, .. } => *span,
            Expr::Loop { span, .. } => *span,
            Expr::For { span, .. } => *span,
            Expr::While { span, .. } => *span,
            Expr::FieldAccess { span, .. } => *span,
            Expr::MethodCall { span, .. } => *span,
            Expr::Call { span, .. } => *span,
            Expr::Index { span, .. } => *span,
            Expr::Cast { span, .. } => *span,
            Expr::Value { span, .. } => *span,
        }
    }

    pub fn check_ownership(&mut self, mut moveset: Moveset, moving: bool) -> Result<Moveset> {
        match self {
            Expr::Literal(_) => (),
            Expr::Path { .. } => unreachable!("all paths should have been resolved to values"),
            Expr::Value { value, span, .. } => {
                moveset.new_value();
                if let Value::Var { ty, .. } = value {
                    if !ty.ty().is_copy_type() {
                        if moving {
                            moveset.mark_move(value, *span)?;
                        } else {
                            moveset.check_if_value_accessible(value, true, *span)?;
                        }
                        moveset.returning(value);
                    }
                }
            },
            Expr::Tuple { expr, .. } => {
                moveset.new_value();
                for e in expr.iter_mut() {
                    moveset.join(&e.check_ownership(moveset.fork(), true)?);
                }
            },
            Expr::Array { elems, .. } => {
                moveset.new_value();
                for e in elems.iter_mut() {
                    moveset.join(&e.check_ownership(moveset.fork(), true)?);
                }
            },
            Expr::Struct(expr) => {
                moveset.new_value();
                match expr {
                    StructExpr::Named { fields, .. } => {
                        for field in fields.iter_mut() {
                            moveset.join(&field.expr.check_ownership(moveset.fork(), true)?);
                        }
                    },
                    StructExpr::Unnamed { fields, .. } => {
                        for field in fields.iter_mut() {
                            moveset.join(&field.check_ownership(moveset.fork(), true)?);
                        }
                    },
                    StructExpr::Unit { .. } => (),
                }
            },
            Expr::Block(block) => {
                moveset.new_value();
                moveset = block.check_ownership(moveset)?;
            },
            Expr::If(expr) => {
                moveset.new_value();
                moveset = expr.check_ownership(moveset)?;
            },
            Expr::Match { .. } => {
                return Err(CompileError::Unimplemented(
                    self.span(),
                    "match expressions",
                ));
            },
            Expr::Loop { body, .. } => {
                moveset.new_value();
                moveset = body.check_ownership(moveset)?;
            },
            Expr::For { .. } => {
                return Err(CompileError::Unimplemented(self.span(), "for loops"));
            },
            Expr::While {
                condition, body, ..
            } => {
                moveset.new_value();
                let other_moveset = condition.check_ownership(moveset.clone(), false)?;
                if moveset.any_moved(&other_moveset) {
                    return Err(CompileError::Error(
                        condition.span(),
                        "cannot move values in while loop condition".to_string(),
                    ));
                }
                moveset.new_value();
                moveset = body.check_ownership(moveset)?;
            },
            Expr::ControlFlow { expr, kind, .. } => {
                if let Some(e) = expr {
                    if let ControlFlowKind::Break = kind {
                        return Err(CompileError::Unimplemented(e.span(), "break with value"));
                    }
                    moveset.new_value();
                    moveset = e.check_ownership(moveset, true)?;
                    if !moveset.references.is_empty() {
                        return Err(CompileError::Error(
                            e.span(),
                            "cannot return values that are borrowed from the current scope"
                                .to_string(),
                        ));
                    }
                    moveset.new_value();
                }
            },
            Expr::Assign {
                expr,
                var,
                field,
                idx,
                value,
                moved,
                partially_moved,
                span,
                ..
            } => {
                match idx {
                    Some(idx) => {
                        moveset.new_value();
                        moveset = idx.check_ownership(moveset, true)?;
                        moveset.new_value();
                        moveset = expr.check_ownership(moveset, true)?;
                        if *moved {
                            return Err(CompileError::Error(
                                *span,
                                "cannot assign to a moved value".to_string(),
                            ));
                        }
                        if let Some(field) = field {
                            if partially_moved.contains(field) {
                                return Err(CompileError::Error(
                                    *span,
                                    "cannot assign to a partially moved value".to_string(),
                                ));
                            }
                        }
                        let value = value.unwrap();
                        moveset.move_in(value, false);
                    },
                    None => {
                        moveset.new_value();
                        moveset = expr.check_ownership(moveset, true)?;
                        let value = value.unwrap();
                        match field {
                            None => {
                                // simple variable assignment
                                if !value.ty().ty().is_copy_type() {
                                    (*moved, *partially_moved) = moveset.unmove(value);
                                }
                            },
                            Some(field) => {
                                // field assignment
                                *moved = moveset.unmove_field(value, field);
                            },
                        }
                        moveset.move_in(value, true);
                    },
                }
                moveset.new_value();
            },
            Expr::Range { .. } => {
                return Err(CompileError::Unimplemented(
                    self.span(),
                    "range expressions",
                ));
            },
            Expr::BinaryOp { left, right, .. } => {
                moveset.new_value();
                moveset = left.check_ownership(moveset, false)?;
                moveset.new_value();
                moveset = right.check_ownership(moveset, false)?;
                moveset.new_value();
            },
            Expr::UnaryOp { expr, op, ty, .. } => {
                if let UnaryOp::Ref = op {
                    if expr.ty().ty().is_copy_type() {
                        return Err(CompileError::Error(
                            expr.span(),
                            "cannot take a reference to a copy type".to_string(),
                        ));
                    }
                    // ref needs to keep the value alive
                } else {
                    moveset.new_value();
                }
                moveset = expr.check_ownership(moveset, false)?;
                // if our expression just created a value, i.e. this is &new Node { ... }, then
                // we error out since the value would be immediately dropped
                if expr.is_owned_value() {
                    return Err(CompileError::Error(
                        expr.span(),
                        "cannot borrow a owned value that will be immediately freed".to_string(),
                    ));
                }
                if let UnaryOp::Ref = op {
                    moveset.values_to_refs();
                    moveset.possible_values.clear();
                } else {
                    moveset.new_value();
                }
            },
            Expr::Unwrap { expr, .. } => {
                moveset.new_value();
                moveset = expr.check_ownership(moveset, moving)?;
            },
            Expr::Wrap { expr, .. } => {
                moveset.new_value();
                moveset = expr.check_ownership(moveset, moving)?;
            },
            Expr::Call { func, args, .. } => {
                moveset.new_value();
                moveset = func.check_ownership(moveset, false)?;
                for arg in args.iter_mut() {
                    moveset.new_value();
                    moveset = arg.check_ownership(moveset, true)?;
                }
                moveset.new_value();
            },
            Expr::FieldAccess {
                expr,
                ty,
                field,
                span,
                ..
            } => {
                moveset.new_value();
                let expr = if let Expr::Unwrap { expr, .. } = expr.as_mut() {
                    expr
                } else {
                    expr
                };
                if let Expr::Value { .. } = expr.as_ref() {
                    moveset = expr.check_ownership(moveset, false)?;
                    #[allow(clippy::mutable_key_type)]
                    if ty.ty().is_copy_type() {
                        moveset.new_value();
                    } else {
                        let possible_values = std::mem::take(&mut moveset.possible_values);
                        for value in possible_values.iter() {
                            moveset.add_reference(value.0, Some(field.clone()));
                            if moving {
                                moveset.mark_partial_move(value.0, field.clone(), *span)?;
                            } else {
                                moveset.check_if_field_accessible(value.0, field, *span)?;
                            }
                        }
                    }
                } else {
                    return Err(CompileError::Unimplemented(
                        *span,
                        "field access on non-value expressions",
                    ));
                }
            },
            Expr::MethodCall { expr, args, .. } => {
                return Err(CompileError::Unimplemented(self.span(), "method calls"));
            },
            Expr::Index {
                expr, index, ty, ..
            } => {
                moveset.new_value();
                moveset = index.check_ownership(moveset, true)?;
                moveset.new_value();
                moveset = expr.check_ownership(moveset, false)?;
                if !ty.ty().is_copy_type() {
                    if moving {
                        return Err(CompileError::Error(
                            expr.span(),
                            "cannot move out of array".to_string(),
                        ));
                    } else {
                        return Err(CompileError::Unimplemented(
                            expr.span(),
                            "borrowing from arrays of non-copy types",
                        ));
                    }
                }
            },
            Expr::Cast { expr, .. } => {
                moveset = expr.check_ownership(moveset, false)?;
            },
        }
        Ok(moveset)
    }

    pub fn is_owned_value(&self) -> bool {
        match self {
            Expr::Literal(_) | Expr::Path { .. } | Expr::Value { .. } => false,
            Expr::Tuple { .. } => true,
            Expr::Array { .. } => true,
            Expr::Block(_) => true,
            Expr::Struct(_) => true,
            // blocks always create owned values
            Expr::Block(_) => true,
            Expr::If(_) => true,
            Expr::Match { .. } => true,
            Expr::Loop { .. } => true,
            Expr::For { .. } => true,
            Expr::While { .. } => true,
            Expr::ControlFlow { .. } => false,
            Expr::Assign { .. } => false,
            Expr::Range { .. } => true,
            Expr::BinaryOp { .. } => true,
            Expr::UnaryOp { .. } => true,
            Expr::Unwrap { .. } => true,
            Expr::Wrap { .. } => true,
            Expr::FieldAccess { .. } => false,
            Expr::MethodCall { .. } => true,
            Expr::Call { .. } => true,
            Expr::Index { .. } => false,
            Expr::Cast { .. } => true,
        }
    }
}

impl If {
    pub fn infer(
        &mut self,
        scope: &'static Scope,
        expected: Option<&'static Type>,
    ) -> Result<bool> {
        let mut inferred = false;
        inferred |= self.condition.infer(scope, Some(&Type::Bool))?;
        inferred |= self.then_block.infer(expected)?;
        let mut then_is_never = false;
        let mut else_is_never = false;
        if let Some(ty) = self.then_block.ty.ty_opt() {
            if ty.is_never() {
                then_is_never = true;
            } else {
                self.ty
                    .reconcile_type(&mut MaybeType::Resolved(ty), self.span)?;
            }
        }
        if let Some(else_block) = &mut self.else_block {
            match else_block {
                ElseBlock::If(ie) => {
                    inferred |= ie.infer(scope, expected)?;
                },
                ElseBlock::Block(b) => {
                    inferred |= b.infer(expected)?;
                },
            }
            if let Some(ty) = self.else_block.as_ref().and_then(|eb| {
                match eb {
                    ElseBlock::If(ie) => ie.ty.ty_opt(),
                    ElseBlock::Block(b) => b.ty.ty_opt(),
                }
            }) {
                if ty.is_never() {
                    else_is_never = true;
                } else {
                    self.ty
                        .reconcile_type(&mut MaybeType::Resolved(ty), self.span)?;
                }
            }
        }
        // if both then and else are never, the if is never
        if then_is_never && else_is_never {
            self.ty
                .reconcile_type(&mut MaybeType::Resolved(&Type::Never), self.span)?;
        }
        // finally, reconcile with expected type
        if let Some(expected_ty) = expected {
            self.ty
                .reconcile_type(&mut MaybeType::Resolved(expected_ty), self.span)?;
        }
        Ok(inferred)
    }

    pub fn check_fully_resolved(&self) -> Result<()> {
        self.condition.check_fully_resolved()?;
        self.then_block.check_fully_resolved()?;
        if let Some(else_block) = &self.else_block {
            match else_block {
                ElseBlock::If(ie) => ie.check_fully_resolved()?,
                ElseBlock::Block(b) => b.check_fully_resolved()?,
            }
        }
        if !self.ty.is_resolved() {
            return Err(CompileError::Error(
                self.span,
                "the type of this if expression could not be fully resolved".to_string(),
            ));
        }
        Ok(())
    }

    pub fn check_ownership(&mut self, mut moveset: Moveset) -> Result<Moveset> {
        moveset.new_value();
        moveset = self.condition.check_ownership(moveset, false)?;
        moveset.new_value();
        let then_moveset = self.then_block.check_ownership(moveset.fork())?;
        if let Some(else_block) = &mut self.else_block {
            match else_block {
                ElseBlock::If(ie) => {
                    moveset = ie.check_ownership(moveset.fork())?;
                },
                ElseBlock::Block(b) => {
                    moveset = b.check_ownership(moveset.fork())?;
                },
            }
            moveset.new_value();
        }
        moveset.join(&then_moveset);
        Ok(moveset)
    }
}
