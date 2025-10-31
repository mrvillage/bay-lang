use std::collections::HashMap;

use dashmap::DashMap;

use crate::{
    prelude::*,
    store::{InitStore, Store},
};

pub static mut IR_VALUE_STORE: Option<Store<IrValue>> = None;
pub static mut IR_VALUE_MAPPING: Option<DashMap<u64, &'static IrValue>> = None;
pub static mut IR_TYPE_STORE: Option<Store<IrType>> = None;
pub static mut IR_TYPE_MAPPING: Option<DashMap<&'static Type, &'static IrType>> = None;

#[derive(Debug)]
pub enum IrValue {
    // used for the currently being lowered type so that we can handle recursive cases
    Temp {
        id: u64,
    },
    Const {
        id:   u64,
        ty:   &'static IrType,
        expr: Box<Expr>,
    },
    Var {
        id: u64,
        ty: &'static IrType,
    },
    Fn {
        id:      u64,
        ret_ty:  &'static IrType,
        params:  Vec<&'static IrValue>,
        body:    Box<Block>,
        locals:  Vec<&'static IrValue>,
        ty:      &'static IrType,
        idx:     u64,
        orig_id: u64,
    },
}

pub const TMP_VAR_ID: u64 = 1;
pub const FIRST_NON_RESERVED_IR_VALUE_ID: u64 = 2;

impl InitStore for IrValue {
    fn init_store(store: &mut Store<Self>) {
        store.next_id.store(
            FIRST_NON_RESERVED_IR_VALUE_ID,
            std::sync::atomic::Ordering::Relaxed,
        );
    }
}

pub enum IrType {
    // used for the currently being lowered type so that we can handle recursive cases
    Temp {
        id: u64,
    },
    Never,
    Unit,
    I32,
    F64,
    Bool,
    Struct {
        id:     u64,
        fields: StructFields,
    },
    Tuple {
        id:     u64,
        fields: Vec<&'static IrType>,
    },
    Array {
        id:     u64,
        ty:     &'static IrType,
        length: usize,
    },
    Ref {
        id: u64,
        ty: &'static IrType,
    },
    Optional {
        id: u64,
        ty: &'static IrType,
    },
    Fn {
        id:        u64,
        param_tys: Vec<&'static IrType>,
        ret_ty:    &'static IrType,
    },
}

impl InitStore for IrType {
    fn init_store(store: &mut Store<Self>) {
        store.next_id.store(
            FIRST_NON_RESERVED_TYPE_ID,
            std::sync::atomic::Ordering::Relaxed,
        );
    }
}

impl std::fmt::Debug for IrType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IrType::Temp { id } => write!(f, "Temp<{}>", id),
            IrType::Never => write!(f, "Never"),
            IrType::Unit => write!(f, "Unit"),
            IrType::I32 => write!(f, "I32"),
            IrType::F64 => write!(f, "F64"),
            IrType::Bool => write!(f, "Bool"),
            IrType::Struct { id, .. } => write!(f, "Struct<{}>", id),
            IrType::Tuple { id, .. } => write!(f, "Tuple<{}>", id),
            IrType::Array { id, .. } => write!(f, "Array<{}>", id),
            IrType::Ref { id, .. } => write!(f, "Ref<{}>", id),
            IrType::Optional { id, .. } => write!(f, "Optional<{}>", id),
            IrType::Fn { id, .. } => write!(f, "Fn<{}>", id),
        }
    }
}

impl std::fmt::Display for IrType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IrType::Temp { id } => write!(f, "temp<{}>", id),
            IrType::Never => write!(f, "!"),
            IrType::Unit => write!(f, "()"),
            IrType::I32 => write!(f, "i32"),
            IrType::F64 => write!(f, "f64"),
            IrType::Bool => write!(f, "bool"),
            IrType::Struct { id, .. } => write!(f, "struct<{}>", id),
            IrType::Tuple { id, .. } => write!(f, "tuple<{}>", id),
            IrType::Array { id, .. } => write!(f, "array<{}>", id),
            IrType::Ref { id, .. } => write!(f, "ref<{}>", id),
            IrType::Optional { id, .. } => write!(f, "optional<{}>", id),
            IrType::Fn { id, .. } => write!(f, "fn<{}>", id),
        }
    }
}

#[derive(Debug)]
pub enum StructFields {
    Named { fields: Vec<NamedStructField> },
}

#[derive(Debug)]
pub struct NamedStructField {
    pub name: token::Ident,
    pub ty:   &'static IrType,
}

#[derive(Debug)]
pub enum Stmt {
    Let {
        value: &'static IrValue,
        ty:    &'static IrType,
        expr:  Option<Box<Expr>>,
    },
    Expr(Expr),
    Semi(Expr),
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Value {
        value: &'static IrValue,
    },
    Tuple {
        expr: Vec<Expr>,
        ty:   &'static IrType,
    },
    Array {
        elems: Vec<Expr>,
        ty:    &'static IrType,
    },
    Struct(StructExpr),
    Block(Block),
    If(If),
    Loop {
        label: usize,
        body:  Block,
        ty:    &'static IrType,
    },
    Return {
        expr: Option<Box<Expr>>,
    },
    LoopControl {
        kind:  LoopControlKind,
        label: usize,
        expr:  Option<Box<Expr>>,
    },
    Assign {
        value:           &'static IrValue,
        field:           Option<token::Ident>,
        idx:             Option<Box<Expr>>,
        expr:            Box<Expr>,
        moved:           bool,
        partially_moved: Vec<token::Ident>,
    },
    BinaryOp {
        left:  Box<Expr>,
        op:    BinaryOp,
        right: Box<Expr>,
        ty:    &'static IrType,
    },
    UnaryOp {
        op:   UnaryOp,
        expr: Box<Expr>,
        ty:   &'static IrType,
    },
    Unwrap {
        expr: Box<Expr>,
        ty:   &'static IrType,
    },
    Wrap {
        expr: Box<Expr>,
        ty:   &'static IrType,
    },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
        ty:   &'static IrType,
    },
    FieldAccess {
        expr:  Box<Expr>,
        field: token::Ident,
        ty:    &'static IrType,
    },
    Index {
        expr:  Box<Expr>,
        index: Box<Expr>,
        ty:    &'static IrType,
    },
    Cast {
        expr: Box<Expr>,
        ty:   &'static IrType,
    },
    // this expression will evaluate expr, then free values, then return expr's value
    // this is used for implementing block returns, assignments, and function returns to indicate
    // to the code generator that it needs to free certain values before it can return the value
    // of the expression
    Free {
        expr:   Box<Expr>,
        values: Vec<(&'static IrValue, Vec<token::Ident>)>,
    },
}

#[derive(Debug)]
pub enum LoopControlKind {
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

#[derive(Debug)]
pub enum UnaryOp {
    Not, // !
    Neg, // -
    Ref, // &
}

#[derive(Debug)]
pub enum StructExpr {
    Named {
        fields: Vec<StructFieldExpr>,
        rest:   Option<Box<Expr>>,
        ty:     &'static IrType,
    },
}

#[derive(Debug)]
pub struct StructFieldExpr {
    pub name: token::Ident,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub ty:    &'static IrType,
}

#[derive(Debug)]
pub struct If {
    pub condition:  Box<Expr>,
    pub then_block: Block,
    pub else_block: Option<ElseBlock>,
    pub ty:         &'static IrType,
}

#[derive(Debug)]
pub enum ElseBlock {
    If(Box<If>),
    Block(Block),
}

#[derive(Debug)]
pub enum Literal {
    Unit,
    I32(i32),
    F64(f64),
    Bool(bool),
    Nil { ty: &'static IrType },
}

#[derive(Debug)]
// this struct stores the label hierarchy for loops and control flow statements
// every loop pushes a label to the stack, if the loop has a label, then that
// label is used as a mapping to the generated IR labels
pub struct Labels {
    depth:  usize,
    labels: HashMap<concrete::Label, usize>,
    locals: Vec<&'static IrValue>,
    params: Vec<&'static Value>,
}

impl Labels {
    pub fn new(params: Vec<&'static Value>) -> Self {
        Labels {
            depth: 0,
            labels: HashMap::new(),
            locals: Vec::new(),
            params,
        }
    }

    pub fn push(&mut self, label: Option<concrete::Label>) -> usize {
        self.depth += 1;
        if let Some(lbl) = label {
            self.labels.insert(lbl, self.depth);
        }
        self.depth
    }

    pub fn find(&self, label: Option<concrete::Label>) -> usize {
        if let Some(lbl) = label {
            if let Some(depth) = self.labels.get(&lbl) {
                return *depth;
            }
            panic!("label {:?} not found in labels stack", lbl);
        }
        self.depth
    }

    pub fn pop(&mut self, label: Option<concrete::Label>) {
        if let Some(lbl) = label {
            self.labels.remove(&lbl);
        }
        if self.depth > 0 {
            self.depth -= 1;
        }
    }

    pub fn local(&mut self, local: &'static IrValue) {
        self.locals.push(local);
    }
}

impl IrType {
    pub fn all() -> Vec<&'static IrType> {
        unsafe { IR_TYPE_STORE.as_ref().unwrap().all() }
    }

    pub fn is_valued(&self) -> bool {
        !matches!(self, IrType::Never | IrType::Unit)
    }

    pub fn id(&self) -> u64 {
        match self {
            IrType::Temp { id } => *id,
            IrType::Never => NEVER_TYPE_ID,
            IrType::Unit => UNIT_TYPE_ID,
            IrType::I32 => I32_TYPE_ID,
            IrType::F64 => F64_TYPE_ID,
            IrType::Bool => BOOL_TYPE_ID,
            IrType::Struct { id, .. } => *id,
            IrType::Tuple { id, .. } => *id,
            IrType::Array { id, .. } => *id,
            IrType::Ref { id, .. } => *id,
            IrType::Optional { id, .. } => *id,
            IrType::Fn { id, .. } => *id,
        }
    }

    pub fn store(mut self) -> &'static IrType {
        unsafe {
            match &self {
                IrType::I32 => return &IrType::I32,
                IrType::F64 => return &IrType::F64,
                IrType::Bool => return &IrType::Bool,
                IrType::Never => return &IrType::Never,
                IrType::Unit => return &IrType::Unit,
                _ => {},
            }
            let store = IR_TYPE_STORE.as_ref().unwrap();
            let id = store
                .next_id
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            match &mut self {
                IrType::Temp { id: ty_id } => *ty_id = id,
                IrType::Struct { id: ty_id, .. } => *ty_id = id,
                IrType::Tuple { id: ty_id, .. } => *ty_id = id,
                IrType::Array { id: ty_id, .. } => *ty_id = id,
                IrType::Ref { id: ty_id, .. } => *ty_id = id,
                IrType::Optional { id: ty_id, .. } => *ty_id = id,
                IrType::Fn { id: ty_id, .. } => *ty_id = id,
                IrType::I32 | IrType::F64 | IrType::Bool | IrType::Never | IrType::Unit => {
                    // primitive types already have ids
                },
            }
            store.items.insert(id, self);
            &*(&*store.items.get(&id).unwrap() as *const IrType)
        }
    }
}

impl IrValue {
    pub fn id(&self) -> u64 {
        match self {
            IrValue::Temp { id } => *id,
            IrValue::Const { id, .. } => *id,
            IrValue::Var { id, .. } => *id,
            IrValue::Fn { id, .. } => *id,
        }
    }

    pub fn ty(&self) -> &'static IrType {
        match self {
            IrValue::Temp { .. } => panic!("temp values do not have a type"),
            IrValue::Const { ty, .. } => ty,
            IrValue::Var { ty, .. } => ty,
            IrValue::Fn { ty, .. } => ty,
        }
    }

    pub fn all() -> Vec<&'static IrValue> {
        unsafe { IR_VALUE_STORE.as_ref().unwrap().all() }
    }

    pub fn all_mut() -> Vec<&'static mut IrValue> {
        unsafe { IR_VALUE_STORE.as_ref().unwrap().all_mut() }
    }

    pub fn store(mut self) -> &'static IrValue {
        unsafe {
            let store = IR_VALUE_STORE.as_ref().unwrap();
            let id = store
                .next_id
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            match &mut self {
                IrValue::Temp { id: val_id } => *val_id = id,
                IrValue::Const { id: val_id, .. } => *val_id = id,
                IrValue::Var { id: val_id, .. } => *val_id = id,
                IrValue::Fn { id: val_id, .. } => *val_id = id,
            }
            store.items.insert(id, self);
            &*(&*store.items.get(&id).unwrap() as *const IrValue)
        }
    }
}

impl Type {
    pub fn into_ir(&'static self) -> &'static IrType {
        unsafe {
            if let Some(mapping) = IR_TYPE_MAPPING.as_ref() {
                if let Some(ir_ty) = mapping.get(&self) {
                    return *ir_ty;
                }
            }
            let ir_ty = IrType::Temp { id: 0 }.store();
            let id = ir_ty.id();
            if let Some(mapping) = IR_TYPE_MAPPING.as_ref() {
                mapping.insert(self, ir_ty);
            }
            let ir_ty = IR_TYPE_STORE.as_ref().unwrap().get_mut(id).unwrap();
            *ir_ty = match self {
                Type::Never => IrType::Never,
                Type::Unit => IrType::Unit,
                Type::I32 => IrType::I32,
                Type::F64 => IrType::F64,
                Type::Bool => IrType::Bool,
                Type::Struct { fields, .. } => {
                    IrType::Struct {
                        id:     ir_ty.id(),
                        fields: fields.into_ir(),
                    }
                },
                Type::Tuple { fields, .. } => {
                    IrType::Tuple {
                        id:     ir_ty.id(),
                        fields: fields.iter().map(|ty| ty.into_ir()).collect(),
                    }
                },
                Type::Array { ty, length, .. } => {
                    IrType::Array {
                        id:     ir_ty.id(),
                        ty:     ty.into_ir(),
                        length: *length,
                    }
                },
                Type::Slice { .. } => {
                    unimplemented!("slice types are not yet supported in IR");
                },
                Type::Ref { ty, .. } => {
                    IrType::Ref {
                        id: ir_ty.id(),
                        ty: ty.into_ir(),
                    }
                },
                Type::Optional { ty, .. } => {
                    IrType::Optional {
                        id: ir_ty.id(),
                        ty: ty.into_ir(),
                    }
                },
                Type::Fn {
                    param_tys, ret_ty, ..
                } => {
                    IrType::Fn {
                        id:        ir_ty.id(),
                        param_tys: param_tys.iter().map(|ty| ty.into_ir()).collect(),
                        ret_ty:    ret_ty.into_ir(),
                    }
                },
                Type::Enum { .. } => {
                    unimplemented!("enum types are not yet supported in IR");
                },
                Type::Module { .. } => {
                    unimplemented!("module types are not supported in IR");
                },
            };
            ir_ty
        }
    }
}

impl hir::StructFields {
    pub fn into_ir(&'static self) -> StructFields {
        match self {
            hir::StructFields::Named { fields } => {
                let mut ir_fields = Vec::with_capacity(fields.len());
                for field in fields {
                    ir_fields.push(NamedStructField {
                        name: field.name.clone(),
                        ty:   field.ty.ty().into_ir(),
                    });
                }
                StructFields::Named { fields: ir_fields }
            },
            hir::StructFields::Unnamed { .. } => {
                unimplemented!("unnamed struct fields are not yet supported in IR");
            },
            hir::StructFields::Unit => {
                unimplemented!("unit struct fields are not yet supported in IR");
            },
        }
    }
}

impl Value {
    pub fn into_ir(&'static self) -> &'static IrValue {
        unsafe {
            if let Some(mapping) = IR_VALUE_MAPPING.as_ref() {
                if let Some(ir_val) = mapping.get(&self.id()) {
                    return *ir_val;
                }
            }
            let ir_val = IrValue::Temp { id: 0 }.store();
            let id = ir_val.id();
            if let Some(mapping) = IR_VALUE_MAPPING.as_ref() {
                mapping.insert(self.id(), ir_val);
            }
            let ir_val = IR_VALUE_STORE.as_ref().unwrap().get_mut(id).unwrap();
            *ir_val = match self {
                Value::Const { .. } => {
                    unimplemented!("constant values are not yet supported in IR")
                },
                Value::Var { ty, .. } => {
                    IrValue::Var {
                        id: ir_val.id(),
                        ty: ty.ty().into_ir(),
                    }
                },
                Value::Fn {
                    id,
                    ret_ty,
                    params,
                    body,
                    ty,
                    ..
                } => {
                    let mut labels =
                        Labels::new(params.iter().map(|(_, _, val)| val.unwrap()).collect());
                    IrValue::Fn {
                        id:      ir_val.id(),
                        ret_ty:  ret_ty.ty().into_ir(),
                        params:  params
                            .iter()
                            .map(|(_, _, val)| val.unwrap().into_ir())
                            .collect::<Vec<_>>(),
                        body:    Box::new(body.into_ir(&mut labels)),
                        ty:      ty.ty().into_ir(),
                        locals:  labels.locals,
                        idx:     0,
                        orig_id: *id,
                    }
                },
            };
            ir_val
        }
    }
}

impl hir::Block {
    pub fn into_ir(&'static self, labels: &mut Labels) -> Block {
        let mut stmts = Vec::new();
        let mut values = std::mem::take(&mut labels.params);
        for stmt in &self.stmts {
            let is_never = match stmt {
                hir::HirStmt::Expr(expr) => expr.ty().ty().is_never(),
                hir::HirStmt::Semi(expr) => expr.ty().ty().is_never(),
                hir::HirStmt::Let { value, .. } => {
                    values.push(value);
                    false
                },
            };
            stmts.push(stmt.into_ir(labels));
            if is_never {
                break;
            }
        }
        // our final statement
        // needs to deallocate all the locals we used in this
        // block
        let expr = if let Some(last_stmt) = stmts.pop() {
            match last_stmt {
                Stmt::Expr(expr) => expr,
                s @ (Stmt::Let { .. } | Stmt::Semi(_)) => {
                    stmts.push(s);
                    // create a unit expression
                    Expr::Literal(Literal::Unit)
                },
            }
        } else {
            // create a unit expression
            Expr::Literal(Literal::Unit)
        };
        stmts.push(Stmt::Expr(Expr::Free {
            expr:   Box::new(expr),
            values: values
                .iter()
                .filter_map(|v| {
                    Some((v.into_ir(), match v {
                        Value::Var {
                            moved,
                            partially_moved,
                            ..
                        } => {
                            if *moved {
                                return None;
                            }
                            partially_moved.clone()
                        },
                        _ => panic!("only variable values can be deallocated"),
                    }))
                })
                .collect(),
        }));
        Block {
            stmts,
            ty: self.ty.ty().into_ir(),
        }
    }
}

impl hir::HirStmt {
    pub fn into_ir(&'static self, labels: &mut Labels) -> Stmt {
        match self {
            hir::HirStmt::Let {
                ty, expr, value, ..
            } => {
                labels.local(value.into_ir());
                Stmt::Let {
                    ty:    ty.ty().into_ir(),
                    expr:  expr.as_ref().map(|e| Box::new(e.into_ir(labels))),
                    value: value.into_ir(),
                }
            },
            hir::HirStmt::Expr(expr) => Stmt::Expr(expr.into_ir(labels)),
            hir::HirStmt::Semi(expr) => Stmt::Semi(expr.into_ir(labels)),
        }
    }
}

impl hir::Expr {
    pub fn into_ir(&'static self, labels: &mut Labels) -> Expr {
        match self {
            hir::Expr::Literal(lit) => Expr::Literal(lit.into_ir()),
            hir::Expr::Path { .. } => {
                unimplemented!("path expressions are not yet supported in IR")
            },
            hir::Expr::Value { value, .. } => {
                Expr::Value {
                    value: value.into_ir(),
                }
            },
            hir::Expr::Tuple { expr, ty, .. } => {
                Expr::Tuple {
                    expr: expr.iter().map(|e| e.into_ir(labels)).collect(),
                    ty:   ty.ty().into_ir(),
                }
            },
            hir::Expr::Array { elems, ty, .. } => {
                Expr::Array {
                    elems: elems.iter().map(|e| e.into_ir(labels)).collect(),
                    ty:    ty.ty().into_ir(),
                }
            },
            hir::Expr::Struct(expr) => Expr::Struct(expr.into_ir(labels)),
            hir::Expr::Block(block) => Expr::Block(block.into_ir(labels)),
            hir::Expr::If(if_expr) => Expr::If(if_expr.into_ir(labels)),
            hir::Expr::Match { .. } => {
                unimplemented!("match expressions are not yet supported in IR");
            },
            hir::Expr::Loop {
                label, body, ty, ..
            } => {
                let lbl = labels.push(label.clone());
                let expr = Expr::Loop {
                    label: lbl,
                    body:  body.into_ir(labels),
                    ty:    ty.ty().into_ir(),
                };
                labels.pop(label.clone());
                expr
            },
            hir::Expr::For { .. } => {
                unimplemented!("for expressions are not yet supported in IR");
            },
            hir::Expr::While {
                label,
                condition,
                body,
                ty,
                ..
            } => {
                // while expressions are lowered to loops with an if inside
                let condition = condition.into_ir(labels);
                let ty = ty.ty().into_ir();
                let lbl = labels.push(label.clone());
                let body = body.into_ir(labels);
                let body = Block {
                    stmts: vec![Stmt::Expr(Expr::If(If {
                        condition: Box::new(condition),
                        then_block: body,
                        else_block: Some(ElseBlock::Block(Block {
                            stmts: vec![Stmt::Expr(Expr::LoopControl {
                                kind:  LoopControlKind::Break,
                                label: lbl,
                                expr:  None,
                            })],
                            ty:    &IrType::Never,
                        })),
                        ty,
                    }))],
                    ty,
                };
                let expr = Expr::Loop {
                    label: lbl,
                    body,
                    ty,
                };
                labels.pop(label.clone());
                expr
            },
            hir::Expr::ControlFlow {
                kind,
                label,
                expr,
                scope,
                ..
            } => {
                match kind {
                    hir::ControlFlowKind::Return => {
                        let expr = expr
                            .as_ref()
                            .map(|e| Box::new(e.into_ir(labels)))
                            .unwrap_or_else(|| Box::new(Expr::Literal(Literal::Unit)));
                        let expr = Expr::Free {
                            expr,
                            values: scope
                                .all_vars()
                                .into_iter()
                                .map(|(v, p)| (v.into_ir(), p))
                                .collect(),
                        };
                        Expr::Return {
                            expr: Some(Box::new(expr)),
                        }
                    },
                    hir::ControlFlowKind::Break => {
                        let expr = expr
                            .as_ref()
                            .map(|e| Box::new(e.into_ir(labels)))
                            .unwrap_or_else(|| Box::new(Expr::Literal(Literal::Unit)));
                        let expr = Expr::Free {
                            expr,
                            values: scope
                                .all_vars_until_break_ty(label.as_ref())
                                .into_iter()
                                .map(|(v, p)| (v.into_ir(), p))
                                .collect(),
                        };
                        Expr::LoopControl {
                            kind:  LoopControlKind::Break,
                            label: labels.find(label.clone()),
                            expr:  Some(Box::new(expr)),
                        }
                    },
                    hir::ControlFlowKind::Continue => {
                        Expr::LoopControl {
                            kind:  LoopControlKind::Continue,
                            label: labels.find(label.clone()),
                            expr:  Some(Box::new(Expr::Free {
                                expr:   Box::new(Expr::Literal(Literal::Unit)),
                                values: scope
                                    .all_vars_until_break_ty(label.as_ref())
                                    .into_iter()
                                    .map(|(v, p)| (v.into_ir(), p))
                                    .collect(),
                            })),
                        }
                    },
                }
            },
            hir::Expr::Assign {
                field,
                idx,
                op,
                expr,
                value,
                moved,
                partially_moved,
                ..
            } => {
                let expr = expr.into_ir(labels);
                let value = value.unwrap().into_ir();
                match op {
                    concrete::AssignOp::Assign => {
                        Expr::Assign {
                            value,
                            field: field.clone(),
                            idx: idx.as_ref().map(|e| Box::new(e.into_ir(labels))),
                            expr: Box::new(expr),
                            moved: *moved,
                            partially_moved: partially_moved.clone(),
                        }
                    },
                    _ => {
                        unimplemented!("compound assignment is not yet supported in IR");
                    },
                }
            },
            hir::Expr::Range { .. } => {
                unimplemented!("range expressions are not yet supported in IR");
            },
            hir::Expr::BinaryOp {
                left,
                op,
                right,
                ty,
                ..
            } => {
                Expr::BinaryOp {
                    left:  Box::new(left.into_ir(labels)),
                    op:    op.into_ir(),
                    right: Box::new(right.into_ir(labels)),
                    ty:    ty.ty().into_ir(),
                }
            },
            hir::Expr::UnaryOp { op, expr, ty, .. } => {
                Expr::UnaryOp {
                    op:   match op {
                        hir::UnaryOp::Not => UnaryOp::Not,
                        hir::UnaryOp::Neg => UnaryOp::Neg,
                        hir::UnaryOp::Ref => UnaryOp::Ref,
                    },
                    expr: Box::new(expr.into_ir(labels)),
                    ty:   ty.ty().into_ir(),
                }
            },
            hir::Expr::Unwrap { expr, ty, .. } => {
                Expr::Unwrap {
                    expr: Box::new(expr.into_ir(labels)),
                    ty:   ty.ty().into_ir(),
                }
            },
            hir::Expr::Wrap { expr, ty, .. } => {
                Expr::Wrap {
                    expr: Box::new(expr.into_ir(labels)),
                    ty:   ty.ty().into_ir(),
                }
            },
            hir::Expr::Call { func, args, ty, .. } => {
                Expr::Call {
                    func: Box::new(func.into_ir(labels)),
                    args: args.iter().map(|e| e.into_ir(labels)).collect(),
                    ty:   ty.ty().into_ir(),
                }
            },
            hir::Expr::FieldAccess {
                expr, field, ty, ..
            } => {
                Expr::FieldAccess {
                    expr:  Box::new(expr.into_ir(labels)),
                    field: field.clone(),
                    ty:    ty.ty().into_ir(),
                }
            },
            hir::Expr::MethodCall { .. } => {
                unimplemented!("method call expressions are not yet supported in IR");
            },
            hir::Expr::Index {
                expr, index, ty, ..
            } => {
                Expr::Index {
                    expr:  Box::new(expr.into_ir(labels)),
                    index: Box::new(index.into_ir(labels)),
                    ty:    ty.ty().into_ir(),
                }
            },
            hir::Expr::Cast { expr, ty, .. } => {
                Expr::Cast {
                    expr: Box::new(expr.into_ir(labels)),
                    ty:   ty.ty().into_ir(),
                }
            },
        }
    }
}

impl hir::BinaryOp {
    pub fn into_ir(&self) -> BinaryOp {
        match self {
            hir::BinaryOp::Or => BinaryOp::Or,
            hir::BinaryOp::And => BinaryOp::And,
            hir::BinaryOp::Eq => BinaryOp::Eq,
            hir::BinaryOp::Neq => BinaryOp::Neq,
            hir::BinaryOp::Lt => BinaryOp::Lt,
            hir::BinaryOp::Gt => BinaryOp::Gt,
            hir::BinaryOp::Leq => BinaryOp::Leq,
            hir::BinaryOp::Geq => BinaryOp::Geq,
            hir::BinaryOp::BitOr => BinaryOp::BitOr,
            hir::BinaryOp::BitXor => BinaryOp::BitXor,
            hir::BinaryOp::BitAnd => BinaryOp::BitAnd,
            hir::BinaryOp::Shl => BinaryOp::Shl,
            hir::BinaryOp::Shr => BinaryOp::Shr,
            hir::BinaryOp::Add => BinaryOp::Add,
            hir::BinaryOp::Sub => BinaryOp::Sub,
            hir::BinaryOp::Mul => BinaryOp::Mul,
            hir::BinaryOp::Div => BinaryOp::Div,
            hir::BinaryOp::Mod => BinaryOp::Mod,
        }
    }
}

impl hir::Literal {
    pub fn into_ir(&'static self) -> Literal {
        match self {
            hir::Literal::Unit(_) => Literal::Unit,
            hir::Literal::Int { value, ty, .. } => {
                match ty.ty() {
                    Type::I32 => {
                        match value.value.parse::<i32>() {
                            Ok(v) => Literal::I32(v),
                            Err(_) => panic!("out of range i32 literal: {}", value.value),
                        }
                    },
                    _ => unimplemented!("only i32 literals are supported in IR"),
                }
            },
            hir::Literal::Float { value, ty, .. } => {
                match ty.ty() {
                    Type::F64 => {
                        match value.value.parse::<f64>() {
                            Ok(v) => Literal::F64(v),
                            Err(_) => panic!("out of range f64 literal: {}", value.value),
                        }
                    },
                    _ => unimplemented!("only f64 literals are supported in IR"),
                }
            },
            hir::Literal::Bool { value, .. } => Literal::Bool(value.value),
            hir::Literal::Nil { ty, .. } => {
                Literal::Nil {
                    ty: ty.ty().into_ir(),
                }
            },
            hir::Literal::String { .. } => {
                unimplemented!("string literals are not yet supported in IR");
            },
            hir::Literal::Char { .. } => {
                unimplemented!("char literals are not yet supported in IR");
            },
        }
    }
}

impl hir::StructExpr {
    pub fn into_ir(&'static self, labels: &mut Labels) -> StructExpr {
        match self {
            hir::StructExpr::Named {
                fields, rest, ty, ..
            } => {
                StructExpr::Named {
                    fields: fields
                        .iter()
                        .map(|field| {
                            StructFieldExpr {
                                name: field.name.clone(),
                                expr: field.expr.into_ir(labels),
                            }
                        })
                        .collect(),
                    rest:   rest.as_ref().map(|e| Box::new(e.into_ir(labels))),
                    ty:     ty.ty().into_ir(),
                }
            },
            hir::StructExpr::Unnamed { .. } => {
                unimplemented!("unnamed struct expressions are not yet supported in IR");
            },
            hir::StructExpr::Unit { .. } => {
                unimplemented!("unit struct expressions are not yet supported in IR");
            },
        }
    }
}

impl hir::If {
    pub fn into_ir(&'static self, labels: &mut Labels) -> If {
        If {
            condition:  Box::new(self.condition.into_ir(labels)),
            then_block: self.then_block.into_ir(labels),
            else_block: self.else_block.as_ref().map(|else_block| {
                match else_block {
                    hir::ElseBlock::If(if_expr) => ElseBlock::If(Box::new(if_expr.into_ir(labels))),
                    hir::ElseBlock::Block(block) => ElseBlock::Block(block.into_ir(labels)),
                }
            }),
            ty:         self.ty.ty().into_ir(),
        }
    }
}

impl Expr {
    pub fn ty(&self) -> &'static IrType {
        match self {
            Expr::Literal(lit) => {
                match lit {
                    Literal::Unit => &IrType::Unit,
                    Literal::I32 { .. } => &IrType::I32,
                    Literal::F64 { .. } => &IrType::F64,
                    Literal::Bool { .. } => &IrType::Bool,
                    Literal::Nil { ty, .. } => ty,
                }
            },
            Expr::Value { value } => value.ty(),
            Expr::Tuple { ty, .. } => ty,
            Expr::Array { ty, .. } => ty,
            Expr::Struct(expr) => {
                match expr {
                    StructExpr::Named { ty, .. } => ty,
                }
            },
            Expr::Block(block) => block.ty,
            Expr::If(expr) => expr.ty,
            Expr::Loop { ty, .. } => ty,
            Expr::Return { .. } => &IrType::Never,
            Expr::LoopControl { .. } => &IrType::Never,
            Expr::Assign { .. } => &IrType::Unit,
            Expr::BinaryOp { ty, .. } => ty,
            Expr::UnaryOp { ty, .. } => ty,
            Expr::Unwrap { ty, .. } => ty,
            Expr::Wrap { ty, .. } => ty,
            Expr::Call { ty, .. } => ty,
            Expr::FieldAccess { ty, .. } => ty,
            Expr::Index { ty, .. } => ty,
            Expr::Cast { ty, .. } => ty,
            Expr::Free { expr, .. } => expr.ty(),
        }
    }
}
