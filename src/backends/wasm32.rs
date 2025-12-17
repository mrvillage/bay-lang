#![allow(clippy::vec_init_then_push)]
use crate::{prelude::*, reprs::ir::*};

#[derive(Debug)]
#[allow(unused)]
pub enum Instr {
    Comment(&'static str),
    Raw(String),
    Nop,
    Drop,
    Unreachable,
    I32Const(i32),
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32DivU,
    I32RemS,
    I32RemU,
    I32Eqz,
    I32Eq,
    I32Ne,
    I32LtS,
    I32LtU,
    I32GtS,
    I32GtU,
    I32LeS,
    I32LeU,
    I32GeS,
    I32GeU,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32ShrS,
    I32ShrU,
    F64Const(f64),
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Rem,
    F64Eq,
    F64Ne,
    F64Lt,
    F64Gt,
    F64Le,
    F64Ge,
    Local(u64, &'static IrType),
    VarGet(&'static IrValue),
    VarSet(&'static IrValue, Vec<Instr>),
    SpGet,
    StackSet(&'static IrType, u32, Vec<Instr>),
    // this is for accessing locals by index, used in some generated code
    LocalGet(u64),
    LocalSet(u64),
    TmpGet,
    TmpSet,
    TmpTee,

    Alloc(u32),
    Dealloc,
    ScAlloc(u32),
    ScDealloc(u32),
    ScPut(u32),
    Memcpy(u32),
    TypeEq(&'static IrType),
    // call the destructor for the given type, one argument is the pointer to the value
    Destruct(&'static IrType),
    I32Load,
    I32Store,
    F64Load,
    F64Store,

    If(&'static IrType),
    Then,
    Else,
    End,
    Loop(u64),
    Block(u64),

    Return,
    BrLoop(u64),
    BrBlock(u64),

    CallIndirect(&'static IrType),

    Open,
    Close,
    RangeAdd(&'static IrType),
    RangeSub(&'static IrType),
    RangeMul(&'static IrType),
    RangeDiv(&'static IrType),
    RangeMod(&'static IrType),
    RangeNeg(&'static IrType, Vec<Instr>),
    RangeNot(&'static IrType),
    RangeEq(&'static IrType),
    RangeNe(&'static IrType),
    RangeLt(&'static IrType),
    RangeLe(&'static IrType),
    RangeGt(&'static IrType),
    RangeGe(&'static IrType),
}

impl Instr {
    pub fn to_wat(&self) -> String {
        match self {
            Instr::Comment(msg) => format!(";; {}", msg),
            Instr::Raw(code) => code.clone(),
            Instr::Nop => "nop".to_string(),
            Instr::Drop => "drop".to_string(),
            Instr::Unreachable => "unreachable".to_string(),
            Instr::I32Const(val) => format!("i32.const {}", val),
            Instr::I32Add => "i32.add".to_string(),
            Instr::I32Sub => "i32.sub".to_string(),
            Instr::I32Mul => "i32.mul".to_string(),
            Instr::I32DivS => "i32.div_s".to_string(),
            Instr::I32DivU => "i32.div_u".to_string(),
            Instr::I32RemS => "i32.rem_s".to_string(),
            Instr::I32RemU => "i32.rem_u".to_string(),
            Instr::I32Eqz => "i32.eqz".to_string(),
            Instr::I32Eq => "i32.eq".to_string(),
            Instr::I32Ne => "i32.ne".to_string(),
            Instr::I32LtS => "i32.lt_s".to_string(),
            Instr::I32LtU => "i32.lt_u".to_string(),
            Instr::I32GtS => "i32.gt_s".to_string(),
            Instr::I32GtU => "i32.gt_u".to_string(),
            Instr::I32LeS => "i32.le_s".to_string(),
            Instr::I32LeU => "i32.le_u".to_string(),
            Instr::I32GeS => "i32.ge_s".to_string(),
            Instr::I32GeU => "i32.ge_u".to_string(),
            Instr::I32And => "i32.and".to_string(),
            Instr::I32Or => "i32.or".to_string(),
            Instr::I32Xor => "i32.xor".to_string(),
            Instr::I32Shl => "i32.shl".to_string(),
            Instr::I32ShrS => "i32.shr_s".to_string(),
            Instr::I32ShrU => "i32.shr_u".to_string(),
            Instr::F64Const(val) => format!("f64.const {}", val),
            Instr::F64Add => "f64.add".to_string(),
            Instr::F64Sub => "f64.sub".to_string(),
            Instr::F64Mul => "f64.mul".to_string(),
            Instr::F64Div => "f64.div".to_string(),
            Instr::F64Rem => "f64.rem".to_string(),
            Instr::F64Eq => "f64.eq".to_string(),
            Instr::F64Ne => "f64.ne".to_string(),
            Instr::F64Lt => "f64.lt".to_string(),
            Instr::F64Gt => "f64.gt".to_string(),
            Instr::F64Le => "f64.le".to_string(),
            Instr::F64Ge => "f64.ge".to_string(),
            Instr::Local(id, ty) => format!("(local $var{} {})", id, ty.to_wat()),
            Instr::VarGet(val) => {
                format!(
                    ";;here\nlocal.get $sp\ni32.const {}\ni32.add\n{};;here2",
                    val.offset(),
                    val.ty().wasm_load().to_wat()
                )
            },
            Instr::VarSet(val, instrs) => {
                // if ty is unit or never, then we can't store it because it won't exist
                if val.ty().is_valued() {
                    format!(
                        "local.get $sp\ni32.const {}\ni32.add\n{}\n{}",
                        val.offset(),
                        instrs
                            .iter()
                            .map(|x| format!("  {}\n", x.to_wat()))
                            .collect::<String>(),
                        val.ty().wasm_store().to_wat()
                    )
                } else {
                    instrs
                        .iter()
                        .map(|x| format!("  {}\n", x.to_wat()))
                        .collect::<String>()
                }
            },
            Instr::SpGet => "local.get $sp".to_string(),
            Instr::StackSet(ty, offset, instrs) => {
                if ty.is_valued() {
                    format!(
                        "local.get $sp\ni32.const {}\ni32.sub\n{}\n{}",
                        offset,
                        instrs
                            .iter()
                            .map(|x| format!("  {}\n", x.to_wat()))
                            .collect::<String>(),
                        ty.wasm_store().to_wat()
                    )
                } else {
                    "".to_string()
                }
            },
            Instr::TmpGet => "local.get $tmp".to_string(),
            Instr::TmpSet => "local.set $tmp".to_string(),
            Instr::TmpTee => "local.tee $tmp".to_string(),
            Instr::LocalGet(idx) => format!("local.get {}", idx),
            Instr::LocalSet(idx) => format!("local.set {}", idx),
            Instr::Alloc(size) => format!("i32.const {}\ncall $alloc", size),
            Instr::Dealloc => "call $dealloc".to_string(),
            Instr::ScAlloc(size) => format!("i32.const {}\ncall $sc_alloc", size),
            Instr::ScDealloc(size) => format!("i32.const {}\ncall $sc_dealloc", size),
            Instr::ScPut(size) => format!("i32.const {}\ncall $sc_put", size),
            Instr::Memcpy(size) => format!("i32.const {}\ncall $memcpy", size),
            Instr::I32Load => "i32.load".to_string(),
            Instr::I32Store => "i32.store".to_string(),
            Instr::F64Load => "f64.load".to_string(),
            Instr::F64Store => "f64.store".to_string(),
            Instr::If(ret_ty) => {
                if ret_ty.is_valued() {
                    format!("if (result {})", ret_ty.to_wat())
                } else {
                    "if".to_string()
                }
            },
            Instr::Then => "then".to_string(),
            Instr::Else => "else".to_string(),
            Instr::End => "end".to_string(),
            Instr::Loop(label) => format!("loop $loop{}", label),
            Instr::Block(label) => format!("block $block{}", label),
            Instr::Return => "return".to_string(),
            Instr::BrLoop(label) => format!("br $loop{}", label),
            Instr::BrBlock(label) => format!("br $block{}", label),
            Instr::CallIndirect(ty) => {
                if let IrType::Fn { id, .. } = ty {
                    format!("call_indirect (type $fn_type{})", id)
                } else {
                    panic!("CallIndirect expects a function type");
                }
            },
            Instr::Destruct(ty) => {
                format!("call $destruct{}", ty.id())
            },
            Instr::TypeEq(ty) => {
                format!("call $eq{}", ty.id())
            },
            Instr::Open => "(".to_string(),
            Instr::Close => ")".to_string(),
            Instr::RangeAdd(ty) => {
                let IrType::Range {
                    start,
                    end,
                    inclusive,
                    ..
                } = ty
                else {
                    panic!("RangeAdd expects a range type");
                };
                let words = words(*start, *end, *inclusive);
                let mut wat = if words == 1 {
                    "i32.add".to_string()
                } else {
                    format!("i32.const {}\ncall $range_add", words)
                };
                wat += range_checks(ty)
                    .iter()
                    .map(|x| format!("\n{}", x.to_wat()))
                    .collect::<String>()
                    .as_str();
                wat
            },
            Instr::RangeSub(ty) => {
                let IrType::Range {
                    start,
                    end,
                    inclusive,
                    ..
                } = ty
                else {
                    panic!("RangeSub expects a range type");
                };
                let words = words(*start, *end, *inclusive);
                let mut wat = if words == 1 {
                    "i32.sub".to_string()
                } else {
                    format!("i32.const {}\ncall $range_sub", words)
                };
                wat += range_checks(ty)
                    .iter()
                    .map(|x| format!("\n{}", x.to_wat()))
                    .collect::<String>()
                    .as_str();
                wat
            },
            Instr::RangeMul(ty) => {
                let IrType::Range {
                    start,
                    end,
                    inclusive,
                    ..
                } = ty
                else {
                    panic!("RangeMul expects a range type");
                };
                let words = words(*start, *end, *inclusive);
                let mut wat = if words == 1 {
                    "i32.mul".to_string()
                } else {
                    format!("i32.const {}\ncall $range_mul", words)
                };
                wat += range_checks(ty)
                    .iter()
                    .map(|x| format!("\n{}", x.to_wat()))
                    .collect::<String>()
                    .as_str();
                wat
            },
            Instr::RangeDiv(ty) => {
                let IrType::Range {
                    start,
                    end,
                    inclusive,
                    ..
                } = ty
                else {
                    panic!("RangeDiv expects a range type");
                };
                let words = words(*start, *end, *inclusive);
                let mut wat = if words == 1 {
                    "i32.div_s".to_string()
                } else {
                    format!("i32.const {}\ncall $range_div", words)
                };
                wat += range_checks(ty)
                    .iter()
                    .map(|x| format!("\n{}", x.to_wat()))
                    .collect::<String>()
                    .as_str();
                wat
            },
            Instr::RangeMod(ty) => {
                let IrType::Range {
                    start,
                    end,
                    inclusive,
                    ..
                } = ty
                else {
                    panic!("RangeMod expects a range type");
                };
                let words = words(*start, *end, *inclusive);
                let mut wat = if words == 1 {
                    "i32.rem_s".to_string()
                } else {
                    format!("i32.const {}\ncall $range_mod", words)
                };
                wat += range_checks(ty)
                    .iter()
                    .map(|x| format!("\n{}", x.to_wat()))
                    .collect::<String>()
                    .as_str();
                wat
            },
            Instr::RangeNeg(ty, instrs) => {
                let IrType::Range {
                    start,
                    end,
                    inclusive,
                    ..
                } = ty
                else {
                    panic!("RangeNeg expects a range type");
                };
                let words = words(*start, *end, *inclusive);
                let instrs = instrs
                    .iter()
                    .map(|x| format!("  {}\n", x.to_wat()))
                    .collect::<String>();
                let mut wat = if words == 1 {
                    format!("i32.const 0\n{}\ni32.sub", instrs)
                } else {
                    format!("{}\ni32.const {}\ncall $range_neg", instrs, words)
                };
                wat += range_checks(ty)
                    .iter()
                    .map(|x| format!("\n{}", x.to_wat()))
                    .collect::<String>()
                    .as_str();
                wat
            },
            Instr::RangeNot(ty) => {
                let IrType::Range {
                    start,
                    end,
                    inclusive,
                    ..
                } = ty
                else {
                    panic!("RangeNot expects a range type");
                };
                let words = words(*start, *end, *inclusive);
                let mut wat = if words == 1 {
                    "i32.const -1\ni32.xor".to_string()
                } else {
                    format!("i32.const {}\ncall $range_not", words)
                };
                wat += range_checks(ty)
                    .iter()
                    .map(|x| format!("\n{}", x.to_wat()))
                    .collect::<String>()
                    .as_str();
                wat
            },
            Instr::RangeEq(ty) => {
                let IrType::Range {
                    start,
                    end,
                    inclusive,
                    ..
                } = ty
                else {
                    panic!("RangeEq expects a range type");
                };
                let words = words(*start, *end, *inclusive);
                if words == 1 {
                    "i32.eq".to_string()
                } else {
                    format!("i32.const {}\ncall $range_eq", words)
                }
            },
            Instr::RangeNe(ty) => {
                let IrType::Range {
                    start,
                    end,
                    inclusive,
                    ..
                } = ty
                else {
                    panic!("RangeNe expects a range type");
                };
                let words = words(*start, *end, *inclusive);
                if words == 1 {
                    "i32.ne".to_string()
                } else {
                    format!("i32.const {}\ncall $range_ne", words)
                }
            },
            Instr::RangeLt(ty) => {
                let IrType::Range {
                    start,
                    end,
                    inclusive,
                    ..
                } = ty
                else {
                    panic!("RangeNot expects a range type");
                };
                let words = words(*start, *end, *inclusive);
                if words == 1 {
                    "i32.lt_s".to_string()
                } else {
                    format!("i32.const {}\ncall $range_lt", words)
                }
            },
            Instr::RangeLe(ty) => {
                let IrType::Range {
                    start,
                    end,
                    inclusive,
                    ..
                } = ty
                else {
                    panic!("RangeLe expects a range type");
                };
                let words = words(*start, *end, *inclusive);
                if words == 1 {
                    "i32.le_s".to_string()
                } else {
                    format!("i32.const {}\ncall $range_le", words)
                }
            },
            Instr::RangeGt(ty) => {
                let IrType::Range {
                    start,
                    end,
                    inclusive,
                    ..
                } = ty
                else {
                    panic!("RangeGt expects a range type");
                };
                let words = words(*start, *end, *inclusive);
                if words == 1 {
                    "i32.gt_s".to_string()
                } else {
                    format!("i32.const {}\ncall $range_gt", words)
                }
            },
            Instr::RangeGe(ty) => {
                let IrType::Range {
                    start,
                    end,
                    inclusive,
                    ..
                } = ty
                else {
                    panic!("RangeGe expects a range type");
                };
                let words = words(*start, *end, *inclusive);
                if words == 1 {
                    "i32.ge_s".to_string()
                } else {
                    format!("i32.const {}\ncall $range_ge", words)
                }
            },
        }
    }
}

impl IrType {
    pub fn to_wat(&self) -> &'static str {
        match self {
            IrType::I32 => "i32",
            IrType::F64 => "f64",
            IrType::Bool => "i32",
            IrType::Struct { .. } => "i32",
            IrType::Tuple { .. } => "i32",
            IrType::Array { .. } => "i32",
            IrType::Ref { .. } => "i32",
            IrType::Fn { .. } => "i32",
            IrType::Optional { ty, .. } => ty.to_wat(),
            _ => unimplemented!("WASM type not implemented for {:?}", self),
        }
    }

    fn wasm_store(&self) -> Instr {
        match self {
            IrType::Temp { .. } | IrType::Never | IrType::Unit => {
                panic!("Cannot store value of type {:?}", self);
            },
            IrType::I32
            | IrType::Bool
            | IrType::Struct { .. }
            | IrType::Tuple { .. }
            | IrType::Array { .. }
            | IrType::Ref { .. }
            | IrType::Fn { .. } => Instr::I32Store,
            IrType::F64 => Instr::F64Store,
            IrType::Optional { ty, .. } => ty.wasm_store(),
            IrType::Range {
                start,
                end,
                inclusive,
                ..
            } => {
                // two values on the stack, the pointer to where to store, and
                // the pointer to the range value, or a straight i32 if the range fits in one
                // word
                let words = words(*start, *end, *inclusive);
                if words == 1 {
                    Instr::I32Store
                } else {
                    Instr::Memcpy(words * 4)
                }
            },
        }
    }

    fn wasm_load(&self) -> Instr {
        match self {
            IrType::Temp { .. } | IrType::Never | IrType::Unit => {
                panic!("Cannot load value of type {:?}", self);
            },
            IrType::I32
            | IrType::Bool
            | IrType::Struct { .. }
            | IrType::Tuple { .. }
            | IrType::Array { .. }
            | IrType::Ref { .. }
            | IrType::Fn { .. } => Instr::I32Load,
            IrType::F64 => Instr::F64Load,
            IrType::Optional { ty, .. } => ty.wasm_load(),
            IrType::Range {
                start,
                end,
                inclusive,
                ..
            } => {
                // pointer to where to load from
                let words = words(*start, *end, *inclusive);
                if words == 1 {
                    Instr::I32Load
                } else {
                    Instr::ScPut(words * 4)
                }
            },
        }
    }

    fn wasm_is_allocated(&self) -> bool {
        match self {
            IrType::Temp { .. } | IrType::Never | IrType::Unit => false,
            IrType::I32 | IrType::F64 | IrType::Bool | IrType::Ref { .. } | IrType::Fn { .. } => {
                false
            },
            IrType::Struct { .. } | IrType::Tuple { .. } | IrType::Array { .. } => true,
            IrType::Optional { ty, .. } => ty.wasm_is_allocated(),
            IrType::Range { .. } => false,
        }
    }

    fn wasm_is_scratch(&self) -> bool {
        match self {
            IrType::Temp { .. } | IrType::Never | IrType::Unit => false,
            IrType::I32 | IrType::F64 | IrType::Bool | IrType::Ref { .. } | IrType::Fn { .. } => {
                false
            },
            IrType::Struct { .. } | IrType::Tuple { .. } | IrType::Array { .. } => false,
            IrType::Optional { ty, .. } => ty.wasm_is_scratch(),
            IrType::Range {
                start,
                end,
                inclusive,
                ..
            } => words(*start, *end, *inclusive) > 1,
        }
    }

    pub fn wat_compile_type(&'static self) -> String {
        let mut wat = String::new();
        match self {
            IrType::Fn {
                id,
                param_tys,
                ret_ty,
                ..
            } => {
                wat.push_str(&format!("(type $fn_type{} (func (param i32) ", id));
                if !param_tys.is_empty() {
                    if let IrType::Range { universe: true, .. } = param_tys[0] {
                        wat.push_str("(param i32) ");
                    }
                }
                if ret_ty.is_valued() {
                    wat.push_str(&format!("(result {}) ", ret_ty.to_wat()));
                }
                wat.push_str("))\n");
            },
            IrType::Struct { id, fields } => {
                // destructor function
                wat.push_str(&format!("(func $destruct{} (param $tmp i32)\n", id));
                let mut instrs = Vec::new();
                match fields {
                    StructFields::Named { fields } => {
                        let mut offset = 0;
                        for field in fields {
                            if field.ty.wasm_is_allocated() {
                                instrs.push(Instr::TmpGet);
                                instrs.push(Instr::I32Const(offset as i32));
                                instrs.push(Instr::I32Add);
                                instrs.push(Instr::I32Load);
                                instrs.push(Instr::Destruct(field.ty));
                            }
                            offset += Wasm32Backend::type_size(field.ty);
                        }
                    },
                }
                instrs.push(Instr::TmpGet);
                instrs.push(Instr::Dealloc);
                wat.push_str(
                    instrs
                        .iter()
                        .map(|x| format!("  {}\n", x.to_wat()))
                        .collect::<String>()
                        .as_str(),
                );
                wat.push_str(")\n");
                wat.push_str(&format!(
                    "(func $eq{} (param i32) (param i32) (result i32)\n",
                    id
                ));
                #[allow(clippy::vec_init_then_push)]
                let mut instrs = Vec::new();
                // check if they're the same pointer
                instrs.push(Instr::LocalGet(0)); // var1
                instrs.push(Instr::LocalGet(1)); // var2
                instrs.push(Instr::I32Eq);
                instrs.push(Instr::If(&IrType::Unit));
                instrs.push(Instr::I32Const(1));
                instrs.push(Instr::Return);
                instrs.push(Instr::End);
                match fields {
                    StructFields::Named { fields } => {
                        let mut offset = 0;
                        for field in fields {
                            if !field.ty.is_valued() {
                                offset += Wasm32Backend::type_size(field.ty);
                                continue;
                            }
                            instrs.push(Instr::LocalGet(0)); // var1
                            instrs.push(Instr::I32Const(offset as i32));
                            instrs.push(Instr::I32Add);
                            instrs.push(field.ty.wasm_load());
                            instrs.push(Instr::LocalGet(1)); // var2
                            instrs.push(Instr::I32Const(offset as i32));
                            instrs.push(Instr::I32Add);
                            instrs.push(field.ty.wasm_load());
                            match field.ty {
                                IrType::F64 => {
                                    instrs.push(Instr::F64Eq);
                                },
                                IrType::I32 | IrType::Bool | IrType::Fn { .. } => {
                                    instrs.push(Instr::I32Eq);
                                },
                                IrType::Temp { .. } | IrType::Never | IrType::Unit => {
                                    panic!(
                                        "Cannot compare field of type {:?} for equality",
                                        field.ty
                                    );
                                },
                                IrType::Ref { ty, .. } => {
                                    instrs.push(Instr::TypeEq(ty));
                                },
                                IrType::Struct { .. }
                                | IrType::Tuple { .. }
                                | IrType::Array { .. } => {
                                    instrs.push(Instr::TypeEq(field.ty));
                                },
                                IrType::Optional { ty, .. } => {
                                    match ty {
                                        IrType::F64 => {
                                            instrs.push(Instr::F64Eq);
                                        },
                                        IrType::I32 | IrType::Bool | IrType::Fn { .. } => {
                                            instrs.push(Instr::I32Eq);
                                        },
                                        IrType::Temp { .. } | IrType::Never | IrType::Unit => {
                                            panic!(
                                                "Cannot compare field of type {:?} for equality",
                                                ty
                                            );
                                        },
                                        IrType::Ref { ty, .. } => {
                                            instrs.push(Instr::TypeEq(ty));
                                        },
                                        IrType::Struct { .. }
                                        | IrType::Tuple { .. }
                                        | IrType::Array { .. } => {
                                            instrs.push(Instr::TypeEq(ty));
                                        },
                                        IrType::Optional { .. } => {
                                            unimplemented!("Nested optionals not supported");
                                        },
                                        IrType::Range { .. } => {
                                            instrs.push(Instr::RangeEq(ty));
                                        },
                                    }
                                },
                                IrType::Range { .. } => {
                                    instrs.push(Instr::RangeEq(field.ty));
                                },
                            }
                            instrs.push(Instr::If(&IrType::Unit));
                            instrs.push(Instr::Else);
                            instrs.push(Instr::I32Const(0));
                            instrs.push(Instr::Return);
                            instrs.push(Instr::End);
                            offset += Wasm32Backend::type_size(field.ty);
                        }
                    },
                }
                wat.push_str(
                    instrs
                        .iter()
                        .map(|x| format!("  {}\n", x.to_wat()))
                        .collect::<String>()
                        .as_str(),
                );
                wat.push_str("(i32.const 1))\n");
            },
            IrType::Tuple { id, fields } => {
                // destructor function
                wat.push_str(&format!("(func $destruct{} (param $tmp i32)\n", id));
                let mut instrs = Vec::new();
                let mut offset = 0;
                for field in fields {
                    if field.wasm_is_allocated() {
                        instrs.push(Instr::TmpGet);
                        instrs.push(Instr::I32Const(offset as i32));
                        instrs.push(Instr::I32Add);
                        instrs.push(field.wasm_load());
                        instrs.push(Instr::Destruct(field));
                    }
                    offset += Wasm32Backend::type_size(field);
                }
                instrs.push(Instr::TmpGet);
                instrs.push(Instr::Dealloc);
                wat.push_str(
                    instrs
                        .iter()
                        .map(|x| format!("  {}\n", x.to_wat()))
                        .collect::<String>()
                        .as_str(),
                );
                wat.push_str(")\n");
                wat.push_str(&format!(
                    "(func $eq{} (param i32) (param i32) (result i32)\n",
                    id
                ));
                let mut instrs = Vec::new();
                // check if they're the same pointer
                instrs.push(Instr::LocalGet(0)); // var1
                instrs.push(Instr::LocalGet(1)); // var2
                instrs.push(Instr::I32Eq);
                instrs.push(Instr::If(&IrType::Unit));
                instrs.push(Instr::I32Const(1));
                instrs.push(Instr::Return);
                instrs.push(Instr::End);
                let mut offset = 0;
                for field in fields {
                    if !field.is_valued() {
                        offset += Wasm32Backend::type_size(field);
                        continue;
                    }
                    instrs.push(Instr::LocalGet(0)); // var1
                    instrs.push(Instr::I32Const(offset as i32));
                    instrs.push(Instr::I32Add);
                    instrs.push(field.wasm_load());
                    instrs.push(Instr::LocalGet(1)); // var2
                    instrs.push(Instr::I32Const(offset as i32));
                    instrs.push(Instr::I32Add);
                    instrs.push(field.wasm_load());
                    match field {
                        IrType::F64 => {
                            instrs.push(Instr::F64Eq);
                        },
                        IrType::I32 | IrType::Bool | IrType::Fn { .. } => {
                            instrs.push(Instr::I32Eq);
                        },
                        IrType::Temp { .. } | IrType::Never | IrType::Unit => {
                            panic!("Cannot compare field of type {:?} for equality", field);
                        },
                        IrType::Ref { ty, .. } => {
                            instrs.push(Instr::TypeEq(ty));
                        },
                        IrType::Struct { .. } | IrType::Tuple { .. } | IrType::Array { .. } => {
                            instrs.push(Instr::TypeEq(field));
                        },
                        IrType::Optional { ty, .. } => {
                            match ty {
                                IrType::F64 => {
                                    instrs.push(Instr::F64Eq);
                                },
                                IrType::I32 | IrType::Bool | IrType::Fn { .. } => {
                                    instrs.push(Instr::I32Eq);
                                },
                                IrType::Temp { .. } | IrType::Never | IrType::Unit => {
                                    panic!("Cannot compare field of type {:?} for equality", ty);
                                },
                                IrType::Ref { ty, .. } => {
                                    instrs.push(Instr::TypeEq(ty));
                                },
                                IrType::Struct { .. }
                                | IrType::Tuple { .. }
                                | IrType::Array { .. } => {
                                    instrs.push(Instr::TypeEq(ty));
                                },
                                IrType::Optional { .. } => {
                                    unimplemented!("Nested optionals not supported");
                                },
                                IrType::Range { .. } => {
                                    instrs.push(Instr::RangeEq(ty));
                                },
                            }
                        },
                        IrType::Range { .. } => {
                            instrs.push(Instr::RangeEq(field));
                        },
                    }
                    instrs.push(Instr::If(&IrType::Unit));
                    instrs.push(Instr::Else);
                    instrs.push(Instr::I32Const(0));
                    instrs.push(Instr::Return);
                    instrs.push(Instr::End);
                    offset += Wasm32Backend::type_size(field);
                }
                wat.push_str(
                    instrs
                        .iter()
                        .map(|x| format!("  {}\n", x.to_wat()))
                        .collect::<String>()
                        .as_str(),
                );
                wat.push_str("(i32.const 1))\n");
            },
            IrType::Array { id, ty, length, .. } => {
                // destructor function
                wat.push_str(&format!("(func $destruct{} (param $tmp i32)\n", id));
                let mut instrs = Vec::new();
                let elem_size = Wasm32Backend::type_size(ty);
                for i in 0..*length {
                    if ty.wasm_is_allocated() {
                        instrs.push(Instr::TmpGet);
                        instrs.push(Instr::I32Const((i as u32 * elem_size) as i32));
                        instrs.push(Instr::I32Add);
                        instrs.push(ty.wasm_load());
                        instrs.push(Instr::Destruct(ty));
                    }
                }
                instrs.push(Instr::TmpGet);
                instrs.push(Instr::Dealloc);
                wat.push_str(")\n");
                wat.push_str(&format!(
                    "(func $eq{} (param i32) (param i32) (result i32)\n",
                    id
                ));
                let mut instrs = Vec::new();
                // check if they're the same pointer
                instrs.push(Instr::LocalGet(0)); // var1
                instrs.push(Instr::LocalGet(1)); // var2
                instrs.push(Instr::I32Eq);
                instrs.push(Instr::If(&IrType::Unit));
                instrs.push(Instr::I32Const(1));
                instrs.push(Instr::Return);
                instrs.push(Instr::End);
                let mut offset = 0;
                for _ in 0..*length {
                    if !ty.is_valued() {
                        offset += Wasm32Backend::type_size(ty);
                        continue;
                    }
                    instrs.push(Instr::LocalGet(0)); // var1
                    instrs.push(Instr::I32Const(offset as i32));
                    instrs.push(Instr::I32Add);
                    instrs.push(ty.wasm_load());
                    instrs.push(Instr::LocalGet(1)); // var2
                    instrs.push(Instr::I32Const(offset as i32));
                    instrs.push(Instr::I32Add);
                    instrs.push(ty.wasm_load());
                    match ty {
                        IrType::F64 => {
                            instrs.push(Instr::F64Eq);
                        },
                        IrType::I32 | IrType::Bool | IrType::Fn { .. } => {
                            instrs.push(Instr::I32Eq);
                        },
                        IrType::Temp { .. } | IrType::Never | IrType::Unit => {
                            panic!("Cannot compare field of type {:?} for equality", ty);
                        },
                        IrType::Ref { ty, .. } => {
                            instrs.push(Instr::TypeEq(ty));
                        },
                        IrType::Struct { .. } | IrType::Tuple { .. } | IrType::Array { .. } => {
                            instrs.push(Instr::TypeEq(ty));
                        },
                        IrType::Optional { ty, .. } => {
                            match ty {
                                IrType::F64 => {
                                    instrs.push(Instr::F64Eq);
                                },
                                IrType::I32 | IrType::Bool | IrType::Fn { .. } => {
                                    instrs.push(Instr::I32Eq);
                                },
                                IrType::Temp { .. } | IrType::Never | IrType::Unit => {
                                    panic!("Cannot compare field of type {:?} for equality", ty);
                                },
                                IrType::Ref { ty, .. } => {
                                    instrs.push(Instr::TypeEq(ty));
                                },
                                IrType::Struct { .. }
                                | IrType::Tuple { .. }
                                | IrType::Array { .. } => {
                                    instrs.push(Instr::TypeEq(ty));
                                },
                                IrType::Optional { .. } => {
                                    unimplemented!("Nested optionals not supported");
                                },
                                IrType::Range { .. } => {
                                    instrs.push(Instr::RangeEq(ty));
                                },
                            }
                        },
                        IrType::Range { .. } => {
                            instrs.push(Instr::RangeEq(ty));
                        },
                    }
                    instrs.push(Instr::If(&IrType::Unit));
                    instrs.push(Instr::Else);
                    instrs.push(Instr::I32Const(0));
                    instrs.push(Instr::Return);
                    instrs.push(Instr::End);
                    offset += Wasm32Backend::type_size(ty);
                }
                wat.push_str(
                    instrs
                        .iter()
                        .map(|x| format!("  {}\n", x.to_wat()))
                        .collect::<String>()
                        .as_str(),
                );
                wat.push_str("(i32.const 1))\n");
            },
            IrType::Optional { id, ty, .. } => {
                if ty.wasm_is_allocated() {
                    wat.push_str(&format!("(func $destruct{id} (param $tmp i32)\n"));
                    let mut instrs = Vec::new();
                    match ty {
                        IrType::F64 => {
                            panic!("f64s are not allocated");
                        },
                        IrType::Temp { .. } | IrType::Never | IrType::Unit => {
                            panic!("Cannot destruct optional of type {:?}", ty);
                        },
                        IrType::Optional { .. } => {
                            unimplemented!("Nested optionals not supported");
                        },
                        IrType::I32
                        | IrType::Bool
                        | IrType::Struct { .. }
                        | IrType::Tuple { .. }
                        | IrType::Array { .. }
                        | IrType::Ref { .. }
                        | IrType::Fn { .. } => {
                            instrs.push(Instr::TmpGet);
                            instrs.extend(Expr::Literal(Literal::Nil { ty }).compile_wasm());
                            instrs.push(Instr::I32Ne);
                        },
                        IrType::Range { .. } => {
                            instrs.push(Instr::TmpGet);
                            instrs.extend(Expr::Literal(Literal::Nil { ty }).compile_wasm());
                            instrs.push(Instr::RangeNe(ty));
                        },
                    }
                    instrs.push(Instr::If(&IrType::Unit));
                    instrs.push(Instr::TmpGet);
                    instrs.push(Instr::Destruct(ty));
                    instrs.push(Instr::End);
                    wat.push_str(
                        instrs
                            .iter()
                            .map(|x| format!("  {}\n", x.to_wat()))
                            .collect::<String>()
                            .as_str(),
                    );
                    wat.push_str(")\n");
                    wat.push_str(&format!(
                        "(func $eq{} (param i32) (param i32) (result i32)\n",
                        id
                    ));
                    let mut instrs = Vec::new();
                    // check if they're the same pointer
                    instrs.push(Instr::LocalGet(0)); // var1
                    instrs.push(Instr::LocalGet(1)); // var2
                    instrs.push(Instr::I32Eq);
                    instrs.push(Instr::If(&IrType::Unit));
                    instrs.push(Instr::I32Const(1));
                    instrs.push(Instr::Return);
                    instrs.push(Instr::End);
                    // if either is nil, return false
                    instrs.push(Instr::LocalGet(0)); // var1
                    instrs.extend(Expr::Literal(Literal::Nil { ty }).compile_wasm());
                    match ty {
                        IrType::F64 => {
                            instrs.push(Instr::F64Eq);
                        },
                        IrType::Temp { .. } | IrType::Never | IrType::Unit => {
                            panic!("Cannot compare optional of type {:?} for equality", ty);
                        },
                        IrType::Optional { .. } => {
                            unimplemented!("Nested optionals not supported");
                        },
                        IrType::I32
                        | IrType::Bool
                        | IrType::Struct { .. }
                        | IrType::Tuple { .. }
                        | IrType::Array { .. }
                        | IrType::Ref { .. }
                        | IrType::Fn { .. } => {
                            instrs.push(Instr::I32Eq);
                        },
                        IrType::Range { .. } => {
                            instrs.push(Instr::RangeEq(ty));
                        },
                    }
                    instrs.push(Instr::If(&IrType::Unit));
                    instrs.push(Instr::I32Const(0));
                    instrs.push(Instr::Return);
                    instrs.push(Instr::End);
                    instrs.push(Instr::LocalGet(1)); // var2
                    instrs.extend(Expr::Literal(Literal::Nil { ty }).compile_wasm());
                    match ty {
                        IrType::F64 => {
                            instrs.push(Instr::F64Eq);
                        },
                        IrType::Temp { .. } | IrType::Never | IrType::Unit => {
                            panic!("Cannot compare optional of type {:?} for equality", ty);
                        },
                        IrType::Optional { .. } => {
                            unimplemented!("Nested optionals not supported");
                        },
                        IrType::I32
                        | IrType::Bool
                        | IrType::Struct { .. }
                        | IrType::Tuple { .. }
                        | IrType::Array { .. }
                        | IrType::Ref { .. }
                        | IrType::Fn { .. } => {
                            instrs.push(Instr::I32Eq);
                        },
                        IrType::Range { .. } => {
                            instrs.push(Instr::RangeEq(ty));
                        },
                    }
                    instrs.push(Instr::If(&IrType::Unit));
                    instrs.push(Instr::I32Const(0));
                    instrs.push(Instr::Return);
                    instrs.push(Instr::End);
                    // compare the inner values
                    instrs.push(Instr::LocalGet(0)); // var1
                    instrs.push(Instr::LocalGet(1)); // var2
                    instrs.push(Instr::TypeEq(ty));
                    wat.push_str(
                        instrs
                            .iter()
                            .map(|x| format!("  {}\n", x.to_wat()))
                            .collect::<String>()
                            .as_str(),
                    );
                    wat.push_str(")\n");
                }
            },
            _ => {},
        }
        wat
    }
}

pub struct Wasm32Backend;

pub struct WasmFn {
    id:         u64,
    ret_ty:     Option<&'static IrType>,
    // need to push an i32 tmp local with the name $var_1
    body:       Vec<Instr>,
    stack_size: u32,
}

impl WasmFn {
    pub fn to_wat(&self) -> String {
        let mut wat = String::new();
        wat.push_str(&format!("(func $fn{} (param $sp i32)", self.id));
        if let Some(ret_ty) = &self.ret_ty {
            wat.push_str(&format!("(result {}) ", ret_ty.to_wat()));
        }
        wat.push_str("(local $tmp i32)\n"); // tmp local
        wat.push_str(&format!(
            "(local.get $sp)\n(i32.const {})\n(i32.sub)\n(local.set $sp)\n",
            self.stack_size
        ));
        wat.push('\n');
        for instr in &self.body {
            wat.push_str(&format!("  {}\n", instr.to_wat()));
        }
        wat.push_str(")\n");
        wat
    }
}

impl Wasm32Backend {
    pub fn compile_function(func: &IrValue) -> WasmFn {
        match func {
            IrValue::Fn {
                id,
                ret_ty,
                params,
                body,
                locals,
                ..
            } => {
                let mut stack_size: u32 = 0;
                for param in params {
                    stack_size += Self::type_size(param.ty());
                }
                for local in locals {
                    stack_size += Self::type_size(local.ty());
                }
                let mut offset = stack_size;
                for param in params {
                    offset -= Self::type_size(param.ty());
                    param.set_offset(offset as u64);
                }
                for local in locals {
                    offset -= Self::type_size(local.ty());
                    local.set_offset(offset as u64);
                }
                WasmFn {
                    id: *id,
                    ret_ty: if ret_ty.is_valued() {
                        Some(ret_ty)
                    } else {
                        None
                    },
                    body: body.compile_wasm(),
                    stack_size,
                }
            },
            _ => unimplemented!("WASM compilation not implemented for {:?}", func),
        }
    }

    pub fn type_size(ty: &'static IrType) -> u32 {
        match ty {
            IrType::Temp { .. } | IrType::Never | IrType::Unit => 0,
            IrType::I32 => 4,
            IrType::F64 => 8,
            IrType::Bool => 4,
            IrType::Struct { fields, .. } => {
                match fields {
                    StructFields::Named { fields } => {
                        fields.iter().map(|x| Self::inner_type_size(x.ty)).sum()
                    },
                }
            },
            IrType::Tuple { fields, .. } => fields.iter().map(|x| Self::inner_type_size(x)).sum(),
            IrType::Array { ty, length, .. } => Self::inner_type_size(ty) * (*length as u32),
            IrType::Ref { .. } => 4, // pointer
            IrType::Fn { .. } => 4,  // function pointer
            IrType::Optional { ty, .. } => Self::type_size(ty),
            IrType::Range {
                start,
                end,
                inclusive,
                ..
            } => words(*start, *end, *inclusive) * 4,
        }
    }

    pub fn inner_type_size(ty: &'static IrType) -> u32 {
        match ty {
            IrType::Struct { .. } | IrType::Tuple { .. } | IrType::Array { .. } => 4, // pointer
            IrType::Optional { ty, .. } => Self::inner_type_size(ty),
            _ => Self::type_size(ty),
        }
    }

    pub fn field_offset(struct_ty: &'static IrType, field_name: &token::Ident) -> u32 {
        match struct_ty {
            IrType::Struct { fields, .. } => {
                match fields {
                    StructFields::Named { fields } => {
                        let mut offset = 0;
                        for field in fields {
                            if &field.name == field_name {
                                return offset;
                            }
                            offset += Self::type_size(field.ty);
                        }
                        panic!("Field {} not found in struct", field_name.value);
                    },
                }
            },
            IrType::Ref { ty, .. } => Self::field_offset(ty, field_name),
            _ => panic!("Expected struct type"),
        }
    }
}

impl Block {
    pub fn compile_wasm(&self) -> Vec<Instr> {
        let mut instrs = Vec::new();
        for stmt in &self.stmts[..self.stmts.len().saturating_sub(1)] {
            match stmt {
                Stmt::Expr(expr) | Stmt::Semi(expr) => {
                    instrs.extend(expr.compile_wasm());
                    if expr.ty().is_valued() {
                        if expr.ty().wasm_is_allocated() {
                            instrs.push(Instr::Destruct(expr.ty()));
                        } else {
                            instrs.push(Instr::Drop);
                        }
                    }
                },
                Stmt::Let { value, expr, .. } => {
                    if let Some(expr) = expr {
                        instrs.push(Instr::VarSet(value, expr.compile_wasm()));
                        if expr.ty().wasm_is_scratch() {
                            instrs.push(Instr::ScDealloc(Wasm32Backend::type_size(expr.ty())));
                        }
                    }
                },
            }
        }
        if let Some(last_stmt) = self.stmts.last() {
            match last_stmt {
                Stmt::Expr(expr) => {
                    instrs.extend(expr.compile_wasm());
                },
                Stmt::Semi(expr) => {
                    instrs.extend(expr.compile_wasm());
                    if expr.ty().is_valued() {
                        if expr.ty().wasm_is_allocated() {
                            instrs.push(Instr::Destruct(expr.ty()));
                        } else if expr.ty().wasm_is_scratch() {
                            instrs.push(Instr::ScDealloc(Wasm32Backend::type_size(expr.ty())));
                        } else {
                            instrs.push(Instr::Drop);
                        }
                    }
                },
                Stmt::Let { value, expr, .. } => {
                    if let Some(expr) = expr {
                        instrs.push(Instr::VarSet(value, expr.compile_wasm()));
                        if expr.ty().wasm_is_scratch() {
                            instrs.push(Instr::ScDealloc(Wasm32Backend::type_size(expr.ty())));
                        }
                    }
                },
            }
        }
        instrs
    }
}

impl Expr {
    fn compile_wasm(&self) -> Vec<Instr> {
        let mut instrs = Vec::new();
        match self {
            Expr::Literal(lit) => {
                match lit {
                    Literal::I32(value) => {
                        instrs.push(Instr::I32Const(*value));
                    },
                    Literal::F64(value) => {
                        instrs.push(Instr::F64Const(*value));
                    },
                    Literal::Bool(value) => {
                        instrs.push(Instr::I32Const(if *value { 1 } else { 0 }));
                    },
                    Literal::Unit => {},
                    Literal::Nil { ty } => {
                        let ty = if let IrType::Optional { ty, .. } = ty {
                            ty
                        } else {
                            ty
                        };
                        match ty {
                            IrType::Temp { .. } | IrType::Never | IrType::Unit => {
                                // do nothing, no value to push
                            },
                            IrType::I32
                            | IrType::Bool
                            | IrType::Struct { .. }
                            | IrType::Tuple { .. }
                            | IrType::Array { .. }
                            | IrType::Ref { .. }
                            | IrType::Fn { .. } => {
                                instrs.push(Instr::I32Const(i32::MIN));
                            },
                            IrType::F64 => {
                                instrs.push(Instr::F64Const(f64::MAX));
                            },
                            IrType::Optional { .. } => {
                                unimplemented!("Nested optionals not supported");
                            },
                            IrType::Range {
                                start,
                                end,
                                inclusive,
                                ..
                            } => {
                                let words = words(*start, *end, *inclusive);
                                if words == 1 {
                                    instrs.push(Instr::I32Const(i32::MIN));
                                } else {
                                    let total_size = words * 4;
                                    instrs.push(Instr::ScAlloc(total_size));
                                    instrs.push(Instr::TmpTee);
                                    // for nil, we use the min value, so every word is 0 except the
                                    // last with is i32::MIN
                                    for i in 0..words {
                                        instrs.push(Instr::TmpGet);
                                        if i == words - 1 {
                                            instrs.push(Instr::I32Const(0));
                                        } else {
                                            instrs.push(Instr::I32Const(i32::MIN));
                                        }
                                    }
                                }
                            },
                        }
                    },
                    Literal::Range { val, ty } => {
                        let words = match ty {
                            IrType::Range {
                                start,
                                end,
                                inclusive,
                                ..
                            } => words(*start, *end, *inclusive),
                            _ => panic!("Expected range type"),
                        };
                        if words > 1 {
                            // allocate memory for the range
                            let total_size = words * 4;
                            instrs.push(Instr::ScAlloc(total_size));
                            instrs.push(Instr::TmpTee);
                            for i in 0..words {
                                instrs.push(Instr::TmpGet);
                                if i > 0 {
                                    instrs.push(Instr::I32Const((i * 4) as i32));
                                    instrs.push(Instr::I32Add);
                                }
                                instrs.push(Instr::I32Const(
                                    ((val >> (i * 32)) & 0xFFFFFFFFi128) as i32,
                                ));
                                instrs.push(Instr::I32Store);
                            }
                        } else {
                            // single word range, just push the word
                            instrs.push(Instr::I32Const(*val as i32));
                        }
                    },
                }
            },
            Expr::Value { value } => {
                match value {
                    IrValue::Const { .. } => {
                        unimplemented!(
                            "WASM compilation for constant values not implemented: {:?}",
                            value
                        );
                    },
                    v @ IrValue::Var { .. } => {
                        instrs.push(Instr::VarGet(v));
                    },
                    IrValue::Fn { idx, .. } => {
                        instrs.push(Instr::I32Const(*idx as i32));
                    },
                    IrValue::Temp { .. } => {
                        panic!("temp values should not appear in codegen");
                    },
                }
            },
            Expr::Tuple { expr, ty, .. } => {
                instrs.push(Instr::Alloc(Wasm32Backend::type_size(ty)));
                instrs.push(Instr::TmpTee);
                let mut offset = 0;
                for e in expr {
                    if e.ty().is_valued() {
                        instrs.push(Instr::TmpGet);
                        instrs.push(Instr::I32Const(offset as i32));
                        instrs.push(Instr::I32Add);
                        instrs.extend(e.compile_wasm());
                        instrs.push(e.ty().wasm_store());
                    }
                    offset += Wasm32Backend::type_size(e.ty());
                }
            },
            Expr::Array { elems, ty, .. } => {
                let elem_ty = match ty {
                    IrType::Array { ty, .. } => ty,
                    _ => panic!("Expected array type"),
                };
                if elem_ty.is_valued() {
                    let elem_size = Wasm32Backend::type_size(elem_ty);
                    let total_size = elem_size * (elems.len() as u32);
                    instrs.push(Instr::Alloc(total_size));
                    instrs.push(Instr::TmpTee);
                    for (i, e) in elems.iter().enumerate() {
                        instrs.push(Instr::TmpGet);
                        instrs.push(Instr::I32Const((i as u32 * elem_size) as i32));
                        instrs.push(Instr::I32Add);
                        instrs.extend(e.compile_wasm());
                        instrs.push(e.ty().wasm_store());
                    }
                }
            },
            Expr::Struct(expr) => {
                match expr {
                    StructExpr::Named { fields, rest, ty } => {
                        let struct_size = Wasm32Backend::type_size(ty);
                        instrs.push(Instr::Alloc(struct_size));
                        instrs.push(Instr::TmpTee);
                        let field_defs = match &ty {
                            IrType::Struct { fields, .. } => {
                                match fields {
                                    StructFields::Named { fields } => fields,
                                }
                            },
                            _ => panic!("Expected struct type, found {:?}", ty),
                        };
                        let mut offset = 0;
                        for field in field_defs {
                            if !field.ty.is_valued() {
                                offset += Wasm32Backend::type_size(field.ty);
                                continue;
                            }
                            // if there is no field in the expr, assign it to
                            // nil
                            let field_expr = fields.iter().find(|x| x.name == field.name);
                            instrs.push(Instr::TmpGet);
                            instrs.push(Instr::I32Const(offset as i32));
                            offset += Wasm32Backend::type_size(field.ty);
                            instrs.push(Instr::I32Add);
                            if let Some(expr) = field_expr {
                                instrs.extend(expr.expr.compile_wasm());
                            } else {
                                // push nil of field.ty
                                instrs.extend(
                                    Expr::Literal(Literal::Nil { ty: field.ty }).compile_wasm(),
                                );
                            }
                            instrs.push(field.ty.wasm_store());
                        }
                        if rest.is_some() {
                            unimplemented!("Struct rest fields not implemented");
                        }
                    },
                }
            },
            Expr::Block(block) => {
                instrs.extend(block.compile_wasm());
            },
            Expr::If(expr) => {
                instrs.extend(expr.compile_wasm());
            },
            Expr::Loop { label, body, .. } => {
                if body.ty.is_valued() {
                    unimplemented!("WASM compilation for valued loop bodies not implemented");
                }
                instrs.push(Instr::Block(*label as u64));
                instrs.push(Instr::Loop(*label as u64));
                instrs.extend(body.compile_wasm());
                instrs.push(Instr::BrLoop(*label as u64));
                instrs.push(Instr::End);
                instrs.push(Instr::End);
            },
            Expr::Return { expr } => {
                if let Some(expr) = expr {
                    instrs.extend(expr.compile_wasm());
                }
                instrs.push(Instr::Return);
            },
            Expr::LoopControl { kind, label, expr } => {
                if let Some(expr) = expr {
                    if expr.ty().is_valued() {
                        unimplemented!(
                            "WASM compilation for valued loop control expressions not implemented"
                        );
                    }
                    instrs.extend(expr.compile_wasm());
                }
                match kind {
                    LoopControlKind::Break => {
                        // break out of the block
                        instrs.push(Instr::BrBlock(*label as u64));
                    },
                    LoopControlKind::Continue => {
                        // jump to the beginning of the loop
                        instrs.push(Instr::BrLoop(*label as u64));
                    },
                }
            },
            Expr::Assign {
                value,
                field,
                idx,
                expr,
                moved,
                partially_moved,
            } => {
                if let Some(idx_expr) = idx {
                    // array index assignment
                    instrs.push(Instr::VarGet(value));
                    let arr_ty = match field {
                        Some(field_name) => {
                            // struct field which is an array
                            let field_ty = match value.ty() {
                                IrType::Struct { fields, .. } => {
                                    match fields {
                                        StructFields::Named { fields } => {
                                            let field = fields
                                                .iter()
                                                .find(|x| &x.name == field_name)
                                                .expect("Field not found in struct");
                                            field.ty
                                        },
                                    }
                                },
                                IrType::Ref {
                                    ty: IrType::Struct { fields, .. },
                                    ..
                                } => {
                                    match fields {
                                        StructFields::Named { fields } => {
                                            let field = fields
                                                .iter()
                                                .find(|x| &x.name == field_name)
                                                .expect("Field not found in struct");
                                            field.ty
                                        },
                                    }
                                },
                                _ => panic!("Expected struct type"),
                            };
                            instrs.push(Instr::I32Const(Wasm32Backend::field_offset(
                                value.ty(),
                                field_name,
                            ) as i32));
                            instrs.push(Instr::I32Add);
                            instrs.push(field_ty.wasm_load());
                            match field_ty {
                                IrType::Array { .. } => field_ty,
                                _ => panic!("Expected array type"),
                            }
                        },
                        None => value.ty(),
                    };
                    instrs.extend(idx_expr.compile_wasm());
                    let elem_ty = match arr_ty {
                        IrType::Array { ty, .. } => ty,
                        IrType::Ref {
                            ty: IrType::Array { ty, .. },
                            ..
                        } => ty,
                        _ => panic!("Expected array type"),
                    };
                    instrs.push(Instr::I32Const(Wasm32Backend::type_size(elem_ty) as i32));
                    instrs.push(Instr::I32Mul);
                    instrs.push(Instr::I32Add);
                    if elem_ty.wasm_is_allocated() {
                        // if this is an allocated type, we need to run expr,
                        // then destruct, then store
                        instrs.push(Instr::TmpTee);
                        instrs.extend(expr.compile_wasm());
                        // now we have our stack ready to store, but first we need
                        instrs.push(Instr::TmpGet);
                        instrs.push(elem_ty.wasm_load());
                        instrs.push(Instr::Destruct(elem_ty));
                        // now, our stack is ready to store again
                        instrs.push(elem_ty.wasm_store());
                    } else {
                        // otherwise we can just store
                        instrs.extend(expr.compile_wasm());
                        instrs.push(elem_ty.wasm_store());
                    }
                } else if expr.ty().is_valued() {
                    if let Some(field) = field {
                        // struct field assignment
                        instrs.push(Instr::VarGet(value));
                        instrs.push(Instr::I32Const(
                            Wasm32Backend::field_offset(value.ty(), field) as i32,
                        ));
                        instrs.push(Instr::I32Add);
                        if !moved
                            && expr.ty().wasm_is_allocated()
                            && !partially_moved.contains(field)
                        {
                            let field_ty = match value.ty() {
                                IrType::Struct { fields, .. } => {
                                    match fields {
                                        StructFields::Named { fields } => {
                                            let field_def = fields
                                                .iter()
                                                .find(|x| &x.name == field)
                                                .expect("Field not found in struct");
                                            field_def.ty
                                        },
                                    }
                                },
                                IrType::Ref {
                                    ty: IrType::Struct { fields, .. },
                                    ..
                                } => {
                                    match fields {
                                        StructFields::Named { fields } => {
                                            let field_def = fields
                                                .iter()
                                                .find(|x| &x.name == field)
                                                .expect("Field not found in struct");
                                            field_def.ty
                                        },
                                    }
                                },
                                _ => panic!("Expected struct type"),
                            };
                            // not partially moved, so we need to call the destructor
                            instrs.push(Instr::TmpTee);
                            instrs.push(field_ty.wasm_load());
                            instrs.push(Instr::Destruct(field_ty));
                            instrs.push(Instr::TmpGet);
                        }
                        instrs.extend(expr.compile_wasm());
                        instrs.push(expr.ty().wasm_store());
                    } else {
                        // simple variable assignment
                        let mut ins = expr.compile_wasm();
                        if !moved && expr.ty().wasm_is_allocated() {
                            ins.extend(
                                Expr::Free {
                                    expr:   Box::new(Expr::Literal(Literal::Unit)),
                                    values: vec![(value, partially_moved.clone())],
                                }
                                .compile_wasm(),
                            );
                        }
                        instrs.push(Instr::VarSet(value, ins));
                    }
                } else {
                    instrs.extend(expr.compile_wasm());
                }
                if expr.ty().wasm_is_scratch() {
                    instrs.push(Instr::ScDealloc(Wasm32Backend::type_size(expr.ty())));
                }
            },
            Expr::BinaryOp {
                left, op, right, ..
            } => {
                instrs.extend(left.compile_wasm());
                instrs.extend(right.compile_wasm());
                match (op, left.ty()) {
                    (BinaryOp::Add, IrType::I32) => instrs.push(Instr::I32Add),
                    (BinaryOp::Sub, IrType::I32) => instrs.push(Instr::I32Sub),
                    (BinaryOp::Mul, IrType::I32) => instrs.push(Instr::I32Mul),
                    (BinaryOp::Div, IrType::I32) => instrs.push(Instr::I32DivS),
                    (BinaryOp::Mod, IrType::I32) => instrs.push(Instr::I32RemS),
                    (BinaryOp::Add, IrType::F64) => instrs.push(Instr::F64Add),
                    (BinaryOp::Sub, IrType::F64) => instrs.push(Instr::F64Sub),
                    (BinaryOp::Mul, IrType::F64) => instrs.push(Instr::F64Mul),
                    (BinaryOp::Div, IrType::F64) => instrs.push(Instr::F64Div),
                    (BinaryOp::Mod, IrType::F64) => instrs.push(Instr::F64Rem),

                    (BinaryOp::And, IrType::Bool) => instrs.push(Instr::I32And),
                    (BinaryOp::Or, IrType::Bool) => instrs.push(Instr::I32Or),

                    (BinaryOp::Lt, IrType::I32) => instrs.push(Instr::I32LtS),
                    (BinaryOp::Leq, IrType::I32) => instrs.push(Instr::I32LeS),
                    (BinaryOp::Gt, IrType::I32) => instrs.push(Instr::I32GtS),
                    (BinaryOp::Geq, IrType::I32) => instrs.push(Instr::I32GeS),
                    (BinaryOp::Lt, IrType::F64) => instrs.push(Instr::F64Lt),
                    (BinaryOp::Leq, IrType::F64) => instrs.push(Instr::F64Le),
                    (BinaryOp::Gt, IrType::F64) => instrs.push(Instr::F64Gt),
                    (BinaryOp::Geq, IrType::F64) => instrs.push(Instr::F64Ge),

                    (BinaryOp::BitOr, IrType::I32) => instrs.push(Instr::I32Or),
                    (BinaryOp::BitAnd, IrType::I32) => instrs.push(Instr::I32And),
                    (BinaryOp::BitXor, IrType::I32) => instrs.push(Instr::I32Xor),
                    (BinaryOp::Shl, IrType::I32) => instrs.push(Instr::I32Shl),
                    (BinaryOp::Shr, IrType::I32) => instrs.push(Instr::I32ShrS),
                    (BinaryOp::Eq, ty) => {
                        match ty {
                            IrType::I32 | IrType::Bool | IrType::Fn { .. } => {
                                instrs.push(Instr::I32Eq);
                            },
                            IrType::F64 => {
                                instrs.push(Instr::F64Eq);
                            },
                            IrType::Temp { .. } | IrType::Never | IrType::Unit => {
                                panic!("Cannot compare values of type {:?} for equality", ty);
                            },
                            IrType::Ref { ty, .. } => {
                                instrs.push(Instr::TypeEq(ty));
                            },
                            IrType::Optional { ty: inner_ty, .. } => {
                                match inner_ty {
                                    IrType::F64 => {
                                        instrs.push(Instr::F64Eq);
                                    },
                                    IrType::I32 | IrType::Bool | IrType::Fn { .. } => {
                                        instrs.push(Instr::I32Eq);
                                    },
                                    IrType::Temp { .. } | IrType::Never | IrType::Unit => {
                                        panic!(
                                            "Cannot compare values of type {:?} for equality",
                                            ty
                                        );
                                    },
                                    IrType::Ref { ty, .. } => {
                                        instrs.push(Instr::TypeEq(ty));
                                    },
                                    IrType::Struct { .. }
                                    | IrType::Tuple { .. }
                                    | IrType::Array { .. } => {
                                        instrs.push(Instr::TypeEq(ty));
                                    },
                                    IrType::Optional { .. } => {
                                        unimplemented!("Nested optionals not supported");
                                    },
                                    IrType::Range { .. } => {
                                        instrs.push(Instr::RangeEq(ty));
                                    },
                                }
                            },
                            IrType::Struct { .. } | IrType::Tuple { .. } | IrType::Array { .. } => {
                                instrs.push(Instr::TypeEq(ty));
                            },
                            IrType::Range { .. } => {
                                instrs.push(Instr::RangeEq(ty));
                            },
                        }
                    },
                    (BinaryOp::Neq, ty) => {
                        match ty {
                            IrType::I32 | IrType::Bool | IrType::Fn { .. } => {
                                instrs.push(Instr::I32Ne);
                            },
                            IrType::F64 => {
                                instrs.push(Instr::F64Ne);
                            },
                            IrType::Temp { .. } | IrType::Never | IrType::Unit => {
                                panic!("Cannot compare values of type {:?} for inequality", ty);
                            },
                            IrType::Ref { ty, .. } => {
                                instrs.push(Instr::TypeEq(ty));
                                instrs.push(Instr::I32Eqz);
                            },
                            IrType::Optional { ty: inner_ty, .. } => {
                                match inner_ty {
                                    IrType::F64 => {
                                        instrs.push(Instr::F64Ne);
                                    },
                                    IrType::I32 | IrType::Bool | IrType::Fn { .. } => {
                                        instrs.push(Instr::I32Ne);
                                    },
                                    IrType::Temp { .. } | IrType::Never | IrType::Unit => {
                                        panic!(
                                            "Cannot compare values of type {:?} for inequality",
                                            ty
                                        );
                                    },
                                    IrType::Ref { ty, .. } => {
                                        instrs.push(Instr::TypeEq(ty));
                                        instrs.push(Instr::I32Eqz);
                                    },
                                    IrType::Struct { .. }
                                    | IrType::Tuple { .. }
                                    | IrType::Array { .. } => {
                                        instrs.push(Instr::TypeEq(ty));
                                        instrs.push(Instr::I32Eqz);
                                    },
                                    IrType::Optional { .. } => {
                                        unimplemented!("Nested optionals not supported");
                                    },
                                    IrType::Range { .. } => {
                                        instrs.push(Instr::RangeNe(ty));
                                    },
                                }
                            },
                            IrType::Struct { .. } | IrType::Tuple { .. } | IrType::Array { .. } => {
                                instrs.push(Instr::TypeEq(ty));
                                instrs.push(Instr::I32Eqz);
                            },
                            IrType::Range { .. } => {
                                instrs.push(Instr::RangeNe(ty));
                            },
                        }
                    },
                    (BinaryOp::Add, ty @ IrType::Range { .. }) => {
                        instrs.push(Instr::RangeAdd(ty));
                    },
                    (BinaryOp::Sub, ty @ IrType::Range { .. }) => {
                        instrs.push(Instr::RangeSub(ty));
                    },
                    (BinaryOp::Mul, ty @ IrType::Range { .. }) => {
                        instrs.push(Instr::RangeMul(ty));
                    },
                    (BinaryOp::Div, ty @ IrType::Range { .. }) => {
                        instrs.push(Instr::RangeDiv(ty));
                    },
                    (BinaryOp::Mod, ty @ IrType::Range { .. }) => {
                        instrs.push(Instr::RangeMod(ty));
                    },
                    (BinaryOp::Lt, IrType::Range { .. }) => {
                        instrs.push(Instr::RangeLt(left.ty()));
                    },
                    (BinaryOp::Leq, IrType::Range { .. }) => {
                        instrs.push(Instr::RangeLe(left.ty()));
                    },
                    (BinaryOp::Gt, IrType::Range { .. }) => {
                        instrs.push(Instr::RangeGt(left.ty()));
                    },
                    (BinaryOp::Geq, IrType::Range { .. }) => {
                        instrs.push(Instr::RangeGe(left.ty()));
                    },

                    _ => {
                        unimplemented!(
                            "WASM compilation not implemented for binary op {:?} with type {:?}",
                            op,
                            left.ty()
                        );
                    },
                }
            },
            Expr::UnaryOp { op, expr, ty } => {
                match (op, expr.ty()) {
                    (UnaryOp::Neg, IrType::I32) => {
                        instrs.push(Instr::I32Const(0));
                        instrs.extend(expr.compile_wasm());
                        instrs.push(Instr::I32Sub);
                    },
                    (UnaryOp::Neg, IrType::F64) => {
                        instrs.push(Instr::F64Const(0.0));
                        instrs.extend(expr.compile_wasm());
                        instrs.push(Instr::F64Sub);
                    },
                    (UnaryOp::Neg, IrType::Range { .. }) => {
                        instrs.push(Instr::RangeNeg(ty, expr.compile_wasm()));
                    },
                    (UnaryOp::Not, IrType::Bool) => {
                        instrs.extend(expr.compile_wasm());
                        instrs.push(Instr::I32Eqz);
                    },
                    (UnaryOp::Not, IrType::I32) => {
                        instrs.extend(expr.compile_wasm());
                        instrs.push(Instr::I32Const(-1));
                        instrs.push(Instr::I32Xor);
                    },
                    (UnaryOp::Not, IrType::Range { .. }) => {
                        instrs.extend(expr.compile_wasm());
                        instrs.push(Instr::RangeNot(ty));
                    },
                    (UnaryOp::Ref, IrType::Struct { .. })
                    | (UnaryOp::Ref, IrType::Tuple { .. }) => {
                        // since structs and tuples are just pointers to the heap, all we need to
                        // do is compile the inner expression
                        instrs.extend(expr.compile_wasm());
                    },
                    _ => {
                        unimplemented!(
                            "WASM compilation not implemented for unary op {:?} with type {:?}",
                            op,
                            ty
                        );
                    },
                }
            },
            Expr::Unwrap { expr, ty } => {
                instrs.extend(expr.compile_wasm());
                match ty {
                    IrType::F64 => {
                        unimplemented!("unwrapping optional f64 type not implemented");
                    },
                    IrType::Temp { .. } | IrType::Never | IrType::Unit => {
                        panic!("Cannot unwrap optional of type {:?}", ty);
                    },
                    IrType::I32
                    | IrType::Bool
                    | IrType::Struct { .. }
                    | IrType::Tuple { .. }
                    | IrType::Array { .. }
                    | IrType::Ref { .. }
                    | IrType::Fn { .. } => {
                        instrs.push(Instr::TmpTee);
                        instrs.push(Instr::TmpGet);
                        instrs.extend(Expr::Literal(Literal::Nil { ty }).compile_wasm());
                        instrs.push(Instr::I32Eq);
                    },
                    IrType::Optional { .. } => {
                        unimplemented!("Nested optionals not supported");
                    },
                    IrType::Range { .. } => {
                        instrs.push(Instr::TmpTee);
                        instrs.push(Instr::TmpGet);
                        instrs.extend(Expr::Literal(Literal::Nil { ty }).compile_wasm());
                        instrs.push(Instr::RangeEq(ty));
                    },
                }
                instrs.push(Instr::If(&IrType::Unit));
                instrs.push(Instr::Unreachable);
                instrs.push(Instr::End);
            },
            Expr::Wrap { expr, .. } => {
                instrs.extend(expr.compile_wasm());
            },
            Expr::Call { func, args, .. } => {
                let mut offset = 0;
                for arg in args {
                    offset += Wasm32Backend::type_size(arg.ty());
                    instrs.push(Instr::StackSet(arg.ty(), offset, arg.compile_wasm()));
                    if arg.ty().wasm_is_scratch() {
                        instrs.push(Instr::ScDealloc(Wasm32Backend::type_size(arg.ty())));
                    }
                }
                instrs.push(Instr::SpGet);
                if let IrType::Fn { param_tys, .. } = func.ty() {
                    if !param_tys.is_empty() {
                        if let IrType::Range { universe: true, .. } = param_tys[0] {
                            // if we have a universe range as the first parameter, we need to
                            // append the number of words used to represent the range
                            let words = match args[0].ty() {
                                IrType::Range {
                                    start,
                                    end,
                                    inclusive,
                                    ..
                                } => words(*start, *end, *inclusive),
                                IrType::I32 => 1,
                                _ => {
                                    panic!(
                                        "Expected range or i32 type for universe range parameter"
                                    )
                                },
                            };
                            instrs.push(Instr::I32Const(words as i32));
                        }
                    }
                }
                instrs.extend(func.compile_wasm());
                instrs.push(Instr::CallIndirect(func.ty()));
            },
            Expr::FieldAccess { expr, field, ty } => {
                instrs.extend(expr.compile_wasm());
                instrs.push(Instr::I32Const(
                    Wasm32Backend::field_offset(expr.ty(), field) as i32,
                ));
                instrs.push(Instr::I32Add);
                instrs.push(ty.wasm_load());
            },
            Expr::Index { expr, index, ty } => {
                instrs.extend(expr.compile_wasm());
                instrs.extend(index.compile_wasm());
                // range check
                if let IrType::Array { length, .. } = expr.ty() {
                    instrs.push(Instr::TmpTee);
                    instrs.push(Instr::I32Const(*length as i32));
                    instrs.push(Instr::I32GeU);
                    instrs.push(Instr::If(&IrType::Unit));
                    instrs.push(Instr::Unreachable);
                    instrs.push(Instr::End);
                    instrs.push(Instr::TmpGet);
                } else {
                    panic!("Expected array type for indexing");
                }
                let elem_size = match expr.ty() {
                    IrType::Array { ty, .. } => Wasm32Backend::type_size(ty),
                    _ => panic!("Expected array type"),
                };
                instrs.push(Instr::I32Const(elem_size as i32));
                instrs.push(Instr::I32Mul);
                instrs.push(Instr::I32Add);
                instrs.push(ty.wasm_load());
            },
            Expr::Cast { expr, .. } => {
                instrs.extend(expr.compile_wasm());
            },
            Expr::Free { expr, values } => {
                instrs.extend(expr.compile_wasm());
                for (value, partially_moved) in values.iter().rev() {
                    if value.ty().wasm_is_allocated() {
                        if partially_moved.is_empty() {
                            instrs.push(Instr::VarGet(value));
                            instrs.push(Instr::Destruct(value.ty()));
                        } else {
                            match value.ty() {
                                IrType::Struct { fields, .. } => {
                                    let field_defs = match fields {
                                        StructFields::Named { fields } => fields,
                                    };
                                    for field in field_defs {
                                        if partially_moved.contains(&field.name)
                                            || !field.ty.wasm_is_allocated()
                                        {
                                            continue;
                                        }
                                        instrs.push(Instr::VarGet(value));
                                        instrs.push(Instr::I32Const(Wasm32Backend::field_offset(
                                            value.ty(),
                                            &field.name,
                                        )
                                            as i32));
                                        instrs.push(Instr::I32Add);
                                        instrs.push(field.ty.wasm_load());
                                        instrs.push(Instr::Destruct(field.ty));
                                    }
                                    // we need to manually deallocate the struct itself
                                    instrs.push(Instr::VarGet(value));
                                    instrs.push(Instr::Dealloc);
                                },
                                _ => {
                                    panic!("Partial frees only supported for structs");
                                },
                            }
                        }
                    }
                }
            },
        }
        instrs
    }
}

impl If {
    fn compile_wasm(&self) -> Vec<Instr> {
        let mut instrs = Vec::new();
        // compile condition
        instrs.extend(self.condition.compile_wasm());
        instrs.push(Instr::Open);
        instrs.push(Instr::If(self.then_block.ty));
        // if self.then_block.ty.is_valued() {
        // then block
        instrs.push(Instr::Open);
        instrs.push(Instr::Then);
        // }
        instrs.extend(self.then_block.compile_wasm());
        // else block
        if let Some(else_block) = &self.else_block {
            instrs.push(Instr::Close);
            instrs.push(Instr::Open);
            instrs.push(Instr::Else);
            match else_block {
                ElseBlock::Block(block) => {
                    instrs.extend(block.compile_wasm());
                },
                ElseBlock::If(if_expr) => {
                    instrs.extend(if_expr.compile_wasm());
                },
            }
        }
        instrs.push(Instr::Close);
        instrs.push(Instr::Close);
        instrs
    }
}

// the number of 32 bit words needed to represent the given integer
fn words(mut start: i128, end: i128, inclusive: bool) -> u32 {
    start = start.saturating_sub(1);
    words_to_represent(start).max(words_to_represent(if inclusive { end } else { end - 1 }))
}

fn words_to_represent(num: i128) -> u32 {
    if num == 0 {
        1
    } else if num > 0 {
        if num < (1i128 << 31) {
            1
        } else if num < (1i128 << 63) {
            2
        } else if num < (1i128 << 95) {
            3
        } else {
            4
        }
    } else {
        // negative number
        if num >= -(1i128 << 31) {
            1
        } else if num >= -(1i128 << 63) {
            2
        } else if num >= -(1i128 << 95) {
            3
        } else {
            4
        }
    }
}

fn range_checks(ty: &'static IrType) -> Vec<Instr> {
    let IrType::Range {
        start,
        end,
        inclusive,
        ..
    } = ty
    else {
        panic!("Expected range type");
    };
    let words = words(*start, *end, *inclusive);
    let mut instrs = Vec::new();
    instrs.push(Instr::TmpTee);
    if words == 1 {
        instrs.push(Instr::TmpGet);
        if *start != 0 {
            instrs.push(Instr::I32Const(*start as i32));
            instrs.push(Instr::I32Sub);
        }
        instrs.push(Instr::I32Const((end - start) as i32));
        instrs.push(if *inclusive {
            Instr::I32GtU
        } else {
            Instr::I32GeU
        });
        instrs.push(Instr::If(&IrType::Unit));
        instrs.push(Instr::Unreachable);
        instrs.push(Instr::End);
    } else {
        instrs.push(Instr::Comment("TEST"));
        instrs.push(Instr::TmpGet);
        instrs.extend(Expr::Literal(Literal::Range { val: *start, ty }).compile_wasm());
        instrs.push(Instr::RangeLt(ty));
        instrs.push(Instr::If(&IrType::Unit));
        instrs.push(Instr::Unreachable);
        instrs.push(Instr::End);
        // the range cmp functions deallocate both words, we want to keep the first
        // though since this is a range check not a typical comparison
        instrs.push(Instr::ScAlloc(4 * words));
        instrs.push(Instr::Drop);
        // $tmp is now the value of start, but we have our left on the stack, we so we
        // can tee again
        instrs.push(Instr::TmpTee);
        instrs.push(Instr::TmpGet);
        instrs.extend(Expr::Literal(Literal::Range { val: *end, ty }).compile_wasm());
        if *inclusive {
            instrs.push(Instr::RangeGt(ty));
        } else {
            instrs.push(Instr::RangeGe(ty));
        }
        instrs.push(Instr::TmpTee);
        instrs.push(Instr::If(&IrType::Unit));
        instrs.push(Instr::Unreachable);
        instrs.push(Instr::End);
        // the range cmp functions deallocate both words, we want to keep the first
        // though since this is a range check not a typical comparison
        instrs.push(Instr::ScAlloc(4 * words));
        instrs.push(Instr::Drop);
    }
    instrs
}
