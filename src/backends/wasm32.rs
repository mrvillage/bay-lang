#![allow(clippy::vec_init_then_push)]
use crate::{prelude::*, reprs::ir::*};

#[derive(Debug)]
#[allow(unused)]
pub enum Instr {
    Comment(&'static str),
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
    LocalGet(u64),
    LocalSet(u64),
    LocalTee(u64),

    Alloc(u32),
    Dealloc,
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
}

impl Instr {
    pub fn to_wat(&self) -> String {
        match self {
            Instr::Comment(msg) => format!(";; {}", msg),
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
            Instr::LocalGet(id) => format!("local.get $var{}", id),
            Instr::LocalSet(id) => format!("local.set $var{}", id),
            Instr::LocalTee(id) => format!("local.tee $var{}", id),
            Instr::Alloc(size) => format!("i32.const {}\ncall $alloc", size),
            Instr::Dealloc => "call $dealloc".to_string(),
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
        }
    }

    pub fn wat_compile_type(&'static self) -> String {
        let mut wat = String::new();
        match self {
            IrType::Fn {
                id,
                ret_ty,
                param_tys,
                ..
            } => {
                wat.push_str(&format!("(type $fn_type{} (func ", id));
                for param in param_tys {
                    wat.push_str(&format!("(param {}) ", param.to_wat()));
                }
                if ret_ty.is_valued() {
                    wat.push_str(&format!("(result {}) ", ret_ty.to_wat()));
                }
                wat.push_str("))\n");
            },
            IrType::Struct { id, fields } => {
                // destructor function
                wat.push_str(&format!(
                    "(func $destruct{} (param $var{TMP_VAR_ID} i32)\n",
                    id
                ));
                let mut instrs = Vec::new();
                match fields {
                    StructFields::Named { fields } => {
                        let mut offset = 0;
                        for field in fields {
                            if field.ty.wasm_is_allocated() {
                                instrs.push(Instr::LocalGet(TMP_VAR_ID));
                                instrs.push(Instr::I32Const(offset as i32));
                                instrs.push(Instr::I32Add);
                                instrs.push(Instr::I32Load);
                                instrs.push(Instr::Destruct(field.ty));
                            }
                            offset += Wasm32Backend::type_size(field.ty);
                        }
                    },
                }
                instrs.push(Instr::LocalGet(TMP_VAR_ID));
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
                    "(func $eq{} (param $var1 i32) (param $var2 i32) (result i32)\n",
                    id
                ));
                #[allow(clippy::vec_init_then_push)]
                let mut instrs = Vec::new();
                // check if they're the same pointer
                instrs.push(Instr::LocalGet(1)); // var1
                instrs.push(Instr::LocalGet(2)); // var2
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
                            instrs.push(Instr::LocalGet(1)); // var1
                            instrs.push(Instr::I32Const(offset as i32));
                            instrs.push(Instr::I32Add);
                            instrs.push(field.ty.wasm_load());
                            instrs.push(Instr::LocalGet(2)); // var2
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
                                    }
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
                wat.push_str(&format!(
                    "(func $destruct{} (param $var{TMP_VAR_ID} i32)\n",
                    id
                ));
                let mut instrs = Vec::new();
                let mut offset = 0;
                for field in fields {
                    if field.wasm_is_allocated() {
                        instrs.push(Instr::LocalGet(TMP_VAR_ID));
                        instrs.push(Instr::I32Const(offset as i32));
                        instrs.push(Instr::I32Add);
                        instrs.push(field.wasm_load());
                        instrs.push(Instr::Destruct(field));
                    }
                    offset += Wasm32Backend::type_size(field);
                }
                instrs.push(Instr::LocalGet(TMP_VAR_ID));
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
                    "(func $eq{} (param $var1 i32) (param $var2 i32) (result i32)\n",
                    id
                ));
                let mut instrs = Vec::new();
                // check if they're the same pointer
                instrs.push(Instr::LocalGet(1)); // var1
                instrs.push(Instr::LocalGet(2)); // var2
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
                    instrs.push(Instr::LocalGet(1)); // var1
                    instrs.push(Instr::I32Const(offset as i32));
                    instrs.push(Instr::I32Add);
                    instrs.push(field.wasm_load());
                    instrs.push(Instr::LocalGet(2)); // var2
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
                            }
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
                wat.push_str(&format!(
                    "(func $destruct{} (param $var{TMP_VAR_ID} i32)\n",
                    id
                ));
                let mut instrs = Vec::new();
                let elem_size = Wasm32Backend::type_size(ty);
                for i in 0..*length {
                    if ty.wasm_is_allocated() {
                        instrs.push(Instr::LocalGet(TMP_VAR_ID));
                        instrs.push(Instr::I32Const((i as u32 * elem_size) as i32));
                        instrs.push(Instr::I32Add);
                        instrs.push(ty.wasm_load());
                        instrs.push(Instr::Destruct(ty));
                    }
                }
                instrs.push(Instr::LocalGet(TMP_VAR_ID));
                instrs.push(Instr::Dealloc);
                wat.push_str(")\n");
                wat.push_str(&format!(
                    "(func $eq{} (param $var1 i32) (param $var2 i32) (result i32)\n",
                    id
                ));
                let mut instrs = Vec::new();
                // check if they're the same pointer
                instrs.push(Instr::LocalGet(1)); // var1
                instrs.push(Instr::LocalGet(2)); // var2
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
                    instrs.push(Instr::LocalGet(1)); // var1
                    instrs.push(Instr::I32Const(offset as i32));
                    instrs.push(Instr::I32Add);
                    instrs.push(ty.wasm_load());
                    instrs.push(Instr::LocalGet(2)); // var2
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
                            }
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
                    wat.push_str(&format!(
                        "(func $destruct{} (param $var{} i32)\n",
                        id, TMP_VAR_ID
                    ));
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
                            instrs.push(Instr::LocalGet(TMP_VAR_ID));
                            instrs.extend(Expr::Literal(Literal::Nil { ty }).compile_wasm());
                            instrs.push(Instr::I32Ne);
                        },
                    }
                    instrs.push(Instr::If(&IrType::Unit));
                    instrs.push(Instr::LocalGet(TMP_VAR_ID));
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
                        "(func $eq{} (param $var1 i32) (param $var2 i32) (result i32)\n",
                        id
                    ));
                    let mut instrs = Vec::new();
                    // check if they're the same pointer
                    instrs.push(Instr::LocalGet(1)); // var1
                    instrs.push(Instr::LocalGet(2)); // var2
                    instrs.push(Instr::I32Eq);
                    instrs.push(Instr::If(&IrType::Unit));
                    instrs.push(Instr::I32Const(1));
                    instrs.push(Instr::Return);
                    instrs.push(Instr::End);
                    // if either is nil, return false
                    instrs.push(Instr::LocalGet(1)); // var1
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
                    }
                    instrs.push(Instr::If(&IrType::Unit));
                    instrs.push(Instr::I32Const(0));
                    instrs.push(Instr::Return);
                    instrs.push(Instr::End);
                    instrs.push(Instr::LocalGet(2)); // var2
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
                    }
                    instrs.push(Instr::If(&IrType::Unit));
                    instrs.push(Instr::I32Const(0));
                    instrs.push(Instr::Return);
                    instrs.push(Instr::End);
                    // compare the inner values
                    instrs.push(Instr::LocalGet(1)); // var1
                    instrs.push(Instr::LocalGet(2)); // var2
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
    id:     u64,
    params: Vec<&'static IrValue>,
    ret_ty: Option<&'static IrType>,
    // need to push an i32 tmp local with the name $var_1
    locals: Vec<&'static IrValue>,
    body:   Vec<Instr>,
}

impl WasmFn {
    pub fn to_wat(&self) -> String {
        let mut wat = String::new();
        wat.push_str(&format!("(func $fn{} ", self.id));
        for param in &self.params {
            wat.push_str(&format!(
                "(param $var{} {}) ",
                param.id(),
                param.ty().to_wat()
            ));
        }
        if let Some(ret_ty) = &self.ret_ty {
            wat.push_str(&format!("(result {}) ", ret_ty.to_wat()));
        }
        wat.push_str("(local $var1 i32) "); // tmp local
        for local in &self.locals {
            wat.push_str(&format!(
                "(local $var{} {}) ",
                local.id(),
                local.ty().to_wat()
            ));
        }
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
                WasmFn {
                    id:     *id,
                    params: params.to_vec(),
                    ret_ty: if ret_ty.is_valued() {
                        Some(ret_ty)
                    } else {
                        None
                    },
                    locals: locals.to_vec(),
                    body:   body.compile_wasm(),
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
                Stmt::Let { ty, value, expr } => {
                    if let Some(expr) = expr {
                        instrs.extend(expr.compile_wasm());
                        if ty.is_valued() {
                            // if ty is unit or never, then we can't store it because it won't exist
                            instrs.push(Instr::LocalSet(value.id()));
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
                        } else {
                            instrs.push(Instr::Drop);
                        }
                    }
                },
                Stmt::Let { ty, value, expr } => {
                    if let Some(expr) = expr {
                        instrs.extend(expr.compile_wasm());
                        if ty.is_valued() {
                            // if ty is unit or never, then we can't store it because it won't exist
                            instrs.push(Instr::LocalSet(value.id()));
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
                    IrValue::Var { id, .. } => {
                        instrs.push(Instr::LocalGet(*id));
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
                instrs.push(Instr::LocalTee(TMP_VAR_ID));
                let mut offset = 0;
                for e in expr {
                    if e.ty().is_valued() {
                        instrs.push(Instr::LocalGet(TMP_VAR_ID));
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
                    instrs.push(Instr::LocalTee(TMP_VAR_ID));
                    for (i, e) in elems.iter().enumerate() {
                        instrs.push(Instr::LocalGet(TMP_VAR_ID));
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
                        instrs.push(Instr::LocalTee(TMP_VAR_ID));
                        let field_defs = match &ty {
                            IrType::Struct { fields, .. } => {
                                match fields {
                                    StructFields::Named { fields } => fields,
                                }
                            },
                            _ => panic!("Expected struct type"),
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
                            instrs.push(Instr::LocalGet(TMP_VAR_ID));
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
                    instrs.push(Instr::LocalGet(value.id()));
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
                        instrs.push(Instr::LocalTee(TMP_VAR_ID));
                        instrs.extend(expr.compile_wasm());
                        // now we have our stack ready to store, but first we need
                        instrs.push(Instr::LocalGet(TMP_VAR_ID));
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
                        instrs.push(Instr::LocalGet(value.id()));
                        instrs.push(Instr::I32Const(
                            Wasm32Backend::field_offset(value.ty(), field) as i32,
                        ));
                        instrs.push(Instr::I32Add);
                        if !moved
                            && expr.ty().wasm_is_allocated()
                            && !partially_moved.contains(field)
                        {
                            // not partially moved, so we need to call the destructor
                            instrs.push(Instr::LocalTee(TMP_VAR_ID));
                            instrs.push(expr.ty().wasm_load());
                            instrs.push(Instr::Destruct(expr.ty()));
                            instrs.push(Instr::LocalGet(TMP_VAR_ID));
                        }
                        instrs.extend(expr.compile_wasm());
                        instrs.push(expr.ty().wasm_store());
                    } else {
                        // simple variable assignment
                        instrs.extend(expr.compile_wasm());
                        if !moved && expr.ty().wasm_is_allocated() {
                            instrs.extend(
                                Expr::Free {
                                    expr:   Box::new(Expr::Literal(Literal::Unit)),
                                    values: vec![(value, partially_moved.clone())],
                                }
                                .compile_wasm(),
                            );
                        }
                        instrs.push(Instr::LocalSet(value.id()));
                    }
                } else {
                    instrs.extend(expr.compile_wasm());
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
                                }
                            },
                            IrType::Struct { .. } | IrType::Tuple { .. } | IrType::Array { .. } => {
                                instrs.push(Instr::TypeEq(ty));
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
                                }
                            },
                            IrType::Struct { .. } | IrType::Tuple { .. } | IrType::Array { .. } => {
                                instrs.push(Instr::TypeEq(ty));
                                instrs.push(Instr::I32Eqz);
                            },
                        }
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
                    (UnaryOp::Not, IrType::Bool) => {
                        instrs.extend(expr.compile_wasm());
                        instrs.push(Instr::I32Eqz);
                    },
                    (UnaryOp::Not, IrType::I32) => {
                        instrs.extend(expr.compile_wasm());
                        instrs.push(Instr::I32Const(-1));
                        instrs.push(Instr::I32Xor);
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
                        instrs.push(Instr::LocalTee(TMP_VAR_ID));
                        instrs.push(Instr::LocalGet(TMP_VAR_ID));
                        instrs.extend(Expr::Literal(Literal::Nil { ty }).compile_wasm());
                        instrs.push(Instr::I32Eq);
                    },
                    IrType::Optional { .. } => {
                        unimplemented!("Nested optionals not supported");
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
                for arg in args {
                    instrs.extend(arg.compile_wasm());
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
                            instrs.push(Instr::LocalGet(value.id()));
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
                                        instrs.push(Instr::LocalGet(value.id()));
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
                                    instrs.push(Instr::LocalGet(value.id()));
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
        instrs.push(Instr::If(self.then_block.ty));
        if self.then_block.ty.is_valued() {
            // then block
            instrs.push(Instr::Then);
        }
        instrs.extend(self.then_block.compile_wasm());
        // else block
        if let Some(else_block) = &self.else_block {
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
        instrs.push(Instr::End);
        instrs
    }
}
