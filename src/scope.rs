use std::collections::HashSet;

use dashmap::DashMap;

use crate::{
    concrete::Visibility,
    hir::Expr,
    prelude::*,
    store::{InitStore, Store},
};

// scopes contain type and value namespaces
// every scope has a parent scope, all the way up to the root scope which has no
// parent scopes are created for every module, function, block, etc.
// some scopes introduce new namespaces, like modules and functions, meaning
// they do not inherit from other scopes except the root scope
// some scopes however, like blocks, inherit from their parent scope, and
// others, like items defined in a function, inherit types from their parent
// scope but not values (except for consts)

pub struct Scope {
    pub id: u64,
    pub type_parent: Option<&'static Scope>,
    // this also includes modules
    pub type_ns: Option<DashMap<&'static str, &'static Type>>,
    pub unresolved_aliases: Option<DashMap<&'static str, concrete::ConcreteType>>,

    pub value_parent: Option<&'static Scope>,
    pub value_ns:     Option<DashMap<&'static str, &'static Value>>,
    pub includes:     Vec<&'static Scope>, /* also includes these scopes, this is for `use`
                                            * statements */
    // if this scope is for a module, this is None
    pub module_scope: Option<&'static Scope>,
    pub use_items:    Vec<concrete::UseItem>,
    pub return_ty:    Option<&'static Type>,
    pub break_ty:     Option<(Option<concrete::Label>, MaybeType)>,
}

impl std::fmt::Debug for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Scope")
            .field("id", &self.id)
            // .field("type_parent", &self.type_parent.as_ref().map(|s| s.id))
            // .field("type_ns", &self.type_ns.as_ref().map(|ns| ns.len()))
            // .field("value_parent", &self.value_parent.as_ref().map(|s| s.id))
            // .field("value_ns", &self.value_ns)
            // .field(
            //     "includes",
            //     &self.includes.iter().map(|s| s.id).collect::<Vec<_>>(),
            // )
            // .field("module_scope", &self.module_scope.as_ref().map(|s| s.id))
            // .field("use_items", &self.use_items)
            .finish()
    }
}

impl PartialEq for Scope {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Scope {}

pub static mut SCOPE_STORE: Option<Store<Scope>> = None;

impl InitStore for Scope {
    fn init_store(store: &mut Store<Scope>) {
        // create the root scope
        let type_ns = DashMap::new();
        type_ns.insert("i32", Type::get_by_id(I32_TYPE_ID).unwrap());
        type_ns.insert("f64", Type::get_by_id(F64_TYPE_ID).unwrap());
        type_ns.insert("bool", Type::get_by_id(BOOL_TYPE_ID).unwrap());
        let root_scope = Scope {
            id: 0,
            type_parent: None,
            type_ns: Some(type_ns),
            unresolved_aliases: None,
            value_parent: None,
            value_ns: None,
            includes: Vec::new(),
            module_scope: None,
            use_items: Vec::new(),
            return_ty: None,
            break_ty: None,
        };
        store.items.insert(0, root_scope);
        store.next_id.store(1, std::sync::atomic::Ordering::Relaxed);
    }
}

pub static mut TYPE_STORE: Option<Store<Type>> = None;

#[derive(Debug, Clone)]
pub enum MaybeType {
    Inferred,
    Unresolved(concrete::ConcreteType, &'static Scope),
    Resolved(&'static Type),
}

pub enum Type {
    Never,
    Unit,
    I32,
    F64,
    Bool,
    Struct {
        id:         u64,
        visibility: Visibility,
        name:       token::Ident,
        fields:     hir::StructFields,
        // the scope where this struct is defined, used for resolving field types
        scope:      &'static Scope,
    },
    Enum {
        id:         u64,
        visibility: Visibility,
        name:       token::Ident,
        variants:   Vec<hir::EnumVariant>,
        ty:         MaybeType, // the underlying type of the enum, defaults to i32
        // the scope where this enum is defined, used for resolving field types
        scope:      &'static Scope,
    },
    Module {
        id:         u64,
        visibility: Visibility,
        name:       token::Ident,
        scope:      &'static Scope,
    },
    Tuple {
        id:     u64,
        fields: Vec<&'static Type>,
        scope:  &'static Scope,
    },
    Array {
        id:     u64,
        ty:     &'static Type,
        length: usize,
        scope:  &'static Scope,
    },
    Slice {
        id:    u64,
        ty:    &'static Type,
        scope: &'static Scope,
    },
    Ref {
        id:    u64,
        ty:    &'static Type,
        scope: &'static Scope,
    },
    Optional {
        id:    u64,
        ty:    &'static Type,
        scope: &'static Scope,
    },
    Fn {
        id:         u64,
        visibility: Visibility,
        param_tys:  Vec<&'static Type>,
        ret_ty:     &'static Type,
        scope:      &'static Scope,
    },
    Range {
        id:        u64,
        start:     i128,
        end:       i128,
        inclusive: bool,
        universe:  bool,
    },
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Never => write!(f, "Never"),
            Type::Unit => write!(f, "Unit"),
            Type::I32 => write!(f, "i32"),
            Type::F64 => write!(f, "f64"),
            Type::Bool => write!(f, "bool"),
            Type::Struct {
                id, name, fields, ..
            } => {
                match fields {
                    hir::StructFields::Named { fields } => {
                        write!(f, "Struct({}, {}, {{", id, name.value)?;
                        for (i, field) in fields.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(
                                f,
                                "{}: {} id={:?}",
                                field.name.value,
                                field.ty.name(),
                                field.ty.id()
                            )?;
                        }
                        write!(f, "}})")
                    },
                    hir::StructFields::Unnamed { fields } => {
                        write!(f, "Struct({}, {}, (", id, name.value)?;
                        for (i, field) in fields.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{} id={:?}", field.ty.name(), field.ty.id())?;
                        }
                        write!(f, "))")
                    },
                    hir::StructFields::Unit => write!(f, "Struct({}, {}, ())", id, name.value),
                }
            },
            Type::Enum { id, name, .. } => write!(f, "Enum({}, {})", id, name.value),
            Type::Module { id, .. } => write!(f, "Module({})", id),
            Type::Tuple { id, fields, .. } => {
                write!(f, "Tuple({}, [", id)?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", field)?;
                }
                write!(f, "])")
            },
            Type::Array { id, ty, length, .. } => {
                write!(f, "Array({}, {:?}, {})", id, ty, length)
            },
            Type::Slice { id, ty, .. } => write!(f, "Slice({}, {:?})", id, ty),
            Type::Ref { id, ty, .. } => write!(f, "Ref({}, {:?})", id, ty),
            Type::Optional { id, ty, .. } => write!(f, "Optional({}, {:?})", id, ty),
            Type::Fn {
                id,
                param_tys,
                ret_ty,
                ..
            } => {
                write!(f, "Fn({}, [", id)?;
                for (i, param_ty) in param_tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", param_ty)?;
                }
                write!(f, "] -> {:?})", ret_ty)
            },
            Type::Range {
                id,
                start,
                end,
                inclusive,
                universe,
            } => {
                if *universe {
                    write!(f, "UniverseRange({})", id)
                } else {
                    write!(
                        f,
                        "Range({}, {}..{}{})",
                        id,
                        start,
                        if *inclusive { "=" } else { "" },
                        end,
                    )
                }
            },
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Never, Type::Never) => true,
            (Type::Unit, Type::Unit) => true,
            (Type::I32, Type::I32) => true,
            (Type::F64, Type::F64) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Struct { id: id1, .. }, Type::Struct { id: id2, .. }) => id1 == id2,
            (Type::Enum { id: id1, .. }, Type::Enum { id: id2, .. }) => id1 == id2,
            (Type::Module { id: id1, .. }, Type::Module { id: id2, .. }) => id1 == id2,
            // the remaining types are equal if their inner types are equal, since (i32, i32) is
            // always equal to (i32, i32) even if it ends up as two distinct Type instances
            (Type::Tuple { fields: f1, .. }, Type::Tuple { fields: f2, .. }) => f1 == f2,
            (
                Type::Array {
                    ty: t1, length: l1, ..
                },
                Type::Array {
                    ty: t2, length: l2, ..
                },
            ) => t1 == t2 && l1 == l2,
            (Type::Slice { ty: t1, .. }, Type::Slice { ty: t2, .. }) => t1 == t2,
            (Type::Ref { ty: t1, .. }, Type::Ref { ty: t2, .. }) => t1 == t2,
            (Type::Optional { ty: t1, .. }, Type::Optional { ty: t2, .. }) => t1 == t2,
            (
                Type::Fn {
                    param_tys: p1,
                    ret_ty: r1,
                    ..
                },
                Type::Fn {
                    param_tys: p2,
                    ret_ty: r2,
                    ..
                },
            ) => p1 == p2 && r1 == r2,
            (Type::Range { id: id1, .. }, Type::Range { id: id2, .. }) => id1 == id2,
            _ => false,
        }
    }
}

impl Eq for Type {}

impl std::hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Type::Never => {
                NEVER_TYPE_ID.hash(state);
            },
            Type::Unit => {
                UNIT_TYPE_ID.hash(state);
            },
            Type::I32 => {
                I32_TYPE_ID.hash(state);
            },
            Type::F64 => {
                F64_TYPE_ID.hash(state);
            },
            Type::Bool => BOOL_TYPE_ID.hash(state),
            Type::Struct { id, .. } => {
                id.hash(state);
            },
            Type::Enum { id, .. } => {
                id.hash(state);
            },
            Type::Module { id, .. } => {
                id.hash(state);
            },
            Type::Tuple { fields, .. } => {
                "tuple".hash(state);
                fields.hash(state);
            },
            Type::Array { ty, length, .. } => {
                "array".hash(state);
                ty.hash(state);
                length.hash(state);
            },
            Type::Slice { ty, .. } => {
                // need to hash something else otherwise this will collide with the inner type
                "optional".hash(state);
                ty.hash(state);
            },
            Type::Ref { ty, .. } => {
                "ref".hash(state);
                ty.hash(state);
            },
            Type::Optional { ty, .. } => {
                "optional".hash(state);
                ty.hash(state);
            },
            Type::Fn {
                param_tys, ret_ty, ..
            } => {
                "fn".hash(state);
                param_tys.hash(state);
                ret_ty.hash(state);
            },
            Type::Range { id, .. } => {
                id.hash(state);
            },
        }
    }
}

pub const NEVER_TYPE_ID: u64 = 0;
pub const UNIT_TYPE_ID: u64 = 1;
pub const I32_TYPE_ID: u64 = 2;
pub const F64_TYPE_ID: u64 = 3;
pub const BOOL_TYPE_ID: u64 = 4;
pub const FIRST_NON_RESERVED_TYPE_ID: u64 = 5;

impl InitStore for Type {
    fn init_store(store: &mut Store<Type>) {
        store.items.insert(I32_TYPE_ID, Type::I32);
        store.items.insert(F64_TYPE_ID, Type::F64);
        store.items.insert(BOOL_TYPE_ID, Type::Bool);
        store.next_id.store(
            FIRST_NON_RESERVED_TYPE_ID,
            std::sync::atomic::Ordering::Relaxed,
        );
    }
}

pub static mut VALUE_STORE: Option<Store<Value>> = None;

pub enum Value {
    Const {
        id:         u64,
        visibility: Visibility,
        name:       token::Ident,
        ty:         MaybeType,
        expr:       Box<Expr>,
    },
    Var {
        id:              u64,
        name:            token::Ident,
        ty:              MaybeType,
        // if the value is fully moved, it cannot be used again
        moved:           bool,
        // if the value is partially moved (a struct field was moved), that field (or the type as a
        // whole) cannot be used again
        // for now, we only track one level of partial moves, we do not track nested partial moves
        partially_moved: Vec<token::Ident>,
    },
    Fn {
        id:         u64,
        visibility: Visibility,
        name:       token::Ident,
        ret_ty:     MaybeType,
        params:     Vec<(token::Ident, MaybeType, Option<&'static Value>)>,
        scope:      &'static Scope,
        body:       Box<hir::Block>,
        ty:         MaybeType,
    },
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Const { id, name, ty, .. } => {
                write!(f, "Const({}, {}, {:?})", id, name.value, ty)
            },
            Value::Var {
                id,
                name,
                ty,
                moved,
                partially_moved,
            } => {
                write!(
                    f,
                    "Var({}, {}, {:?}, moved: {}, partially_moved: {:?})",
                    id,
                    name.value,
                    ty,
                    moved,
                    partially_moved
                        .iter()
                        .map(|id| &id.value)
                        .collect::<Vec<_>>()
                )
            },
            Value::Fn {
                id,
                name,
                ret_ty,
                params,
                ..
            } => {
                write!(f, "Fn({}, {}, {:?}, params: [", id, name.value, ret_ty)?;
                for (i, (param_name, param_ty, _)) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {:?}", param_name.value, param_ty)?;
                }
                write!(f, "])")
            },
        }
    }
}

impl std::hash::Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Value::Const { id, .. } => id.hash(state),
            Value::Var { id, .. } => id.hash(state),
            Value::Fn { id, .. } => id.hash(state),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Const { id: id1, .. }, Value::Const { id: id2, .. }) => id1 == id2,
            (Value::Var { id: id1, .. }, Value::Var { id: id2, .. }) => id1 == id2,
            (Value::Fn { id: id1, .. }, Value::Fn { id: id2, .. }) => id1 == id2,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl Value {
    pub fn mark_move(&self) {
        let this = unsafe { &mut *(self as *const Value as *mut Value) };
        match this {
            Value::Const { .. } => {
                // consts cannot be moved
            },
            Value::Var { moved, .. } => {
                *moved = true;
            },
            Value::Fn { .. } => {
                // functions cannot be moved
            },
        }
    }

    pub fn mark_partial_move(&self, field: token::Ident) {
        let this = unsafe { &mut *(self as *const Value as *mut Value) };
        match this {
            Value::Const { .. } => {
                // consts cannot be partially moved
            },
            Value::Var {
                partially_moved, ..
            } => {
                partially_moved.push(field);
            },
            Value::Fn { .. } => {
                // functions cannot be partially moved
            },
        }
    }

    pub fn ty(&self) -> &MaybeType {
        match self {
            Value::Const { ty, .. } => ty,
            Value::Var { ty, .. } => ty,
            Value::Fn { ty, .. } => ty,
        }
    }

    pub fn ty_mut(&mut self) -> &mut MaybeType {
        match self {
            Value::Const { ty, .. } => ty,
            Value::Var { ty, .. } => ty,
            Value::Fn { ty, .. } => ty,
        }
    }

    pub fn name(&self) -> &token::Ident {
        match self {
            Value::Const { name, .. } => name,
            Value::Var { name, .. } => name,
            Value::Fn { name, .. } => name,
        }
    }

    pub fn get_by_id(id: u64) -> Option<&'static Value> {
        unsafe {
            VALUE_STORE
                .as_ref()
                .unwrap()
                .items
                .get(&id)
                .map(|x| &*(&*x as *const Value))
        }
    }
}

pub const PRINT_I32_VALUE_ID: u64 = 1;
pub const PRINT_WORDS_VALUE_ID: u64 = 2;
pub const FIRST_NON_RESERVED_VALUE_ID: u64 = 5;

impl InitStore for Value {
    fn init_store(store: &mut Store<Value>) {
        store.next_id.store(
            FIRST_NON_RESERVED_VALUE_ID,
            std::sync::atomic::Ordering::Relaxed,
        );
    }
}

pub fn create_scope(
    type_parent: &'static Scope,
    value_parent: &'static Scope,
    module_scope: Option<&'static Scope>,
) -> Result<&'static Scope> {
    unsafe {
        let store = SCOPE_STORE.as_ref().unwrap();
        let id = store
            .next_id
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let scope = Scope {
            id,
            type_parent: Some(type_parent),
            type_ns: None,
            unresolved_aliases: None,
            value_parent: Some(value_parent),
            value_ns: None,
            includes: Vec::new(),
            // if none is provided, then we're creating a new module scope
            module_scope,
            use_items: Vec::new(),
            return_ty: None,
            break_ty: None,
        };
        store.items.insert(id, scope);
        let x = &*(&*store.items.get(&id).unwrap() as *const Scope);
        Ok(x)
    }
}

impl Scope {
    pub fn inherit_all(&'static self) -> Result<&'static Scope> {
        create_scope(self, self, Some(self.module_scope.unwrap_or(self)))
    }

    pub fn inherit_types(&'static self) -> Result<&'static Scope> {
        create_scope(
            self,
            self.module_scope.unwrap_or(self),
            Some(self.module_scope.unwrap_or(self)),
        )
    }

    pub fn new_module_scope() -> Result<&'static Scope> {
        let root =
            unsafe { &*(&*SCOPE_STORE.as_ref().unwrap().items.get(&0).unwrap() as *const Scope) };
        create_scope(root, root, None)
    }

    pub fn add_use_item(&self, item: concrete::UseItem) {
        unsafe {
            let store = SCOPE_STORE.as_ref().unwrap();
            let mut scope = store.items.get_mut(&self.id).unwrap();
            scope.use_items.push(item);
        }
    }

    pub fn new_type(&self, name: Option<token::Ident>, mut ty: Type) -> Result<&'static Type> {
        unsafe {
            let store = SCOPE_STORE.as_ref().unwrap();
            let mut scope = store.items.get_mut(&self.id).unwrap();
            if scope.type_ns.is_none() {
                scope.type_ns = Some(DashMap::new());
            }
            let type_store = TYPE_STORE.as_ref().unwrap();
            let id = type_store
                .next_id
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            match &mut ty {
                Type::Struct { id: ty_id, .. } => *ty_id = id,
                Type::Enum { id: ty_id, .. } => *ty_id = id,
                Type::Module { id: ty_id, .. } => *ty_id = id,
                Type::Tuple { id: ty_id, .. } => *ty_id = id,
                Type::Array { id: ty_id, .. } => *ty_id = id,
                Type::Slice { id: ty_id, .. } => *ty_id = id,
                Type::Ref { id: ty_id, .. } => *ty_id = id,
                Type::Optional { id: ty_id, .. } => *ty_id = id,
                Type::Fn { id: ty_id, .. } => *ty_id = id,
                Type::I32 | Type::F64 | Type::Bool | Type::Never | Type::Unit => {
                    // primitive types already have ids
                },
                Type::Range { id: ty_id, .. } => *ty_id = id,
            }
            type_store.items.insert(id, ty);
            let ty_ref = Type::get_by_id(id).unwrap();
            if let Some(name) = name {
                if scope.type_ns.as_ref().unwrap().contains_key(name.value) {
                    return Err(CompileError::Error(
                        name.span,
                        format!("type `{}` is already defined in this scope", name.value),
                    ));
                }
                scope.type_ns.as_ref().unwrap().insert(name.value, ty_ref);
            }
            Ok(ty_ref)
        }
    }

    pub fn new_alias(&self, name: token::Ident, ty: concrete::ConcreteType) -> Result<()> {
        unsafe {
            let store = SCOPE_STORE.as_ref().unwrap();
            let mut scope = store.items.get_mut(&self.id).unwrap();
            if scope.unresolved_aliases.is_none() {
                scope.unresolved_aliases = Some(DashMap::new());
            }
            if scope
                .unresolved_aliases
                .as_ref()
                .unwrap()
                .contains_key(name.value)
            {
                return Err(CompileError::Error(
                    name.span,
                    format!(
                        "type alias `{}` is already defined in this scope",
                        name.value
                    ),
                ));
            }
            scope
                .unresolved_aliases
                .as_ref()
                .unwrap()
                .insert(name.value, ty);
            Ok(())
        }
    }

    pub fn new_value(&self, name: token::Ident, mut val: Value) -> Result<&'static Value> {
        unsafe {
            let store = SCOPE_STORE.as_ref().unwrap();
            let mut scope = store.items.get_mut(&self.id).unwrap();
            if scope.value_ns.is_none() {
                scope.value_ns = Some(DashMap::new());
            }
            let value_store = VALUE_STORE.as_ref().unwrap();
            let id = if val.id() < FIRST_NON_RESERVED_VALUE_ID && val.id() != 0 {
                val.id()
            } else {
                let id = value_store
                    .next_id
                    .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                match &mut val {
                    Value::Const { id: val_id, .. } => *val_id = id,
                    Value::Var { id: val_id, .. } => *val_id = id,
                    Value::Fn { id: val_id, .. } => *val_id = id,
                }
                id
            };
            value_store.items.insert(id, val);
            let val_ref = &*(&*value_store.items.get(&id).unwrap() as *const Value);
            if scope.value_ns.as_ref().unwrap().contains_key(name.value) {
                return Err(CompileError::Error(
                    name.span,
                    format!("value `{}` is already defined in this scope", name.value),
                ));
            }
            scope.value_ns.as_ref().unwrap().insert(name.value, val_ref);
            Ok(val_ref)
        }
    }

    pub fn get_by_id(id: u64) -> Option<&'static Scope> {
        unsafe {
            SCOPE_STORE
                .as_ref()
                .unwrap()
                .items
                .get(&id)
                .map(|x| &*(&*x as *const Scope))
        }
    }

    pub fn all() -> Vec<&'static Scope> {
        unsafe { SCOPE_STORE.as_ref().unwrap().all() }
    }

    pub fn is_type_parent_of(&self, other: &Scope) -> bool {
        let mut current = other;
        while let Some(parent) = current.type_parent {
            if parent == self {
                return true;
            }
            current = parent;
        }
        false
    }

    pub fn resolve_path_type(&self, path: &concrete::Path) -> Result<&'static Type> {
        let mut working_scope = self;
        if path.root {
            // 0 is always the global root (prelude, external packages, etc)
            working_scope = Scope::get_by_id(0).unwrap();
        }
        if path.pkg {
            // TODO: this will need to be unhardcoded when multiple packages are supported,
            // std just should not do this
            match working_scope
                .type_ns
                .as_ref()
                .unwrap()
                .get("pkg")
                .unwrap()
                .value()
            {
                Type::Module { scope, .. } => {
                    working_scope = scope;
                },
                _ => unreachable!("`pkg` is not a module"),
            }
        }
        if path.segments.len() == 1 {
            // single segment path, just resolve the name
        }
        for segment in path.segments[..path.segments.len() - 1].iter() {
            // for each segment except the last, we must find a module
            // the module must either be public or a parent of the current scope
            let ty = working_scope.resolve_name_type(segment)?;
            match ty {
                Type::Module {
                    visibility, scope, ..
                } => {
                    if *visibility == Visibility::Private && !working_scope.is_type_parent_of(self)
                    {
                        return Err(CompileError::Error(
                            segment.span,
                            format!("module `{}` is private", segment.value),
                        ));
                    }
                    working_scope = scope;
                },
                _ => {
                    return Err(CompileError::Error(
                        segment.span,
                        format!("`{}` is not a module", segment.value),
                    ));
                },
            }
        }
        // the last segment must be a type (not module)
        let last_segment = path.segments.last().unwrap();
        let ty = working_scope.resolve_name_type(last_segment)?;
        if let Type::Module { .. } = ty {
            return Err(CompileError::Error(
                last_segment.span,
                format!("`{}` is a module, not a type", last_segment.value),
            ));
        }
        Ok(ty)
    }

    pub fn is_value_parent_of(&self, other: &Scope) -> bool {
        let mut current = other;
        while let Some(parent) = current.value_parent {
            if parent == self {
                return true;
            }
            current = parent;
        }
        false
    }

    pub fn resolve_path_value(&self, path: &concrete::Path) -> Result<&'static Value> {
        let mut working_scope = self;
        if path.root {
            // 0 is always the global root (prelude, external packages, etc)
            working_scope = Scope::get_by_id(0).unwrap();
        }
        if path.pkg {
            // TODO: this will need to be unhardcoded when multiple packages are supported,
            // std just should not do this
            match working_scope
                .type_ns
                .as_ref()
                .unwrap()
                .get("pkg")
                .unwrap()
                .value()
            {
                Type::Module { scope, .. } => {
                    working_scope = scope;
                },
                _ => unreachable!("`pkg` is not a module"),
            }
        }
        let last_index = path.segments.len() - 1;
        let second_last_index = if last_index == 0 { 0 } else { last_index - 1 };
        for (i, segment) in path.segments.iter().enumerate() {
            // for each segment, we must find a module
            // the second to last segment can be an enum or a module
            // the last segment must be a value if the second to last was a module,
            // otherwise it must be a variant of the enum
            if i == last_index {
                // we now want to find a value in the current scope
                return working_scope.resolve_name_value(segment);
            } else {
                let ty = working_scope.resolve_name_type(segment)?;
                match ty {
                    Type::Module {
                        visibility, scope, ..
                    } => {
                        if *visibility == Visibility::Private
                            && !working_scope.is_type_parent_of(self)
                        {
                            return Err(CompileError::Error(
                                segment.span,
                                format!("module `{}` is private", segment.value),
                            ));
                        }
                        working_scope = scope;
                    },
                    Type::Enum { visibility, .. } => {
                        if *visibility == Visibility::Private
                            && !working_scope.is_type_parent_of(self)
                        {
                            return Err(CompileError::Error(
                                segment.span,
                                format!("enum `{}` is private", segment.value),
                            ));
                        }
                        if i != second_last_index {
                            return Err(CompileError::Error(
                                segment.span,
                                format!("`{}` is an enum, not a module", segment.value),
                            ));
                        }
                        todo!("enum variant resolution is not yet implemented")
                    },
                    _ => {
                        return Err(CompileError::Error(
                            segment.span,
                            format!("`{}` is not a module or enum", segment.value),
                        ));
                    },
                }
            }
        }
        unreachable!("unreachable code in resolve_path_value");
    }

    pub fn resolve_path_value_mut(&self, path: &concrete::Path) -> Result<&'static mut Value> {
        Ok(unsafe { &mut *(self.resolve_path_value(path)? as *const Value as *mut Value) })
    }

    pub fn resolve_name_type(&self, name: &token::Ident) -> Result<&'static Type> {
        // first, we check if this type is in the current scope
        let ty = self.type_ns.as_ref().and_then(|ns| ns.get(name.value));
        if let Some(ty) = ty {
            return Ok(unsafe { &*(&**ty.value() as *const Type) });
        }
        // TODO: check included scopes as well, this needs to respect privacy
        // then, we check the parent scope
        if let Some(parent) = self.type_parent {
            return parent.resolve_name_type(name);
        }
        Err(CompileError::Error(
            name.span,
            format!("type `{}` not found", name.value),
        ))
    }

    pub fn resolve_name_value(&self, name: &token::Ident) -> Result<&'static Value> {
        // first, we check if this value is in the current scope
        let val = self.value_ns.as_ref().and_then(|ns| ns.get(name.value));
        if let Some(val) = val {
            return Ok(unsafe { &*(&**val.value() as *const Value) });
        }
        // TODO: check included scopes as well, this needs to respect privacy
        // then, we check the parent scope
        if let Some(parent) = self.value_parent {
            return parent.resolve_name_value(name);
        }
        Err(CompileError::Error(
            name.span,
            format!("value `{}` not found", name.value),
        ))
    }

    pub fn resolve_name_value_mut(&self, name: &token::Ident) -> Result<&'static mut Value> {
        Ok(unsafe { &mut *(self.resolve_name_value(name)? as *const Value as *mut Value) })
    }

    pub fn return_ty(&self) -> Option<&'static Type> {
        if let Some(ret_ty) = self.return_ty {
            return Some(ret_ty);
        }
        if let Some(parent) = self.value_parent {
            return parent.return_ty();
        }
        None
    }

    #[allow(clippy::mut_from_ref)]
    pub fn break_ty(&self, label: Option<concrete::Label>) -> Option<&mut MaybeType> {
        if let Some((lbl, break_ty)) = &self.break_ty {
            if lbl.is_none() || lbl == &label {
                return Some(unsafe { &mut *(break_ty as *const MaybeType as *mut MaybeType) });
            }
        }
        if let Some(parent) = self.value_parent {
            return parent.break_ty(label);
        }
        None
    }

    pub fn label_exists(&self, label: &concrete::Label) -> bool {
        if let Some((Some(lbl), _)) = &self.break_ty {
            if lbl == label {
                return true;
            }
        }
        if let Some(parent) = self.value_parent {
            return parent.label_exists(label);
        }
        false
    }

    pub fn set_return_ty(&self, ty: &'static Type) {
        let this = unsafe { &mut *(self as *const Scope as *mut Scope) };
        this.return_ty = Some(ty);
    }

    pub fn set_break_ty(&self, label: Option<concrete::Label>, ty: MaybeType) {
        let this = unsafe { &mut *(self as *const Scope as *mut Scope) };
        this.break_ty = Some((label, ty));
    }

    pub fn all_vars(&self) -> Vec<(&'static Value, Vec<token::Ident>)> {
        let mut vars = Vec::new();
        if let Some(ns) = &self.value_ns {
            for entry in ns.iter() {
                if let Value::Var {
                    partially_moved, ..
                } = entry.value()
                {
                    vars.push((
                        unsafe { &*(&**entry.value() as *const Value) },
                        partially_moved.clone(),
                    ));
                }
            }
        }
        if let Some(parent) = self.value_parent {
            vars.extend(parent.all_vars());
        }
        vars
    }

    pub fn all_vars_until_break_ty(
        &self,
        label: Option<&concrete::Label>,
    ) -> Vec<(&'static Value, Vec<token::Ident>)> {
        let mut vars = Vec::new();
        if let Some(ns) = &self.value_ns {
            for entry in ns.iter() {
                if let Value::Var {
                    partially_moved, ..
                } = entry.value()
                {
                    vars.push((
                        unsafe { &*(&**entry.value() as *const Value) },
                        partially_moved.clone(),
                    ));
                }
            }
        }
        if let Some(parent) = self.value_parent {
            if let Some(label) = label {
                if let Some((Some(lbl), _)) = &parent.break_ty {
                    if lbl == label {
                        return vars;
                    }
                }
            } else if parent.break_ty.is_some() {
                return vars;
            } else {
                vars.extend(parent.all_vars_until_break_ty(label));
            }
        }
        vars
    }
}

impl Type {
    pub fn all() -> Vec<&'static Type> {
        unsafe { TYPE_STORE.as_ref().unwrap().all() }
    }

    pub fn all_mut() -> Vec<&'static mut Type> {
        unsafe { TYPE_STORE.as_ref().unwrap().all_mut() }
    }

    pub fn store(mut self) -> &'static Type {
        unsafe {
            match self {
                Type::I32 => return &Type::I32,
                Type::F64 => return &Type::F64,
                Type::Bool => return &Type::Bool,
                Type::Never => return &Type::Never,
                Type::Unit => return &Type::Unit,
                _ => {},
            }
            let store = TYPE_STORE.as_ref().unwrap();
            let id = store
                .next_id
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            match &mut self {
                Type::Struct { id: ty_id, .. } => *ty_id = id,
                Type::Enum { id: ty_id, .. } => *ty_id = id,
                Type::Module { id: ty_id, .. } => *ty_id = id,
                Type::Tuple { id: ty_id, .. } => *ty_id = id,
                Type::Array { id: ty_id, .. } => *ty_id = id,
                Type::Slice { id: ty_id, .. } => *ty_id = id,
                Type::Ref { id: ty_id, .. } => *ty_id = id,
                Type::Optional { id: ty_id, .. } => *ty_id = id,
                Type::Fn { id: ty_id, .. } => *ty_id = id,
                Type::I32 | Type::F64 | Type::Bool | Type::Never | Type::Unit => {
                    // primitive types already have ids
                },
                Type::Range { id: ty_id, .. } => *ty_id = id,
            }
            store.items.insert(id, self);
            &*(&*store.items.get(&id).unwrap() as *const Type)
        }
    }

    pub fn name(&self) -> String {
        match self {
            Type::Never => "Never".to_string(),
            Type::Unit => "Unit".to_string(),
            Type::I32 => "i32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Struct { name, .. } => name.value.to_string(),
            Type::Enum { name, .. } => name.value.to_string(),
            Type::Module { .. } => "mod".to_string(),
            Type::Tuple { fields, .. } => {
                let mut s = "(".to_string();
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&field.name());
                }
                s.push(')');
                s
            },
            Type::Array { ty, length, .. } => {
                format!("[{}; {}]", ty.name(), length)
            },
            Type::Slice { ty, .. } => {
                format!("[{}]", ty.name())
            },
            Type::Ref { ty, .. } => {
                format!("&{}", ty.name())
            },
            Type::Optional { ty, .. } => {
                format!("{}?", ty.name())
            },
            Type::Fn {
                param_tys, ret_ty, ..
            } => {
                let mut s = "fn(".to_string();
                for (i, param_ty) in param_tys.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&param_ty.name());
                }
                s.push_str(") -> ");
                s.push_str(&ret_ty.name());
                s
            },
            Type::Range {
                start,
                end,
                inclusive,
                ..
            } => {
                format!(
                    "Range<{}{}{}>",
                    start,
                    if *inclusive { "..=" } else { ".." },
                    end
                )
            },
        }
    }

    pub fn id(&self) -> u64 {
        match self {
            Type::Never => NEVER_TYPE_ID,
            Type::Unit => UNIT_TYPE_ID,
            Type::I32 => I32_TYPE_ID,
            Type::F64 => F64_TYPE_ID,
            Type::Bool => BOOL_TYPE_ID,
            Type::Struct { id, .. } => *id,
            Type::Enum { id, .. } => *id,
            Type::Module { id, .. } => *id,
            Type::Tuple { id, .. } => *id,
            Type::Array { id, .. } => *id,
            Type::Slice { id, .. } => *id,
            Type::Ref { id, .. } => *id,
            Type::Optional { id, .. } => *id,
            Type::Fn { id, .. } => *id,
            Type::Range { id, .. } => *id,
        }
    }

    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            Type::I32 | Type::F64 | Type::Bool | Type::Unit | Type::Never
        )
    }

    pub fn is_copy_type(&self) -> bool {
        match self {
            Type::Never
            | Type::Unit
            | Type::I32
            | Type::F64
            | Type::Bool
            | Type::Slice { .. }
            | Type::Ref { .. }
            | Type::Fn { .. }
            | Type::Range { .. } => true,
            Type::Optional { ty, .. } => ty.is_copy_type(),
            Type::Struct { .. }
            | Type::Enum { .. }
            | Type::Module { .. }
            | Type::Tuple { .. }
            | Type::Array { .. } => false,
        }
    }

    pub fn resolve_explicit(&mut self) -> Result<()> {
        if self.is_primitive() {
            return Ok(());
        }
        match self {
            Type::Never | Type::Unit | Type::I32 | Type::F64 | Type::Bool => {},
            Type::Struct { fields, .. } => {
                match fields {
                    hir::StructFields::Named { fields } => {
                        for field in fields {
                            field.ty.resolve_explicit()?;
                        }
                    },
                    hir::StructFields::Unnamed { fields } => {
                        for field in fields {
                            field.ty.resolve_explicit()?;
                        }
                    },
                    hir::StructFields::Unit => {},
                }
            },
            Type::Enum { variants, .. } => {
                for variant in variants {
                    match &mut variant.fields {
                        hir::StructFields::Named { fields } => {
                            for field in fields {
                                field.ty.resolve_explicit()?;
                            }
                        },
                        hir::StructFields::Unnamed { fields } => {
                            for field in fields {
                                field.ty.resolve_explicit()?;
                            }
                        },
                        hir::StructFields::Unit => {},
                    }
                    if variant.disc.is_some() {
                        todo!("enum discriminant type checking is not yet implemented");
                    }
                }
            },
            Type::Module { .. }
            | Type::Tuple { .. }
            | Type::Array { .. }
            | Type::Slice { .. }
            | Type::Ref { .. }
            | Type::Optional { .. }
            | Type::Fn { .. }
            | Type::Range { .. } => {},
        }
        Ok(())
    }

    pub fn get_by_id(id: u64) -> Option<&'static Type> {
        unsafe {
            TYPE_STORE
                .as_ref()
                .unwrap()
                .items
                .get(&id)
                .map(|x| &*(&*x as *const Type))
        }
    }

    pub fn unary_op(&'static self, op: &hir::UnaryOp, span: Span) -> Result<&'static Type> {
        match op {
            hir::UnaryOp::Neg => {
                match self {
                    Type::I32 | Type::F64 | Type::Range { .. } => Ok(self),
                    Type::Ref {
                        ty: Type::I32 | Type::F64,
                        ..
                    } => Ok(self),
                    _ => {
                        Err(CompileError::Error(
                            span,
                            format!("cannot negate type `{}`", self.name()),
                        ))
                    },
                }
            },
            hir::UnaryOp::Not => {
                match self {
                    Type::Bool | Type::I32 | Type::Range { .. } => Ok(self),
                    Type::Ref {
                        ty: Type::Bool | Type::I32,
                        ..
                    } => Ok(self),
                    _ => {
                        Err(CompileError::Error(
                            span,
                            format!("cannot apply `!` to type `{}`", self.name()),
                        ))
                    },
                }
            },
            hir::UnaryOp::Ref => {
                if matches!(self, Type::Ref { .. }) {
                    return Err(CompileError::Error(
                        span,
                        format!("cannot take reference of reference type `{}`", self.name()),
                    ));
                }
                if self.is_copy_type() {
                    return Err(CompileError::Error(
                        span,
                        format!("cannot take reference of copy type `{}`", self.name()),
                    ));
                }
                Ok(Type::Ref {
                    id:    0,
                    ty:    self,
                    // this scope isn't needed for anything else, so we can just use the global
                    // scope
                    scope: Scope::get_by_id(0).unwrap(),
                }
                .store())
            },
        }
    }

    pub fn unary_op_hint(&'static self, op: &hir::UnaryOp) -> Option<&'static Type> {
        match op {
            hir::UnaryOp::Neg => {
                if matches!(
                    self,
                    Type::I32 | Type::F64 | Type::Bool | Type::Range { .. }
                ) {
                    Some(self)
                } else {
                    None
                }
            },
            hir::UnaryOp::Not => {
                if matches!(self, Type::Bool | Type::I32 | Type::Range { .. }) {
                    Some(self)
                } else {
                    None
                }
            },
            hir::UnaryOp::Ref => None,
        }
    }

    pub fn binary_op(
        &'static self,
        other: &'static Type,
        op: &hir::BinaryOp,
        span: Span,
    ) -> Result<&'static Type> {
        // if both sides are references, then we dereference both sides
        let (this, other) = match (self, other) {
            (
                Type::Ref { ty: self_inner, .. },
                Type::Ref {
                    ty: other_inner, ..
                },
            ) => (*self_inner, *other_inner),
            _ => (self, other),
        };
        if this.is_never() || other.is_never() {
            return Ok(&Type::Never);
        }
        match op {
            hir::BinaryOp::Add
            | hir::BinaryOp::Sub
            | hir::BinaryOp::Mul
            | hir::BinaryOp::Div
            | hir::BinaryOp::Mod => {
                if this == other {
                    match this {
                        Type::I32 | Type::F64 | Type::Range { .. } => Ok(this),
                        _ => {
                            Err(CompileError::Error(
                                span,
                                format!("cannot apply `{}` to type `{}`", op, this.name()),
                            ))
                        },
                    }
                } else {
                    Err(CompileError::Error(
                        span,
                        format!(
                            "type mismatch for `{}`: `{}` and `{}`",
                            op,
                            this.name(),
                            other.name()
                        ),
                    ))
                }
            },
            hir::BinaryOp::And | hir::BinaryOp::Or => {
                if this == other {
                    match this {
                        Type::Bool => Ok(this),
                        _ => {
                            Err(CompileError::Error(
                                span,
                                format!("cannot apply `{}` to type `{}`", op, this.name()),
                            ))
                        },
                    }
                } else {
                    Err(CompileError::Error(
                        span,
                        format!(
                            "type mismatch for `{}`: `{}` and `{}`",
                            op,
                            this.name(),
                            other.name()
                        ),
                    ))
                }
            },
            hir::BinaryOp::Eq | hir::BinaryOp::Neq => {
                if this == other {
                    Ok(Type::Bool.store())
                } else {
                    Err(CompileError::Error(
                        span,
                        format!(
                            "type mismatch for `{}`: `{}` and `{}`",
                            op,
                            this.name(),
                            other.name()
                        ),
                    ))
                }
            },
            hir::BinaryOp::Lt | hir::BinaryOp::Leq | hir::BinaryOp::Gt | hir::BinaryOp::Geq => {
                if this == other {
                    match this {
                        Type::I32 | Type::F64 => Ok(Type::Bool.store()),
                        _ => {
                            Err(CompileError::Error(
                                span,
                                format!("cannot apply `{}` to type `{}`", op, this.name()),
                            ))
                        },
                    }
                } else {
                    Err(CompileError::Error(
                        span,
                        format!(
                            "type mismatch for `{}`: `{}` and `{}`",
                            op,
                            this.name(),
                            other.name()
                        ),
                    ))
                }
            },
            hir::BinaryOp::BitOr
            | hir::BinaryOp::BitAnd
            | hir::BinaryOp::BitXor
            | hir::BinaryOp::Shl
            | hir::BinaryOp::Shr => {
                if this == other {
                    match this {
                        Type::I32 => Ok(this),
                        _ => {
                            Err(CompileError::Error(
                                span,
                                format!("cannot apply `{}` to type `{}`", op, this.name()),
                            ))
                        },
                    }
                } else {
                    Err(CompileError::Error(
                        span,
                        format!(
                            "type mismatch for `{}`: `{}` and `{}`",
                            op,
                            this.name(),
                            other.name()
                        ),
                    ))
                }
            },
        }
    }

    pub fn binary_op_hint(
        &'static self,
        op: &hir::BinaryOp,
    ) -> Option<(&'static Type, &'static Type)> {
        if let Type::Ref { ty: inner_ty, .. } = self {
            match inner_ty.binary_op_hint(op) {
                Some((right, ret)) => {
                    return Some((
                        Type::Ref {
                            id:    0,
                            ty:    right,
                            scope: Scope::get_by_id(0).unwrap(),
                        }
                        .store(),
                        ret,
                    ))
                },
                None => return None,
            }
        }
        if self.is_never() {
            return Some((&Type::Never, &Type::Never));
        }
        match op {
            hir::BinaryOp::Add
            | hir::BinaryOp::Sub
            | hir::BinaryOp::Mul
            | hir::BinaryOp::Div
            | hir::BinaryOp::Mod => {
                if matches!(self, Type::I32 | Type::F64 | Type::Range { .. }) {
                    Some((self, self))
                } else {
                    None
                }
            },
            hir::BinaryOp::And | hir::BinaryOp::Or => {
                if matches!(self, Type::Bool) {
                    Some((self, self))
                } else {
                    None
                }
            },
            hir::BinaryOp::Eq | hir::BinaryOp::Neq => Some((self, Type::Bool.store())),
            hir::BinaryOp::Lt | hir::BinaryOp::Leq | hir::BinaryOp::Gt | hir::BinaryOp::Geq => {
                if matches!(self, Type::I32 | Type::F64) {
                    Some((self, Type::Bool.store()))
                } else {
                    None
                }
            },
            hir::BinaryOp::BitOr
            | hir::BinaryOp::BitAnd
            | hir::BinaryOp::BitXor
            | hir::BinaryOp::Shl
            | hir::BinaryOp::Shr => {
                if matches!(self, Type::I32) {
                    Some((self, self))
                } else {
                    None
                }
            },
        }
    }

    pub fn is_never(&self) -> bool {
        matches!(self, Type::Never)
    }

    pub fn is_ref_of(&self, ty: &Type) -> bool {
        match self {
            Type::Ref { ty: inner_ty, .. } => {
                // recurse through layers of references
                if *inner_ty == ty {
                    true
                } else {
                    inner_ty.is_ref_of(ty)
                }
            },
            _ => false,
        }
    }

    pub fn contains_refs(&self, visited: &mut HashSet<u64>) -> bool {
        if visited.contains(&self.id()) {
            return false;
        }
        visited.insert(self.id());
        match self {
            Type::Ref { .. } => true,
            Type::Array { ty, .. } | Type::Slice { ty, .. } | Type::Optional { ty, .. } => {
                ty.contains_refs(visited)
            },
            Type::Tuple { fields, .. } => {
                for field in fields {
                    if field.contains_refs(visited) {
                        return true;
                    }
                }
                false
            },
            Type::Fn {
                param_tys, ret_ty, ..
            } => {
                for param_ty in param_tys {
                    if param_ty.contains_refs(visited) {
                        return true;
                    }
                }
                ret_ty.contains_refs(visited)
            },
            Type::Never | Type::Unit | Type::I32 | Type::F64 | Type::Bool => false,
            Type::Struct { fields, .. } => {
                match fields {
                    hir::StructFields::Named { fields } => {
                        for field in fields {
                            if field.ty.ty().contains_refs(visited) {
                                return true;
                            }
                        }
                        false
                    },
                    hir::StructFields::Unnamed { fields } => {
                        for field in fields {
                            if field.ty.ty().contains_refs(visited) {
                                return true;
                            }
                        }
                        false
                    },
                    hir::StructFields::Unit => false,
                }
            },
            Type::Enum { variants, .. } => {
                for variant in variants {
                    match &variant.fields {
                        hir::StructFields::Named { fields } => {
                            for field in fields {
                                if field.ty.ty().contains_refs(visited) {
                                    return true;
                                }
                            }
                        },
                        hir::StructFields::Unnamed { fields } => {
                            for field in fields {
                                if field.ty.ty().contains_refs(visited) {
                                    return true;
                                }
                            }
                        },
                        hir::StructFields::Unit => {},
                    }
                }
                false
            },
            Type::Module { .. } => false,
            Type::Range { .. } => false,
        }
    }

    #[track_caller]
    pub fn reconcile_expected(&'static self, expected: &mut MaybeType, span: Span) -> Result<bool> {
        let mut found = MaybeType::Resolved(self);
        found.reconcile_expected(expected, span)
    }
}

impl MaybeType {
    pub fn ty(&self) -> &'static Type {
        if let MaybeType::Resolved(ty) = self {
            return ty;
        }
        panic!("attempted to get type of unresolved MaybeType");
    }

    pub fn ty_opt(&self) -> Option<&'static Type> {
        if let MaybeType::Resolved(ty) = self {
            return Some(ty);
        }
        None
    }

    pub fn resolve_explicit(&mut self) -> Result<()> {
        match self {
            MaybeType::Inferred => {
                Err(CompileError::Error(
                    Span::dummy(),
                    "cannot resolve inferred type".to_string(),
                ))
            },
            MaybeType::Unresolved(ty, scope) => {
                *self = MaybeType::Resolved(ty.resolve_type(scope)?);
                Ok(())
            },
            MaybeType::Resolved(_) => Ok(()),
        }
    }

    pub fn name(&self) -> String {
        match self {
            MaybeType::Inferred => "Inferred".to_string(),
            MaybeType::Unresolved(ty, _) => {
                match ty {
                    concrete::ConcreteType::Path(path) => {
                        if path.segments.len() == 1 {
                            path.segments[0].value
                        } else {
                            "Path"
                        }
                    },
                    concrete::ConcreteType::Tuple(_) => "Unresolved Tuple",
                    concrete::ConcreteType::Array { .. } => "Unresolved Array",
                    concrete::ConcreteType::Slice(_) => "Unresolved Slice",
                    concrete::ConcreteType::Ref(_) => "Unresolved Ref",
                    concrete::ConcreteType::Optional(_) => "Unresolved Optional",
                    concrete::ConcreteType::Range { .. } => "Unresolved Range",
                }
                .to_string()
            },
            MaybeType::Resolved(ty) => ty.name().to_string(),
        }
    }

    pub fn id(&self) -> Option<u64> {
        match self {
            MaybeType::Inferred => None,
            MaybeType::Unresolved(..) => None,
            MaybeType::Resolved(ty) => Some(ty.id()),
        }
    }

    #[track_caller]
    // reconcile this found type (self) with the expected type
    pub fn reconcile_expected(&mut self, expected: &mut MaybeType, span: Span) -> Result<bool> {
        let found = self;
        // if they are both inferred, then we do nothing
        match (expected, found) {
            (MaybeType::Inferred, MaybeType::Inferred) => Ok(false),
            (MaybeType::Resolved(expected), MaybeType::Resolved(found)) => {
                if expected.is_never() || found.is_never() {
                    // never can be any type, so we treat this as a successful reconciliation
                    return Ok(false);
                }
                // optional types can implicitly coerce to their inner type
                if let Type::Optional { ty, .. } = expected {
                    if ty == found {
                        return Ok(false);
                    }
                }
                // ranges can coerce to i32 if they fit within i32 bounds
                if let (
                    Type::I32,
                    Type::Range {
                        start,
                        end,
                        inclusive,
                        ..
                    },
                ) = (&expected, &found)
                {
                    // an i32 can accept any range that fits within i32 bounds, i32::MIN is
                    // reserved for nil though, so the start must be greater than that
                    if *start > i32::MIN as i128 {
                        if *inclusive {
                            if *end <= i32::MAX as i128 {
                                return Ok(false);
                            }
                        } else if *end < i32::MAX as i128 {
                            return Ok(false);
                        }
                    }
                }
                if let (Type::Range { universe: true, .. }, Type::Range { .. } | Type::I32) =
                    (&expected, &found)
                {
                    // a universe range can accept any range
                    return Ok(false);
                }
                // if let (
                //     Type::Range {
                //         start: e_start,
                //         end: e_end,
                //         inclusive: e_incl,
                //         ..
                //     },
                //     Type::Range {
                //         start: f_start,
                //         end: f_end,
                //         inclusive: f_incl,
                //         ..
                //     },
                // ) = (&expected, &found)
                // {
                //     // ranges can coerce if the expected range is equal to or larger than the
                // found range
                //     let start_ok = e_start <= f_start;
                //     let end_ok = e_end >= f_end;
                //     let incl_ok = if e_end == f_end {
                //         // if the ends are equal, then the expected range must be at least as
                //         // inclusive
                //         if *e_incl {
                //             *f_incl
                //         } else {
                //             true
                //         }
                //     } else {
                //         true
                //     };
                //     if start_ok && end_ok && incl_ok {
                //         return Ok(false);
                //     }
                // }
                if expected == found {
                    Ok(false)
                } else {
                    Err(CompileError::Error(
                        span,
                        format!("type mismatch: expected `{}`, found `{}`", expected, found),
                    ))
                }
            },
            (MaybeType::Unresolved(..), _) | (_, MaybeType::Unresolved(..)) => {
                unreachable!()
            },
            (MaybeType::Resolved(res), inf @ MaybeType::Inferred)
            | (inf @ MaybeType::Inferred, MaybeType::Resolved(res)) => {
                *inf = MaybeType::Resolved(res);
                Ok(true)
            },
        }
    }

    pub fn is_inferred(&self) -> bool {
        matches!(self, MaybeType::Inferred)
    }

    pub fn is_resolved(&self) -> bool {
        matches!(self, MaybeType::Resolved(_))
    }
}

impl concrete::ConcreteType {
    pub fn resolve_type(&self, scope: &'static Scope) -> Result<&'static Type> {
        match self {
            concrete::ConcreteType::Path(path) => scope.resolve_path_type(path),
            concrete::ConcreteType::Tuple(fields) => {
                let mut field_types = Vec::with_capacity(fields.len());
                for field in fields {
                    field_types.push(field.resolve_type(scope)?);
                }
                Ok(Type::Tuple {
                    id: 0,
                    fields: field_types,
                    scope,
                }
                .store())
            },
            concrete::ConcreteType::Array { ty, len } => {
                let elem_type = ty.resolve_type(scope)?;
                Ok(Type::Array {
                    id: 0,
                    ty: elem_type,
                    length: len.as_len()?,
                    scope,
                }
                .store())
            },
            concrete::ConcreteType::Slice(ty) => {
                let elem_type = ty.resolve_type(scope)?;
                Ok(Type::Slice {
                    id: 0,
                    ty: elem_type,
                    scope,
                }
                .store())
            },
            concrete::ConcreteType::Ref(ty) => {
                let ref_type = ty.resolve_type(scope)?;
                Ok(Type::Ref {
                    id: 0,
                    ty: ref_type,
                    scope,
                }
                .store())
            },
            concrete::ConcreteType::Optional(ty) => {
                let opt_type = ty.resolve_type(scope)?;
                if matches!(opt_type, Type::Optional { .. }) {
                    return Err(CompileError::Error(
                        self.span(),
                        "nested optional types are not allowed".to_string(),
                    ));
                }
                Ok(Type::Optional {
                    id: 0,
                    ty: opt_type,
                    scope,
                }
                .store())
            },
            concrete::ConcreteType::Range {
                start,
                end,
                inclusive,
                ..
            } => {
                Ok(Type::Range {
                    id:        0,
                    start:     *start,
                    end:       *end,
                    inclusive: *inclusive,
                    universe:  false,
                }
                .store())
            },
        }
    }

    pub fn span(&self) -> Span {
        match self {
            concrete::ConcreteType::Path(path) => path.span,
            concrete::ConcreteType::Tuple(elems) => {
                if elems.is_empty() {
                    Span::dummy()
                } else {
                    elems[0].span().join(elems[elems.len() - 1].span())
                }
            },
            concrete::ConcreteType::Array { ty, .. } => ty.span(),
            concrete::ConcreteType::Slice(ty) => ty.span(),
            concrete::ConcreteType::Ref(ty) => ty.span(),
            concrete::ConcreteType::Optional(ty) => ty.span(),
            concrete::ConcreteType::Range { span, .. } => *span,
        }
    }
}

impl Value {
    pub fn all() -> Vec<&'static Value> {
        unsafe { VALUE_STORE.as_ref().unwrap().all() }
    }

    pub fn all_mut() -> Vec<&'static mut Value> {
        unsafe { VALUE_STORE.as_ref().unwrap().all_mut() }
    }

    pub fn id(&self) -> u64 {
        match self {
            Value::Const { id, .. } => *id,
            Value::Var { id, .. } => *id,
            Value::Fn { id, .. } => *id,
        }
    }

    pub fn resolve_explicit(&mut self) -> Result<()> {
        match self {
            Value::Const { ty, .. } => {
                ty.resolve_explicit()?;
            },
            Value::Var { ty, .. } => {
                match ty {
                    MaybeType::Inferred => {
                        // these get inferred later
                    },
                    MaybeType::Resolved(_) => {
                        // already resolved
                    },
                    ty @ MaybeType::Unresolved(..) => {
                        ty.resolve_explicit()?;
                    },
                }
            },
            Value::Fn {
                ret_ty,
                params,
                ty,
                body,
                scope,
                ..
            } => {
                let ret_ty_span = match ret_ty {
                    MaybeType::Unresolved(ty, _) => ty.span(),
                    _ => Span::dummy(),
                };
                ret_ty.resolve_explicit()?;
                body.scope.set_return_ty(ret_ty.ty());
                for (name, ty, value) in params.iter_mut() {
                    ty.resolve_explicit()?;
                    *value = Some(scope.new_value(name.clone(), Value::Var {
                        id:              0,
                        name:            name.clone(),
                        ty:              ty.clone(),
                        moved:           false,
                        partially_moved: Vec::new(),
                    })?);
                }
                let ret_ty = match ret_ty {
                    MaybeType::Resolved(ty) => {
                        if ty.contains_refs(&mut HashSet::new()) {
                            return Err(CompileError::Error(
                                ret_ty_span,
                                "function return type cannot contain references".to_string(),
                            ));
                        }
                        ty
                    },
                    _ => unreachable!("function return type not resolved"),
                };
                *ty = MaybeType::Resolved(
                    Type::Fn {
                        id: 0,
                        visibility: Visibility::Private,
                        param_tys: params
                            .iter()
                            .map(|ty| {
                                match ty.1 {
                                    MaybeType::Resolved(ty) => ty,
                                    _ => unreachable!("function parameter type not resolved"),
                                }
                            })
                            .collect(),
                        ret_ty,
                        scope: body.scope.type_parent.unwrap(),
                    }
                    .store(),
                );
            },
        }
        Ok(())
    }

    pub fn infer(&mut self) -> Result<()> {
        match self {
            Value::Const { name, .. } => {
                return Err(CompileError::Unimplemented(name.span, "constants"));
            },
            Value::Var { .. } => {
                // variable types get inferred by their use
            },
            Value::Fn { body, ret_ty, .. } => {
                // the real meat of type inference happens in the function body and the actual
                // expressions
                loop {
                    if !body.infer(ret_ty.ty_opt())? {
                        // done inferring
                        break;
                    }
                }
            },
        }
        Ok(())
    }

    pub fn check_fully_resolved(&self) -> Result<()> {
        match self {
            Value::Const { ty, name, .. } => {
                if !ty.is_resolved() {
                    return Err(CompileError::Error(
                        name.span,
                        "constant type not fully resolved".to_string(),
                    ));
                }
            },
            Value::Var { ty, name, .. } => {
                if !ty.is_resolved() {
                    return Err(CompileError::Error(
                        name.span,
                        "variable type not fully resolved".to_string(),
                    ));
                }
            },
            Value::Fn {
                ret_ty,
                params,
                ty,
                body,
                name,
                ..
            } => {
                if !ret_ty.is_resolved() {
                    return Err(CompileError::Error(
                        name.span,
                        "function return type not fully resolved".to_string(),
                    ));
                }
                for (param, param_ty, _) in params {
                    if !param_ty.is_resolved() {
                        return Err(CompileError::Error(
                            param.span,
                            "function parameter type not fully resolved".to_string(),
                        ));
                    }
                }
                if !ty.is_resolved() {
                    return Err(CompileError::Error(
                        name.span,
                        "function type not fully resolved".to_string(),
                    ));
                }
                body.check_fully_resolved()?;
            },
        }
        Ok(())
    }

    pub fn check_ownership(&mut self) -> Result<()> {
        match self {
            Value::Const { name, .. } => {
                return Err(CompileError::Unimplemented(name.span, "constants"));
            },
            Value::Var { .. } => {
                // variables have no ownership rules
            },
            Value::Fn { body, .. } => {
                let moveset = body.check_ownership(hir::Moveset::new())?;
                moveset.apply();
            },
        }
        Ok(())
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // show the type name
        write!(f, "{}", match self {
            Type::Never => "!".to_string(),
            Type::Unit => "()".to_string(),
            Type::I32 => "i32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Struct { name, .. } => name.value.to_string(),
            Type::Enum { name, .. } => name.value.to_string(),
            Type::Module { name, .. } => format!("mod {}", name.value),
            Type::Tuple { fields, .. } => {
                let mut s = "(".to_string();
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&field.to_string());
                }
                s.push(')');
                s
            },
            Type::Array { ty, length, .. } => {
                format!("[{}; {}]", ty, length)
            },
            Type::Slice { ty, .. } => {
                format!("[{}]", ty)
            },
            Type::Ref { ty, .. } => {
                format!("&{}", ty)
            },
            Type::Optional { ty, .. } => {
                format!("{}?", ty)
            },
            Type::Fn {
                param_tys, ret_ty, ..
            } => {
                let mut s = "fn(".to_string();
                for (i, param_ty) in param_tys.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&param_ty.to_string());
                }
                s.push_str(") -> ");
                s.push_str(&ret_ty.to_string());
                s
            },
            Type::Range {
                start,
                end,
                inclusive,
                ..
            } => {
                if *inclusive {
                    format!("{}..={}", start, end)
                } else {
                    format!("{}..{}", start, end)
                }
            },
        })
    }
}
