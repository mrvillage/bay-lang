use hir::*;

use crate::{concrete::Visibility, prelude::*};

pub fn type_check(source: concrete::Source) -> Result<()> {
    // our type checking has the following main steps:
    // 1. build HIR from concrete syntax tree, attach empty types and define scopes
    // 2. resolve all item definitions (functions, structs, enums, consts, etc.)
    to_hir(source)?;
    // 3. resolve all use statements
    resolve_use_items()?;
    // 4. resolve all paths and explicit types
    resolve_paths_and_types()?;
    // 5. infer remaining types, if any empty error out, this will also perform
    //    operator type checking and stuff (otherwise it wouldn't be able to resolve
    //    the types)
    infer_types()?;
    println!("{:#?}", Type::all());
    println!("{:#?}", Value::all());
    // 6. finally, we check ownership rules
    check_ownership()?;
    Ok(())
}

// everything is stored in the type and value store, this becomes the HIR/MIR
// for items functions are the only items that can contain statements and
// expressions, but functions are values so they go in the value store with
// their associated type when we actually do type checking, we need to resolve
// all types first, then we can go through all the values we have and attempt to
// resolve them at code generation time, we simply find the main function and
// recursively generate the required code for it and all its dependencies
// this lets us very easily do dead code elimination, as we only generate code
// for what is reachable from the main function
fn to_hir(source: concrete::Source) -> Result<&'static Scope> {
    // this is our package root
    let global_root = Scope::get_by_id(0).unwrap();
    let pkg_root = Scope::new_module_scope()?;
    global_root.new_type(
        Some(token::Ident {
            value: "pkg",
            span:  Span::dummy(),
        }),
        Type::Module {
            id:         0,
            visibility: Visibility::Public,
            name:       token::Ident {
                value: "pkg",
                span:  Span::dummy(),
            },
            scope:      pkg_root,
        },
    )?;
    // our package
    assert!(pkg_root.id == 1);
    for item in source.items {
        item_to_scope(item, pkg_root)?;
    }
    Ok(pkg_root)
}

fn item_to_scope(item: concrete::Item, scope: &'static Scope) -> Result<()> {
    match item {
        concrete::Item::Use(item) => scope.add_use_item(item),
        // values
        concrete::Item::Fn(item) => {
            scope.new_value(item.name.clone(), Value::Fn {
                id:         0,
                visibility: item.visibility.unwrap_or(Visibility::Private),
                name:       item.name,
                ret_ty:     match item.ret_ty {
                    Some(ty) => MaybeType::Unresolved(ty, scope),
                    None => MaybeType::Resolved(&Type::Unit),
                },
                params:     item
                    .params
                    .into_iter()
                    .map(|param| (param.name, MaybeType::Unresolved(param.ty, scope)))
                    .collect(),
                body:       Box::new(item.body.into_hir(scope)?),
                ty:         MaybeType::Inferred,
            })?;
        },
        concrete::Item::Const(item) => {
            scope.new_value(item.name.clone(), Value::Const {
                id:         0,
                visibility: item.visibility.unwrap_or(Visibility::Private),
                name:       item.name,
                ty:         MaybeType::Unresolved(item.ty, scope),
                expr:       Box::new(item.expr.into_hir(scope)?),
            })?;
        },
        // types
        concrete::Item::Struct(item) => {
            scope.new_type(Some(item.name.clone()), Type::Struct {
                id: 0,
                visibility: item.visibility.unwrap_or(Visibility::Private),
                name: item.name,
                fields: item.fields.into_hir(scope)?,
                scope,
            })?;
        },
        concrete::Item::Enum(item) => {
            scope.new_type(Some(item.name.clone()), Type::Enum {
                id: 0,
                visibility: item.visibility.unwrap_or(Visibility::Private),
                name: item.name,
                variants: {
                    let mut variants = Vec::with_capacity(item.variants.len());
                    for variant in item.variants {
                        variants.push(EnumVariant {
                            name:   variant.name,
                            fields: variant.fields.into_hir(scope)?,
                            disc:   variant.disc.map(|disc| {
                                Expr::Literal(Literal::Int {
                                    span:  disc.span,
                                    value: disc,
                                    ty:    MaybeType::Inferred,
                                })
                            }),
                            span:   variant.span,
                        });
                    }
                    variants
                },
                ty: MaybeType::Inferred,
                scope,
            })?;
        },
        concrete::Item::Mod(item) => {
            let module_scope = Scope::new_module_scope()?;
            scope.new_type(Some(item.name.clone()), Type::Module {
                id:         0,
                visibility: item.visibility.unwrap_or(Visibility::Private),
                name:       item.name,
                scope:      module_scope,
            })?;
            for item in item.items {
                item_to_scope(item, module_scope)?;
            }
        },
    }
    Ok(())
}

fn resolve_use_items() -> Result<()> {
    for scope in Scope::all() {
        // will need to do this recursively for included scopes as well, likely need an
        // intermediate stage for "this use item has been resolved to point to this
        // module, but we're not sure if the item it points to exists yet"
        for _ in &scope.use_items {
            todo!("use items are not implemented yet");
        }
    }
    Ok(())
}

fn resolve_paths_and_types() -> Result<()> {
    for ty in Type::all_mut() {
        // first, we resolve all paths in types
        ty.resolve_explicit()?;
    }
    for val in Value::all_mut() {
        // then we resolve all paths in values
        val.resolve_explicit()?;
    }
    Ok(())
}

// what can i infer?
// - function return types can be used to infer the return type if not given
// - function parameter types can be used to infer the type of arguments to
//   functions, and also to
// infer the type of parameters when used
// - let bindings with explicit types can be used to infer the type of the
//   expression assigned to
// them, and also
// - literals can be used to infer types
fn infer_types() -> Result<()> {
    for val in Value::all_mut() {
        val.infer()?;
    }
    println!("After inference:");
    println!("{:#?}", Type::all());
    println!("{:#?}", Value::all());
    // now we need to iterate through and raise errors for any remaining unresolved
    // types
    for val in Value::all() {
        val.check_fully_resolved()?;
    }
    Ok(())
}

/// a value can only be moved if it is returned as the result of a block
/// expression, assigned to another variable, or passed as an argument to a
/// function call if it's used to access a field, perform an operation, or
/// anything else, it is borrowed and not moved
/// the problem is
fn check_ownership() -> Result<()> {
    for val in Value::all_mut() {
        val.check_ownership()?;
    }
    Ok(())
}
