use token::Ident;

use crate::parser::*;

#[track_caller]
fn roundtrip<T: Parse + std::fmt::Debug>(input: &'static str) -> T {
    let tokens = crate::tokenizer::tokenize_str(input, 0).expect(input);
    let mut cursor = Cursor::new(&tokens);
    let expr = T::parse(&mut cursor).expect(input);
    assert_eq!(expr.unparse(), input);
    expr
}

#[test]
fn test_source() {
    roundtrip::<Source>("fn my_function() { return 42; }");
    roundtrip::<Source>("struct MyStruct { a: i32, b: f64 }");
    roundtrip::<Source>("enum MyEnum { Variant1, Variant2(i32), Variant3 { a: i32, b: f64 } }");
    roundtrip::<Source>("const MY_CONST: i32 = 42;");
    roundtrip::<Source>("mod my_module {\nfn my_function() { return 42; }\n}");
    roundtrip::<Source>("use my_module::MyStruct;\nfn my_function() { return 42; }");
    roundtrip::<Source>("pub struct MyStruct { a: i32, b: f64 }\nfn my_function() { return 42; }");
    roundtrip::<Source>(
        "pub enum MyEnum { Variant1, Variant2(i32), Variant3 { a: i32, b: f64 } }\nfn \
         my_function() { return 42; }",
    );
    roundtrip::<Source>("pub const MY_CONST: i32 = 42;\nfn my_function() { return 42; }");
    roundtrip::<Source>(
        "pub mod my_module {\nfn my_function() { return 42; }\n}\nfn main() { \
         my_module::my_function(); }",
    );
}

#[test]
fn test_use_item() {
    roundtrip::<UseItem>("use my_module::MyStruct;");
    roundtrip::<UseItem>("use my_module::MyEnum::{Variant1, Variant2, test::*};");
    roundtrip::<UseItem>("use my_module::MyEnum::{Variant1, Variant2 as V, test::*};");
    roundtrip::<UseItem>("use my_module::MyEnum::*;");
    roundtrip::<UseItem>("use ::my_module::MyStruct;");
    roundtrip::<UseItem>("pub use my_module::MyStruct;");
}

#[test]
fn test_fn_item() {
    roundtrip::<FnItem>("fn my_function() { return 42; }");
    roundtrip::<FnItem>("pub fn my_function() { return 42; }");
    roundtrip::<FnItem>("fn my_function(a: i32, b: f64) -> i32 { return a + b as i32; }");
    roundtrip::<FnItem>("pub fn my_function(a: i32, b: f64) -> i32 { return a + b as i32; }");
    roundtrip::<FnItem>(
        "fn my_function(a: i32, b: f64) -> (i32, f64) { return (a + b as i32, b); }",
    );
    roundtrip::<FnItem>(
        "pub fn my_function(a: i32, b: f64) -> (i32, f64) { return (a + b as i32, b); }",
    );
}

#[test]
fn test_struct_item() {
    roundtrip::<StructItem>("struct MyStruct { a: i32, b: f64 }");
    roundtrip::<StructItem>("pub struct MyStruct { a: i32, b: f64 }");
    roundtrip::<StructItem>("struct MyStruct(i32, f64);");
    roundtrip::<StructItem>("pub struct MyStruct(i32, f64);");
    roundtrip::<StructItem>("struct MyStruct;");
    roundtrip::<StructItem>("pub struct MyStruct;");
}

#[test]
fn test_enum_item() {
    roundtrip::<EnumItem>("enum MyEnum { Variant1, Variant2(i32), Variant3 { a: i32, b: f64 } }");
    roundtrip::<EnumItem>(
        "pub enum MyEnum { Variant1, Variant2(i32), Variant3 { a: i32, b: f64 } }",
    );
}

#[test]
fn test_enum_variant() {
    roundtrip::<EnumVariant>("Variant");
    roundtrip::<EnumVariant>("Variant(i32, f64)");
    roundtrip::<EnumVariant>("Variant { a: i32, b: f64 }");
}

#[test]
fn test_const_item() {
    roundtrip::<ConstItem>("const MY_CONST: i32 = 42;");
    roundtrip::<ConstItem>("pub const MY_CONST: i32 = 42;");
}

#[test]
fn test_mod_item() {
    roundtrip::<ModItem>("mod my_module {\nfn my_function() { return 42; }\n}");
    roundtrip::<ModItem>("mod my_module;");
    roundtrip::<ModItem>("pub mod my_module;");
}

#[test]
fn test_block() {
    roundtrip::<Block>("{ let x = 42; x = x + 1; return x; }");
    roundtrip::<Block>("{ let (x, y) = (1, 2); x = x + y; return x; }");
    roundtrip::<Block>("{ if x { y = z; } else { a = b; } }");
    roundtrip::<Block>("{ match x { y => z, _ => w } }");
    roundtrip::<Block>("{ loop { x = x + 1; } }");
    roundtrip::<Block>("{ for i in 0..10 { x = x + i; } }");
    roundtrip::<Block>("{ while x < 20 { x = x + 1; } }");
    roundtrip::<Block>("{ func(a, b, c).method().field + 42; }");
    roundtrip::<Block>("{}");
    roundtrip::<Block>("{ x = x + 1; y + 1 }");
}

#[test]
fn test_let_stmt() {
    roundtrip::<LetStmt>("let x = 42;");
    roundtrip::<LetStmt>("let x: i32 = 42;");
    roundtrip::<LetStmt>("let x = func(a, b, c);");
    roundtrip::<LetStmt>("let (x, y) = (1, 2);");
    roundtrip::<LetStmt>("let (x, y, z) = (1, 2, 3);");
}

#[test]
fn test_path() {
    roundtrip::<Path>("x");
    roundtrip::<Path>("x::y");
    roundtrip::<Path>("x::y::z");
    roundtrip::<Path>("::x::y::z");
}

#[test]
fn test_pattern() {
    roundtrip::<Pattern>("x");
    roundtrip::<Pattern>("_");
    roundtrip::<Pattern>("(x, y)");
    roundtrip::<Pattern>("(x, y, z)");
    roundtrip::<Pattern>("(x, y, _)");
}

#[test]
fn test_type() {
    roundtrip::<ConcreteType>("i32");
    roundtrip::<ConcreteType>("f64");
    roundtrip::<ConcreteType>("bool");
    roundtrip::<ConcreteType>("String");
    roundtrip::<ConcreteType>("MyStruct");
    roundtrip::<ConcreteType>("MyEnum");
    roundtrip::<ConcreteType>("&i32");
    roundtrip::<ConcreteType>("[i32; 10]");
    roundtrip::<ConcreteType>("[f64]");
    roundtrip::<ConcreteType>("(i32, f64)");
    roundtrip::<ConcreteType>("(String, bool, MyStruct)");
}

#[test]
fn test_expr() {
    roundtrip::<Expr>("x = y");
    roundtrip::<Expr>("x + y * z");
    roundtrip::<Expr>("if x { y = z; } else { a = b; }");
    roundtrip::<Expr>("match x { y => z, _ => w }");
    roundtrip::<Expr>("loop { x = x + 1; }");
    roundtrip::<Expr>("for i in 0..10 { x = x + i; }");
    roundtrip::<Expr>("while x < 20 { x = x + 1; }");
    roundtrip::<Expr>("func(a, b, c).method().field + 42");
}

#[test]
fn test_assign_expr() {
    roundtrip::<AssignExpr>("x = y");
    roundtrip::<AssignExpr>("x = y + z");
    roundtrip::<AssignExpr>("x = (y + z) * 2");
    roundtrip::<AssignExpr>("x += y");
    roundtrip::<AssignExpr>("x -= y");
    roundtrip::<AssignExpr>("x *= y");
    roundtrip::<AssignExpr>("x /= y");
    roundtrip::<AssignExpr>("x %= y");
    roundtrip::<AssignExpr>("x &= y");
    roundtrip::<AssignExpr>("x |= y");
    roundtrip::<AssignExpr>("x ^= y");
    roundtrip::<AssignExpr>("x <<= y");
    roundtrip::<AssignExpr>("x >>= y");
}

#[test]
fn test_range_expr() {
    roundtrip::<RangeExpr>("x..y");
    roundtrip::<RangeExpr>("x..=y");
    roundtrip::<RangeExpr>("..y");
    roundtrip::<RangeExpr>("x..");
    roundtrip::<RangeExpr>("..=y");
    roundtrip::<RangeExpr>("..");
    roundtrip::<RangeExpr>("(x + 1)..(y - 1)");
}

#[test]
fn test_logical_or_expr() {
    roundtrip::<LogicalOrExpr>("x || y");
    roundtrip::<LogicalOrExpr>("x || y || z || w");
    roundtrip::<LogicalOrExpr>("(x && true) || (y || false) || (z && x)");
}

#[test]
fn test_logical_and_expr() {
    roundtrip::<LogicalAndExpr>("x && y");
    roundtrip::<LogicalAndExpr>("x && y && z && w");
    roundtrip::<LogicalAndExpr>("(x || true) && (y && false) && (z || x)");
}

#[test]
fn test_comparison_expr() {
    roundtrip::<ComparisonExpr>("x == y");
    roundtrip::<ComparisonExpr>("x != y");
    roundtrip::<ComparisonExpr>("x < y");
    roundtrip::<ComparisonExpr>("x <= y");
    roundtrip::<ComparisonExpr>("x > y");
    roundtrip::<ComparisonExpr>("x >= y");
    roundtrip::<ComparisonExpr>("((x == y) != z) < w");
}

#[test]
fn test_bitwise_or_expr() {
    roundtrip::<BitwiseOrExpr>("x | y");
    roundtrip::<BitwiseOrExpr>("x | y | z | w");
    roundtrip::<BitwiseOrExpr>("(x & 1) | (y ^ 2) | (z << 3)");
}

#[test]
fn test_bitwise_xor_expr() {
    roundtrip::<BitwiseXorExpr>("x ^ y");
    roundtrip::<BitwiseXorExpr>("x ^ y ^ z ^ w");
    roundtrip::<BitwiseXorExpr>("(x & 1) ^ (y | 2) ^ (z >> 3)");
}

#[test]
fn test_bitwise_and_expr() {
    roundtrip::<BitwiseAndExpr>("x & y");
    roundtrip::<BitwiseAndExpr>("x & y & z & w");
    roundtrip::<BitwiseAndExpr>("(x | 1) & (y ^ 2) & (z << 3)");
}

#[test]
fn test_shift_expr() {
    roundtrip::<ShiftExpr>("x << y");
    roundtrip::<ShiftExpr>("x >> y");
    roundtrip::<ShiftExpr>("x << y >> z << w");
    roundtrip::<ShiftExpr>("(x + 1) << (y - 2) >> (z * 3)");
}

#[test]
fn test_add_sub_expr() {
    roundtrip::<AddSubExpr>("x + y");
    roundtrip::<AddSubExpr>("x - y");
    roundtrip::<AddSubExpr>("x + y - z + w");
    roundtrip::<AddSubExpr>("(x * 2) + (y / 3) - (z % 4)");
}

#[test]
fn test_mul_div_mod_expr() {
    roundtrip::<MulDivModExpr>("x * y");
    roundtrip::<MulDivModExpr>("x / y");
    roundtrip::<MulDivModExpr>("x % y");
    roundtrip::<MulDivModExpr>("x * y / z % w");
    roundtrip::<MulDivModExpr>("(x + 1) * (y - 2) / (z % 3)");
}

#[test]
fn test_cast_expr() {
    roundtrip::<CastExpr>("x as Type");
    roundtrip::<CastExpr>("(x + 1) as Type");
    roundtrip::<CastExpr>("x as Type as Type2");
}

#[test]
fn test_unary_expr() {
    roundtrip::<UnaryExpr>("-x");
    roundtrip::<UnaryExpr>("--x");
    roundtrip::<UnaryExpr>("!x");
    roundtrip::<UnaryExpr>("!!x");
    roundtrip::<UnaryExpr>("&x");
    roundtrip::<UnaryExpr>("&&x");
}

#[test]
fn test_field_access_expr() {
    roundtrip::<FieldAccessExpr>("func()");
    roundtrip::<FieldAccessExpr>("func(a)");
    roundtrip::<FieldAccessExpr>("func(a, b, c)");
    roundtrip::<FieldAccessExpr>("func(1 + 2, x * y, z.method())");
    roundtrip::<FieldAccessExpr>("func(1 + 2, x * y, z.method()).field");
    roundtrip::<FieldAccessExpr>("func(1 + 2, x * y, z.method()).field.subfield");
    roundtrip::<FieldAccessExpr>("func(1 + 2, x * y, z.method()).field.subfield.method()");
    roundtrip::<FieldAccessExpr>("x[0]");
    roundtrip::<FieldAccessExpr>("x[0][1]");
    roundtrip::<FieldAccessExpr>("x[0 + 1]");
    roundtrip::<FieldAccessExpr>("x[func()]");
    roundtrip::<FieldAccessExpr>("x[func(a, b, c)]");
    roundtrip::<FieldAccessExpr>("x[func(a, b, c)].field");
    roundtrip::<FieldAccessExpr>("x[func(a, b, c)].field.subfield");
    roundtrip::<FieldAccessExpr>("x[func(a, b, c)].field.subfield.method()");
    roundtrip::<FieldAccessExpr>("x.field");
    roundtrip::<FieldAccessExpr>("x.field.subfield");
    roundtrip::<FieldAccessExpr>("x.method()");
    roundtrip::<FieldAccessExpr>("x.method(a, b, c)");
    roundtrip::<FieldAccessExpr>("x.method().other_method()");
    roundtrip::<FieldAccessExpr>("x.method(a, b).other_method(c, d)");
    roundtrip::<FieldAccessExpr>("x.field.subfield.method()");
    roundtrip::<FieldAccessExpr>("x.field.subfield.method(a, b)");
}

#[test]
fn test_primary_expr() {
    roundtrip::<PrimaryExpr>("42");
    roundtrip::<PrimaryExpr>("1.0f64");
    roundtrip::<PrimaryExpr>("true");
    roundtrip::<PrimaryExpr>("false");
    roundtrip::<PrimaryExpr>("\"hello\"");
    roundtrip::<PrimaryExpr>("x");
    roundtrip::<PrimaryExpr>("x::y::z");
    roundtrip::<PrimaryExpr>("::x::y::z");
    roundtrip::<PrimaryExpr>("(x)");
    roundtrip::<PrimaryExpr>("()");
    roundtrip::<PrimaryExpr>("(x,)");
    roundtrip::<PrimaryExpr>("(x, y)");
    roundtrip::<PrimaryExpr>("[x]");
    roundtrip::<PrimaryExpr>("[x, y, z]");
    roundtrip::<PrimaryExpr>("new Type { a, b: y }");
    roundtrip::<PrimaryExpr>("new Type { a, b: y, ..z }");
    roundtrip::<PrimaryExpr>("new Type {}");
    roundtrip::<PrimaryExpr>("new Type(a, b, c)");
    roundtrip::<PrimaryExpr>("new Type()");
    roundtrip::<PrimaryExpr>("Type");
    roundtrip::<PrimaryExpr>("{ x = x + 1; y = y + 1; }");
    roundtrip::<PrimaryExpr>("{ x = x + 1; y + 1 }");
    roundtrip::<PrimaryExpr>("if x { y = z; }");
}

#[test]
fn test_struct_expr() {
    roundtrip::<StructExpr>("new Type { a, b: y }");
    roundtrip::<StructExpr>("new Type { a, b: y, ..z }");
    roundtrip::<StructExpr>("new Type {}");
    roundtrip::<StructExpr>("new Type(a, b, c)");
    roundtrip::<StructExpr>("new Type()");
    roundtrip::<StructExpr>("new Type");
}

#[test]
fn test_struct_field_expr() {
    roundtrip::<StructFieldExpr>("a");
    roundtrip::<StructFieldExpr>("b: y");
}

#[test]
fn test_if_expr() {
    roundtrip::<IfExpr>("if x { y = z; }");
    roundtrip::<IfExpr>("if (x + 1) * 2 > 10 { y = z; }");
    roundtrip::<IfExpr>("if x { y = z; } else { a = b; }");
    roundtrip::<IfExpr>("if x { y = z; } else if a { b = c; } else { d = e; }");
    roundtrip::<IfExpr>("if x { y = z; } else if a { b = c; }");
}

#[test]
fn test_else_expr() {
    roundtrip::<ElseExpr>("if x { y = z; } else { a = b; }");
    roundtrip::<ElseExpr>("if x { y = z; } else if a { b = c; } else { d = e; }");
    roundtrip::<ElseExpr>("if x { y = z; } else if a { b = c; }");
}

#[test]
fn test_match_expr() {
    roundtrip::<MatchExpr>("match x { y => z }");
    roundtrip::<MatchExpr>("match (x, y) { (a, b) => c, _ => d }");
    roundtrip::<MatchExpr>("match x { _ => { a = b + c; d = e + f; } }");
}

#[test]
fn test_match_arm() {
    roundtrip::<MatchArm>("x => y");
    roundtrip::<MatchArm>("(x, y) => z");
    roundtrip::<MatchArm>("(x, y, _) => z");
    roundtrip::<MatchArm>("_ => z");
    roundtrip::<MatchArm>("(x, y, z) => { a = b + c; d = e + f; }");
    roundtrip::<MatchArm>("x if x > 0 => y");
}

#[test]
fn test_loop_expr() {
    roundtrip::<LoopExpr>("loop { x = x + 1; }");
    roundtrip::<LoopExpr>("$label: loop { x = x + 1; }");
}

#[test]
fn test_for_expr() {
    roundtrip::<ForExpr>("for i in 0..10 { x = x + i; }");
    roundtrip::<ForExpr>("$label: for i in 0..10 { x = x + i; }");
}

#[test]
fn test_while_expr() {
    roundtrip::<WhileExpr>("while x < 20 { x = x + 1; }");
    roundtrip::<WhileExpr>("$label: while x < 20 { x = x + 1; }");
}

#[test]
fn test_label() {
    let tokens = crate::tokenizer::tokenize_str("$label", 0).unwrap();
    let mut cursor = Cursor::new(&tokens);
    let label = Label::parse(&mut cursor).unwrap();
    assert_eq!(label.name, Ident {
        value: "label",
        span:  Span {
            start:  1,
            end:    6,
            line:   1,
            col:    2,
            indent: 0,
            file:   0,
        },
    });
}
