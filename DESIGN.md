# Design


## Step 1: Tokenizing
First, all source code is passed through a tokenizer that converts the raw text into a stream of tokens. Each token represents an atomic element of the language, such as identifiers, literals, symbols, and keywords. Whitespace and comments are ignored during this process. Each token includes a `Span`, which file it came from and its position in the source code to facilitate error reporting.

- Identifiers (variable name, function name, etc.) are composed of a single ASCII letter or underscore followed by zero or more ASCII letters, digits, or underscores.
- Literals:
  - Integer literals are sequences of ASCII digits and underscores with an optional prefix (e.g., `0b` for binary, `0x` for hexadecimal) to indicate the base, and an optional type suffix (e.g., `i32`, `u64`) to indicate the type.
  - Float literals are sequences of ASCII digits and underscores with a decimal point, an optional exponent part (e.g., `e10`), and an optional type suffix (e.g., `f32`, `f64`).
  - Boolean literals are represented by the identifiers `true` and `false`.
  - Nil literals are represented by the identifier `nil`.
- Symbols include all operators and special symbols such as `+`, `-`, `*`, `/`, `=`, `;`, `{`, `}`, `(`, `)`, and others.
- Keywords are reserved words in the language such as `fn`, `struct`, `enum`, `if`, `else`, `loop`, `while`, `return`, `break`, `continue`, `let`, `const`, `mod`, `pub`, `use`, `as`, and others.

## Step 2: Parsing
The token stream is then fed into a parser that constructs a concrete syntax tree (CST) based on grammar. The CST represents the exact structure of the source code, including all syntactic elements and represents precedence and associativity of operators using a hierarchy of nodes.

For example, consider the following source code:

```rs
fn main() {
    let x = 1i32 + 2i32 * 3i32;
}
```

The corresponding CST would start with a `Source` node containing a single `Fn` item representing the `main` function. The function body then contains a `Block` with a single `Let` statement. The expression on the right side of the assignment is represented by a deeply nested structure of nodes that encodes the language's precedence and associativity rules, with `*` having higher precedence than `+`, and both being left-associative. Every token is also annotated with its original `Span` to facilitate error reporting later in the compilation process.
```rs
Source {
    items: [
        Fn(
            FnItem {
                visibility: None,
                name: Ident {
                    value: "main",
                    span: Span {
                        start: 3,
                        end: 7,
                        line: 1,
                        col: 4,
                        indent: 0,
                        file: 1,
                    },
                },
                params: [],
                ret_ty: None,
                body: Block {
                    stmts: [
                        Let(
                            LetStmt {
                                pattern: Ident(
                                    Ident {
                                        value: "x",
                                        span: Span {
                                            start: 20,
                                            end: 21,
                                            line: 2,
                                            col: 9,
                                            indent: 4,
                                            file: 1,
                                        },
                                    },
                                ),
                                ty: None,
                                expr: Some(
                                    Assign(
                                        Range(
                                            LogicalOr(
                                                LogicalAnd(
                                                    Comparison(
                                                        BitwiseOr(
                                                            BitwiseXor(
                                                                BitwiseAnd(
                                                                    Shift(
                                                                        AddSub(
                                                                            AddSub {
                                                                                left: MulDivMod(
                                                                                    Cast(
                                                                                        Unary(
                                                                                            FieldAccess(
                                                                                                Primary(
                                                                                                    Literal(
                                                                                                        Int(
                                                                                                            IntLiteral {
                                                                                                                value: "1",
                                                                                                                radix: Decimal,
                                                                                                                suffix: Some(
                                                                                                                    "i32",
                                                                                                                ),
                                                                                                                span: Span {
                                                                                                                    start: 24,
                                                                                                                    end: 28,
                                                                                                                    line: 2,
                                                                                                                    col: 13,
                                                                                                                    indent: 4,
                                                                                                                    file: 1,
                                                                                                                },
                                                                                                            },
                                                                                                        ),
                                                                                                    ),
                                                                                                ),
                                                                                            ),
                                                                                        ),
                                                                                    ),
                                                                                ),
                                                                                op: Add,
                                                                                right: MulDivMod {
                                                                                    left: Cast(
                                                                                        Unary(
                                                                                            FieldAccess(
                                                                                                Primary(
                                                                                                    Literal(
                                                                                                        Int(
                                                                                                            IntLiteral {
                                                                                                                value: "2",
                                                                                                                radix: Decimal,
                                                                                                                suffix: Some(
                                                                                                                    "i32",
                                                                                                                ),
                                                                                                                span: Span {
                                                                                                                    start: 31,
                                                                                                                    end: 35,
                                                                                                                    line: 2,
                                                                                                                    col: 20,
                                                                                                                    indent: 4,
                                                                                                                    file: 1,
                                                                                                                },
                                                                                                            },
                                                                                                        ),
                                                                                                    ),
                                                                                                ),
                                                                                            ),
                                                                                        ),
                                                                                    ),
                                                                                    op: Mul,
                                                                                    right: Unary(
                                                                                        FieldAccess(
                                                                                            Primary(
                                                                                                Literal(
                                                                                                    Int(
                                                                                                        IntLiteral {
                                                                                                            value: "3",
                                                                                                            radix: Decimal,
                                                                                                            suffix: Some(
                                                                                                                "i32",
                                                                                                            ),
                                                                                                            span: Span {
                                                                                                                start: 38,
                                                                                                                end: 42,
                                                                                                                line: 2,
                                                                                                                col: 27,
                                                                                                                indent: 4,
                                                                                                                file: 1,
                                                                                                            },
                                                                                                        },
                                                                                                    ),
                                                                                                ),
                                                                                            ),
                                                                                        ),
                                                                                    ),
                                                                                    span: Span {
                                                                                        start: 31,
                                                                                        end: 42,
                                                                                        line: 2,
                                                                                        col: 20,
                                                                                        indent: 4,
                                                                                        file: 1,
                                                                                    },
                                                                                },
                                                                                span: Span {
                                                                                    start: 24,
                                                                                    end: 42,
                                                                                    line: 2,
                                                                                    col: 13,
                                                                                    indent: 4,
                                                                                    file: 1,
                                                                                },
                                                                            },
                                                                        ),
                                                                    ),
                                                                ),
                                                            ),
                                                        ),
                                                    ),
                                                ),
                                            ),
                                        ),
                                    ),
                                ),
                                else_block: None,
                                span: Span {
                                    start: 16,
                                    end: 43,
                                    line: 2,
                                    col: 5,
                                    indent: 4,
                                    file: 1,
                                },
                            },
                        ),
                    ],
                    span: Span {
                        start: 10,
                        end: 45,
                        line: 1,
                        col: 11,
                        indent: 0,
                        file: 1,
                    },
                },
                span: Span {
                    start: 0,
                    end: 45,
                    line: 1,
                    col: 1,
                    indent: 0,
                    file: 1,
                },
            },
        ),
    ],
}
```

## Step 3: Type Checking
### Step 3.1: Lower to High-level Intermediate Representation (HIR)
The CST is transformed into a high-level intermediate representation (HIR) that abstracts away syntactic details and focuses on the structure of the code. At this point, all expressions and blocks are annotated with scopes, all items are collected into a global symbol table, and the exact ordering and structure of the source code is lost. Rather, all further analysis is done on individual items and can thus can be done in parallel. All function bodies and other expressions are lowered to HIR, meaning that precedence is no longer encoded directly in the type definitions of the tree structure, but rather through explicit nodes representing a single expression at a time.

Continuing the previous example, the global set of our values would now contain a single function `main` and a single variable `x` within its body. Technically it also contains the function definitions of the two built-in functions `print` and `print_i32`, but those are omitted here for clarity. Each expression is now represented as a single node, with no need to tunnel through multiple layers to encode precendence and associativity. As well, the HIR attaches type information to each value and expression, though at this point most types are still unknown and represented as `Inferred` (although some are statically known, such as the unit type for function return types when none is specified). Each scope is also represented explicitly and contains a variety of names and information which are omitted here since it results in an infinite expansion of the output.
```rs
[
    Fn {
        id: 6,
        visibility: Private,
        name: Ident {
            value: "main",
            span: Span {
                start: 3,
                end: 7,
                line: 1,
                col: 4,
                indent: 0,
                file: 1,
            },
        },
        ret_ty: Resolved(
            Unit,
        ),
        params: [],
        scope: Scope {
            id: 4,
            value_ns: None,
        },
        body: Block {
            stmts: [
                Let {
                    pattern: Ident(
                        Ident {
                            value: "x",
                            span: Span {
                                start: 20,
                                end: 21,
                                line: 2,
                                col: 9,
                                indent: 4,
                                file: 1,
                            },
                        },
                    ),
                    value: Var {
                        id: 5,
                        name: Ident {
                            value: "x",
                            span: Span {
                                start: 20,
                                end: 21,
                                line: 2,
                                col: 9,
                                indent: 4,
                                file: 1,
                            },
                        },
                        ty: Inferred,
                        moved: false,
                        partially_moved: [],
                    },
                    ty: Inferred,
                    expr: Some(
                        BinaryOp {
                            left: Literal(
                                Int {
                                    value: IntLiteral {
                                        value: "1",
                                        radix: Decimal,
                                        suffix: Some(
                                            "i32",
                                        ),
                                        span: Span {
                                            start: 24,
                                            end: 28,
                                            line: 2,
                                            col: 13,
                                            indent: 4,
                                            file: 1,
                                        },
                                    },
                                    ty: Inferred,
                                    span: Span {
                                        start: 24,
                                        end: 28,
                                        line: 2,
                                        col: 13,
                                        indent: 4,
                                        file: 1,
                                    },
                                },
                            ),
                            op: Add,
                            right: BinaryOp {
                                left: Literal(
                                    Int {
                                        value: IntLiteral {
                                            value: "2",
                                            radix: Decimal,
                                            suffix: Some(
                                                "i32",
                                            ),
                                            span: Span {
                                                start: 31,
                                                end: 35,
                                                line: 2,
                                                col: 20,
                                                indent: 4,
                                                file: 1,
                                            },
                                        },
                                        ty: Inferred,
                                        span: Span {
                                            start: 31,
                                            end: 35,
                                            line: 2,
                                            col: 20,
                                            indent: 4,
                                            file: 1,
                                        },
                                    },
                                ),
                                op: Mul,
                                right: Literal(
                                    Int {
                                        value: IntLiteral {
                                            value: "3",
                                            radix: Decimal,
                                            suffix: Some(
                                                "i32",
                                            ),
                                            span: Span {
                                                start: 38,
                                                end: 42,
                                                line: 2,
                                                col: 27,
                                                indent: 4,
                                                file: 1,
                                            },
                                        },
                                        ty: Inferred,
                                        span: Span {
                                            start: 38,
                                            end: 42,
                                            line: 2,
                                            col: 27,
                                            indent: 4,
                                            file: 1,
                                        },
                                    },
                                ),
                                ty: Inferred,
                                span: Span {
                                    start: 31,
                                    end: 42,
                                    line: 2,
                                    col: 20,
                                    indent: 4,
                                    file: 1,
                                },
                            },
                            ty: Inferred,
                            span: Span {
                                start: 24,
                                end: 42,
                                line: 2,
                                col: 13,
                                indent: 4,
                                file: 1,
                            },
                        },
                    ),
                    else_expr: None,
                    scope: Scope {
                        id: 6,
                        value_ns: Some(
                            {
                                "x": Var {
                                    id: 5,
                                    name: Ident {
                                        value: "x",
                                        span: Span {
                                            start: 20,
                                            end: 21,
                                            line: 2,
                                            col: 9,
                                            indent: 4,
                                            file: 1,
                                        },
                                    },
                                    ty: Inferred,
                                    moved: false,
                                    partially_moved: [],
                                },
                            },
                        ),
                    },
                    span: Span {
                        start: 16,
                        end: 43,
                        line: 2,
                        col: 5,
                        indent: 4,
                        file: 1,
                    },
                },
            ],
            ty: Inferred,
            scope: Scope {
                id: 6,
                value_ns: Some(
                    {
                        "x": Var {
                            id: 5,
                            name: Ident {
                                value: "x",
                                span: Span {
                                    start: 20,
                                    end: 21,
                                    line: 2,
                                    col: 9,
                                    indent: 4,
                                    file: 1,
                                },
                            },
                            ty: Inferred,
                            moved: false,
                            partially_moved: [],
                        },
                    },
                ),
            },
            span: Span {
                start: 10,
                end: 45,
                line: 1,
                col: 11,
                indent: 0,
                file: 1,
            },
        },
        ty: Inferred,
    },
    Var {
        id: 5,
        name: Ident {
            value: "x",
            span: Span {
                start: 20,
                end: 21,
                line: 2,
                col: 9,
                indent: 4,
                file: 1,
            },
        },
        ty: Inferred,
        moved: false,
        partially_moved: [],
    },
]
```


### Step 3.2: Item resolution
Since all items require that their types are given explicitly, the first step in type checking is to add all items to the appropriate scopes. This ensures that all names are defined, even though the exact type of their fields is not yet known.

### Step 3.3: Use resolution
Next, all `use` statements are resolved to ensure that all imported names are valid and are included in the correct scopes. By the end of this step, all scopes are fully resolved and include all names in the type and value namespaces that are accessible from that scope, no more name definition will occur. Use items are not currently implemented, however this same constraint holds after Step 3.2.

### Step 3.4: Resolve explicit types
Next, all items, expressions, and statements with explicit type annotations are checked and resolved according to the names available in their respective scopes. This includes function parameters, return types, struct fields, enum variants, constant types, and variable declarations with explicit types. By the end of this step, all types (structs, enums, etc) are fully resolved and only expressions within function bodies and constant initializers remain with unknown types.

### Step 3.5: Type inference
Here, every value is traversed and its type is inferred based on the explicit annotations already resolved, the types of literals, and any expressions with statically known types (control flow expressions always return the never type). This process may require multiple passes over the entire set of values to propagate types correctly, as soon as a pass is completed without any changes, a final pass is done to ensure all types are fully resolved. If any types remain unknown after this step, a type error is reported.

### Step 3.6: Ownership checking
Finally, all values are traversed to ensure that ownership rules are followed. This includes checking that values are not used after being moved and that references do not outlive the values they point to. To do so, each value (variable in this case) is annotated with its ownership status (owned, moved, partially moved (field of struct moved), referencing another value). As scopes are created and destroyed, and as values are assigned and moved, this ownership status is updated accordingly and type errors are reported when violations occur. This step also adds some free instructions to the HIR to ensure that values are properly dropped when they go out of scope, some of this is handled in code generation as well.

## Step 4: Code Generation
After type checking is complete, the HIR is transformed into a lower-level intermediate representation (IR) suitable for code generation. This IR constrains the HIR to be fully typed, removing all options for unknown types, and further simplifies complex expressions into simpler ones (such as simplifying while expressions into loop expressions with conditional breaks). This IR is then used to generate target code, in the case of WASM this is a vector of WASM instructions per function, which is then assmembled into a complete WASM module.

To do so, the code generator finds the main function and converts it to IR, by doing so it converts all types, values (including functions used), and expressions used within that function or nested functions to IR, thereby performing dead code elimination. The resulting set of values for our example would be as follows:
```rs
[
    Fn {
        id: 2,
        ret_ty: Unit,
        params: [],
        body: Block {
            stmts: [
                Let {
                    value: Var {
                        id: 3,
                        ty: I32,
                        offset: 0,
                    },
                    ty: I32,
                    expr: Some(
                        BinaryOp {
                            left: Literal(
                                I32(
                                    1,
                                ),
                            ),
                            op: Add,
                            right: BinaryOp {
                                left: Literal(
                                    I32(
                                        2,
                                    ),
                                ),
                                op: Mul,
                                right: Literal(
                                    I32(
                                        3,
                                    ),
                                ),
                                ty: I32,
                            },
                            ty: I32,
                        },
                    ),
                },
                Expr(
                    Free {
                        expr: Literal(
                            Unit,
                        ),
                        values: [
                            (
                                Var {
                                    id: 3,
                                    ty: I32,
                                    offset: 0,
                                },
                                [],
                            ),
                        ],
                    },
                ),
            ],
            ty: Unit,
        },
        locals: [
            Var {
                id: 3,
                ty: I32,
                offset: 0,
            },
        ],
        ty: Fn {
            id: 7,
            param_tys: [],
            ret_ty: Unit,
        },
        idx: 0,
        orig_id: 6,
    },
    Var {
        id: 3,
        ty: I32,
        offset: 0,
    },
]
```

To convert this to WASM, the code generator traverses the tree and generates the instructions (encoded as an `Instr` enum) for each expression before converting the entire thing to WebAssembly Text Format (WAT) alongside any generated code for types (cleanup, equality), built-in functions, and a prelude of other utility functions used for memory management and other tasks. The resulting WAT is then fed into `wat2wasm` to produce the final WASM binary module.

### Memory Layout
Bay has three kinds of types when it comes to memory layout: copy types, allocated types, and scratch types. Copy types are types that can be copied bitwise without any special handling such as `i32` and `bool` all copy types fit within a WASM `i32` so are stored directly on the WASM stack or the Bay stack as needed and have no ownership semantics since they are always copied. Allocated types are types that require heap allocation such as structs, tuples, and arrays. They are stored on the Bay heap and are always behind a pointer (WASM `i32`), even when assigned as struct field or in a variable. This is what allows for simple recursive data types since structs are never present on the stack or in a struct field directly, but rather always behind a pointer. These types have ownership semantics and must be properly freed from the heap when they go out of scope. Scratch types are currently only used for subrange types and are effectively copy types that are greater than 32 bits in size. They are stored directly on the stack or in struct fields, but when they are used in expressions or any other situation where a copy type would be on the WASM stack, they are copied to the Bay scratch space and a pointer to that space is used instead. When values of scratch types go out of scope, the scratch space is simply reset to free the memory.
