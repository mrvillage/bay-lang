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

## Step 3: Type Checking
### Step 3.1: Lower to High-level Intermediate Representation (HIR)
The CST is transformed into a high-level intermediate representation (HIR) that abstracts away syntactic details and focuses on the structure of the code. At this point, all expressions and blocks are annotated with scopes, all items are collected into a global symbol table, and the exact ordering and structure of the source code is lost. Rather, all further analysis is done on individual items and can thus can be done in parallel. All function bodies and other expressions are lowered to HIR, meaning that precedence is no longer encoded directly in the type definitions of the tree structure, but rather through explicit nodes representing a single expression at a time.

### Step 3.2: Item resolution
Since all items require that their types are given explicitly, the first step in type checking is to resolve all items in the global symbol table. This ensures that all names are defined, even though the exact type of their fields is not yet known.

### Step 3.3: Use resolution
Next, all `use` statements are resolved to ensure that all imported names are valid and are included in the correct scopes. By the end of this step, all scopes are fully resolved and include all names in the type and value namespaces that are accessible from that scope, no more name definition will occur. Use items are not currently implemented, however this same constraint holds after Step 3.2.

### Step 3.4: Resolve explicit types
Next, all items, expressions, and statements with explicit type annotations are checked and resolved according to the names available in their respective scopes. This includes function parameters, return types, struct fields, enum variants, constant types, and variable declarations with explicit types. By the end of this step, all types (structs, enums, etc) are fully resolved and only expressions within function bodies and constant initializers remain with unknown types.

### Step 3.5: Type inference
Here, every value is traversed and its type is inferred based on the explicit annotations already resolved, the types of literals, and any expressions with statically known types (control flow expressions always return the never type). This process may require multiple passes over the entire set of values to propagate types correctly, as soon as a pass is completed without any changes, a final pass is done to ensure all types are fully resolved. If any types remain unknown after this step, a type error is reported.

### Step 3.6: Ownership checking
Finally, all values are traversed to ensure that ownership rules are followed. This includes checking that values are not used after being moved and that references do not outlive the values they point to. To do so, each value (variable in this case) is annotated with its ownership status (owned, moved, partially moved (field of struct moved), referencing another value). As scopes are created and destroyed, and as values are assigned and moved, this ownership status is updated accordingly and type errors are reported when violations occur.

## Step 4: Code Generation
After type checking is complete, the HIR is transformed into a lower-level intermediate representation (IR) suitable for code generation. This IR constrains the HIR to be fully typed, removing all options for unknown types, and further simplifies complex expressions into simpler ones (such as simplifying while expressions into loop expressions with conditional breaks). This IR is then used to generate target code, in the case of WASM this is a vector of WASM instructions per function, which is then assmembled into a complete WASM module.
