# Grammar

```ebnf
source -> item*
item -> use_item | fn_item | struct_item | enum_item | const_item | mod_item
use_item -> [vis] use_tree
use_tree -> ["::"] ("{" use_tree ("," use_tree)* [","] "}" | "*" | ident ["as" ident | "::" use_tree])
vis -> "pub"
fn_item -> [vis] "fn" ident "(" [fn_param ("," fn_param)* [","]] ")" ["->" type] block
fn_param -> ident ":" type
struct_item -> [vis] "struct" ident ("{" [named_struct_field ("," named_struct_field)* [","] "}" | "(" [unnamed_struct_field ("," unnamed_struct_field)* [","]] ")" ";" | ";")
named_struct_field -> [vis] ident ":" type
unnamed_struct_field -> [vis] type
enum_item -> [vis] "enum" ident "{" [enum_variant ("," enum_variant)* [","]] "}"
enum_variant -> ident ("(" [type ("," type)* [","]] ")" | "{" [named_struct_field ("," named_struct_field)* [","]] "}" | "") ["=" expr]
const_item -> [vis] "const" ident ":" type "=" expr ";"
mod_item -> [vis] "mod" ident [("{" item* "}" | ";")]
type -> path | tuple_type | array_type | slice_type | ref_type | optional_type
path -> ["::"] ["pkg"] ident ("::" ident)*
tuple_type -> "(" [type ("," type)* [","]] ")"
array_type -> "[" type ";" expr "]"
slice_type -> "[" type "]"
ref_type -> "&" type
optional_type -> type "?"
// all expr statements require semicolons unless they are "block exprs," i.e., they are just blocks, if exprs, loop exprs, or while exprs
block -> "{" stmt* "}"
stmt -> let_stmt | expr [";"] | item
let_stmt -> "let" ident_pattern [":" type] ["=" expr] ["else" block] ";"
ident_pattern -> ident | "_" | "(" [ident_pattern ("," ident_pattern)* [","]] ")"
pattern -> "_" | path | "(" [pattern ("," pattern)* [","]] ")"
expr -> assign_expr | control_flow_kind [label] [range_expr]
control_flow_kind -> "return" | "break" | "continue"
label -> "$" ident
assign_expr -> range_expr | ident ["." ident] ["[" expr "]"] assign_op range_expr
assign_op -> "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "|=" | "&=" | "^=" | "<<=" | ">>="
range_expr -> logical_or_expr | [logical_or_expr] (".." | "..=") [logical_or_expr]
logical_or_expr -> logical_and_expr ( "||" logical_and_expr )*
logical_and_expr -> comparison_expr ( "&&" comparison_expr )*
// chained comparisons like a < b < c require parentheses, not sure how to represent this in EBNF
comparison_expr -> bitwise_or_expr ( comparison_op bitwise_or_expr )*
comparison_op -> "==" | "!=" | "<" | "<=" | ">" | ">="
bitwise_or_expr -> bitwise_xor_expr ( "|" bitwise_xor_expr )*
bitwise_xor_expr -> bitwise_and_expr ( "^" bitwise_and_expr )*
bitwise_and_expr -> shift_expr ( "&" shift_expr )*
shift_expr -> add_sub_expr ( shift_op add_sub_expr )*
shift_op -> "<<" | ">>"
add_sub_expr -> mul_div_mod_expr ( add_sub_op mul_div_mod_expr )*
add_sub_op -> "+" | "-"
mul_div_mod_expr -> cast_expr ( mul_div_mod_op cast_expr )*
mul_div_mod_op -> "*" | "/" | "%"
cast_expr -> unary_expr | cast_expr "as" type
unary_expr -> field_access_expr | unary_op unary_expr
unary_op -> "!" | "-" | "&"
field_access_expr -> field_access_expr "!" | field_access_expr "?" | field_access_expr "[" expr "]" | field_access_expr "(" [expr ("," expr)* [","]] ")" | field_access_expr "." ident | field_access_expr "." ident "(" [expr ("," expr)* [","]] ")" | primary_expr
primary_expr -> literal | path | "(" expr ")" | tuple_literal | array_literal | struct_expr | block | if_expr | loop_expr | while_expr | "(" ")"
tuple_literal -> "(" [expr ("," expr)* [","]] ")"
array_literal -> "[" [expr ("," expr)* [","]] "]"
struct_expr -> "new" path ("{" [struct_field_expr ("," struct_field_expr)* [","] [".." expr] "}" | "(" [expr ("," expr)* [","]] ")")
struct_field_expr -> ident [":" expr]
if_expr -> "if" expr block ["else" (if_expr | block)]
loop_expr -> [label ":"] "loop" block
while_expr -> [label ":"] "while" expr block
```
