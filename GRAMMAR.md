# Grammar

At the moment I have not defined a formal grammar, rather I have put below examples of all the various possible statements and things.

## Current
This is the current state of the language, it's a subset of what I eventually want it to be since it's a work in progress. I'm slowly adding more things.

```ebnf
program -> decl*
decl -> fn_decl | struct_decl | mod_decl | const_decl | impl_decl | enum_decl | use_decl
fn_decl -> [vis] "fn" ident "(" [fn_param ("," fn_param)* [","]] ")" ["->" type] fn_body
method_decl -> [vis] "fn" ident "(" self ("," fn_param)* [","] ")" ["->" type] fn_body
struct_decl -> [vis] "struct" ident "{" [struct_field ("," struct_field)* [","]] "}"
struct_field -> ident ":" type ["=" expr]
mod_decl -> [vis] "mod" ident (("{" [decl*] "}") | ";")
const_decl -> [vis] "const" ident ":" type "=" expr ";"
impl_decl -> [vis] "impl" [generic_params] type ["{" [impl_item ("," impl_item)* [","]] "}"]
impl_item -> fn_decl | method_decl | const_decl
enum_decl -> [vis] "enum" ident ["{" enum_variant ("," enum_variant)* [","]] ["{" [struct_field ("," struct_field)* [","]] "}"]
enum_variant -> ident [("(" [type ("," type)* [","]] ")") | ("{" [struct_field ("," struct_field)* [","]] "}")] ["=" expr]
use_decl -> [vis] "use" use_path ";"
use_path -> "*" | ((path | self) ["as" ident]) | (path "::" "{" use_path ("," use_path)* [","] "}")

fn_param -> ident ":" type ["=" expr]
vis -> "pub" ["(" ("pkg" | "super" | "in" abs_path) ")"]
self -> ["&"] ["mut"] "self" [":" type]
path -> [namespace "::"] ident ("::" ident)*
namespace -> "@" | "@" ident
type -> path ["::" generic_params]

fn_body -> "{" stmt* "}"
stmt -> expr | let_stmt
let_stmt -> "let" ident [":" type] ["=" expr] ";"

```

## Goal
This is the goal of the grammar, in the end this is what I want the language to look like.

```ebnf
program -> program_doc_comment* decl*
program_doc_comment -> ("//!" (!\n)* "\n")*
decl -> fn_decl | struct_decl | mod_decl | const_decl | impl_decl | trait_decl | enum_decl | type_decl | use_decl
fn_decl -> [vis] "fn" ident [generic_params] "(" [fn_param ("," fn_param)* [","]] ")" ["->" type] [where] fn_body
method_decl -> [vis] "fn" ident [generic_params] "(" self ("," fn_param)* [","] ")" ["->" type] [where] fn_body
struct_decl -> [vis] "struct" ident [generic_params] [where] [";" | ("{" [struct_field ("," struct_field)* [","]] "}")]
struct_field -> [vis] ident ":" type ["=" expr]
tuple_struct_field -> [vis] type ["=" expr]
mod_decl -> [vis] "mod" ident (("{" [decl*] "}") | ";")
const_decl -> [vis] "const" ident ":" type "=" expr ";"
impl_decl -> [vis] "impl" [generic_params] [type "for"] type [where] [";" | ("{" [impl_item ("," impl_item)* [","]] "}")]
impl_item -> fn_decl | method_decl | const_decl | type_decl
trait_decl -> [vis] "trait" ident [generic_params] [where] [";" | ("{" [trait_item ("," trait_item)* [","]] "}")]
trait_item -> fn_decl | method_decl | const_decl | type_decl | fn_proto_decl | method_proto_decl | const_proto_decl | type_proto_decl
fn_proto_decl -> [vis] "fn" ident [generic_params] "(" [fn_param ("," fn_param)* [","]] ")" ["->" type] [where] ";"
method_proto_decl -> [vis] "fn" ident [generic_params] "(" self ("," fn_param)* [","] ")" ["->" type] [where] ";"
const_proto_decl -> [vis] "const" ident ":" type ";"
type_proto_decl -> [vis] "type" ident [generic_params] [where] ";"
enum_decl -> [vis] "enum" ident [generic_params] ["{" enum_variant ("," enum_variant)* [","]] [where]
enum_variant -> ident [("(" [type ("," type)* [","]] ")") | ("{" [struct_field ("," struct_field)* [","]] "}")] ["=" expr]
type_decl -> [vis] "type" ident [generic_params] "=" type [where] ";"
use_decl -> [vis] "use" use_path ";"
use_path -> "*" | ((path | self) ["as" ident]) | (path "::" "{" use_path ("," use_path)* [","] "}")

fn_param -> ident ":" type ["=" expr]
vis -> "pub" ["(" ("pkg" | "super" | "in" abs_path) ")"]
self -> ["&" [lifetime]] ["mut"] "self" [":" type]
generic_params -> "<" generic_param ("," generic_param)* [","] ">"
generic_param -> lifetime | (type [":" type_bound] ["=" type]) | ("const" ident ":" type ["=" expr])
where -> "where" where_clause ("," where_clause)* [","]
where_clause -> ident ":" type

path -> [namespace "::"] ident ("::" ident)*
namespace -> "@" | "@" ident
lifetime -> "'" ident
type -> path ["::" generic_params]

fn_body -> "{" [stmt*] "}"
stmt -> decl | expr | let_stmt
let_stmt -> "let" ident [":" type] ["=" expr] ";"
```
