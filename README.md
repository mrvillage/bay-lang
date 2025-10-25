# bay-lang

Bay is a statically typed, compiled programming language with ownership semantics and syntax inspired by Rust. Currently it only compiles to WebAssembly Text Format (WAT) and requires [`wat2wasm`](https://github.com/WebAssembly/wabt) and [`wasmtime`](https://wasmtime.dev/) to run the generated code.

## Installation
Please ensure that you have Rust (and Cargo), `wat2wasm`, and `wasmtime` installed on your system. You can then install Bay using Cargo:

```bash
cargo install --git https://github.com/mrvillage/bay-lang.git
```

## Usage
```bash
bay new <project_name>   # Create a new Bay project
cd <project_name>        # Navigate into the project directory
bay run                # Compile and run the Bay project
```
