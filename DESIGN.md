# Design

## Pipeline
Start with a tokenizer, then the token stream gets consumed to be a parsed into a concrete representation (an AST that slowly gets hydrated with type and scope information as type checking and other passes are performed). Then, once it passes all the checks, it gets converted into a lower level representation (IR) that is then optimized and reordered as needed. It is at this stage where things like parallel calculus and other optimizations can be performed. Finally, the completed IR is converted into the target language (x86, WASM, bytecode, etc) and can be run as needed.

## Commands

### `build`
Build the project. This will run the entire pipeline from source to target language, outputting a relevant file for the target.

### `run`
Build and run the project. This runs the entire pipeline similar to `build`, but then runs the output file.

### `test`
Build the project and run the tests. This runs the entire pipeline, but rather than outputting and running a normal executable it outputs and runs a test one.

### `check`
Check the project for errors. This runs the pipeline up to the end of the AST stage, it performs no optimizations or code generation.

### `format`
Format all files in the project. This runs the parsing portion of the pipeline, however runs no type checking or other passes, just simply the AST construction pass. Then it writes the formatted code to the relevant source files.
