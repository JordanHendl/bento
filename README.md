[BENTO]

Crate to package up shaders and their associated info into a binary file format. Also supports inspecting and loading this format.

Aiming to support GLSL, HLSL, and Slang reflection.

Output binary format is SPIR-V.

## CLI usage

The `bentocc` binary compiles shader sources into serialized `CompilationResult` files. Typical usage:

```
bentocc tests/fixtures/simple_compute.glsl \
    --stage compute \
    --lang glsl \
    --opt performance \
    --output target/simple_compute.bin \
    --name simple_compute \
    --verbose
```

The command prints metadata about the compiled shader when `--verbose` is provided and writes the binary artifact to the path specified by `--output`.
