use bento::{BentoError, Compiler, OptimizationLevel, Request, ShaderLang};

fn sample_request() -> Request {
    Request {
        name: Some("sample".to_string()),
        lang: ShaderLang::Glsl,
        stage: dashi::ShaderType::Compute,
        optimization: OptimizationLevel::None,
        debug_symbols: false,
    }
}

#[test]
fn compiles_fixture_shader() -> Result<(), BentoError> {
    let compiler = Compiler::new()?;
    let request = sample_request();
    let path = "tests/fixtures/simple_compute.glsl";

    let result = compiler.compile_from_file(path, &request)?;

    assert_eq!(result.file.as_deref(), Some(path));
    assert_eq!(result.stage, dashi::ShaderType::Compute);
    assert_eq!(result.lang, ShaderLang::Glsl);
    assert!(!result.spirv.is_empty());
    assert!(!result.variables.is_empty());

    Ok(())
}

#[test]
fn returns_missing_file_error() {
    let compiler = Compiler::new().unwrap();
    let request = sample_request();
    let missing_path = "tests/fixtures/not_real_shader.glsl";

    let err = compiler.compile_from_file(missing_path, &request).unwrap_err();

    match err {
        BentoError::Io(io_err) => {
            assert!(io_err.to_string().contains(missing_path));
        }
        other => panic!("Unexpected error: {:?}", other),
    }
}

#[test]
fn fails_with_invalid_shader_source() {
    let compiler = Compiler::new().unwrap();
    let request = sample_request();

    let err = compiler
        .compile(b"#version 450\nvoid main() {", &request)
        .unwrap_err();

    assert!(matches!(err, BentoError::ShaderCompilation(_)));
}
