use assert_cmd::cargo::cargo_bin_cmd;
use predicates::prelude::*;
use std::fs;

#[test]
fn compiles_shader_via_cli() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let output = tmp_dir.path().join("simple_compute.bin");

    cargo_bin_cmd!("bentosc")
        .args([
            "tests/fixtures/simple_compute.glsl",
            "--stage",
            "compute",
            "--lang",
            "glsl",
            "--opt",
            "performance",
            "--output",
            output.to_str().unwrap(),
            "--name",
            "simple_compute",
            "--verbose",
        ])
        .assert()
        .success();

    assert!(output.exists());

    let result = bento::CompilationResult::load_from_disk(output.to_str().unwrap()).unwrap();
    assert_eq!(result.stage, dashi::ShaderType::Compute);
    assert_eq!(result.lang, bento::ShaderLang::Glsl);
    assert!(result.variables.len() > 0);
    assert!(!result.spirv.is_empty());

    fs::remove_file(output).ok();
}

#[test]
fn fails_gracefully_for_missing_shader() {
    cargo_bin_cmd!("bentosc")
        .args([
            "tests/fixtures/does_not_exist.glsl",
            "--stage",
            "compute",
            "--lang",
            "glsl",
            "--output",
            "target/should_not_exist.bin",
        ])
        .assert()
        .failure()
        .stderr(predicate::str::contains("does_not_exist.glsl"));
}
