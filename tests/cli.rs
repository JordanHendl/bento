use assert_cmd::cargo::cargo_bin_cmd;
use predicates::prelude::*;
use std::fs;
use std::path::PathBuf;

#[test]
fn compiles_shader_via_cli() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let requested_output = tmp_dir.path().join("simple_compute");
    let actual_output = requested_output.with_extension("bto");

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
            requested_output.to_str().unwrap(),
            "--name",
            "simple_compute",
            "--verbose",
        ])
        .assert()
        .success();

    assert!(actual_output.exists());

    let result =
        bento::CompilationResult::load_from_disk(actual_output.to_str().unwrap()).unwrap();
    assert_eq!(result.stage, dashi::ShaderType::Compute);
    assert_eq!(result.lang, bento::ShaderLang::Glsl);
    assert!(result.variables.len() > 0);
    assert!(!result.spirv.is_empty());

    fs::remove_file(actual_output).ok();
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
            "target/should_not_exist.bto",
        ])
        .assert()
        .failure()
        .stderr(predicate::str::contains("does_not_exist.glsl"));
}

#[test]
fn defaults_output_to_out_bto() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let shader = PathBuf::from("tests/fixtures/simple_compute.glsl")
        .canonicalize()
        .unwrap();
    let expected_output = tmp_dir.path().join("out.bto");

    cargo_bin_cmd!("bentosc")
        .current_dir(tmp_dir.path())
        .args([
            shader.to_str().unwrap(),
            "--stage",
            "compute",
            "--lang",
            "glsl",
        ])
        .assert()
        .success();

    assert!(expected_output.exists());
}

#[test]
fn coerces_output_extension_to_bto() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let shader = PathBuf::from("tests/fixtures/simple_compute.glsl");
    let requested_output = tmp_dir.path().join("custom_output.bin");
    let expected_output = requested_output.with_extension("bto");

    cargo_bin_cmd!("bentosc")
        .args([
            shader.to_str().unwrap(),
            "--stage",
            "compute",
            "--lang",
            "glsl",
            "--output",
            requested_output.to_str().unwrap(),
        ])
        .assert()
        .success();

    assert!(expected_output.exists());
}
