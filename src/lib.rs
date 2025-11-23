pub mod error;
use std::{fs, path::Path};

use serde::{Deserialize, Serialize};
use shaderc::{
    CompileOptions, Compiler as ShadercCompiler, EnvVersion, OptimizationLevel as ShadercOpt,
    ShaderKind, SourceLanguage, TargetEnv,
};

pub use error::*;

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum ShaderLang {
    Slang,
    Glsl,
    Hlsl,
    Other,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum OptimizationLevel {
    None,
    FileSize,
    Performance,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ShaderVariable {
    pub name: String,
    pub kind: dashi::BindGroupVariable,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Request {
    pub name: Option<String>,
    pub lang: ShaderLang,
    pub stage: dashi::ShaderType,
    pub optimization: OptimizationLevel,
    pub debug_symbols: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CompilationResult {
    pub name: Option<String>,
    pub file: Option<String>,
    pub lang: ShaderLang,
    pub stage: dashi::ShaderType,
    pub variables: Vec<ShaderVariable>,
    pub spirv: Vec<u32>,
}

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

impl CompilationResult {
    pub fn save_to_disk(&self, path: &str) -> Result<(), BentoError> {
        let path = Path::new(path);

        if let Some(parent) = path.parent() {
            if !parent.as_os_str().is_empty() {
                fs::create_dir_all(parent)?;
            }
        }

        let bytes = self.to_bytes()?;
        fs::write(path, bytes)?;

        Ok(())
    }

    pub fn to_bytes(&self) -> Result<Vec<u8>, BentoError> {
        Ok(bincode::serialize(self)?)
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<Self, BentoError> {
        Ok(bincode::deserialize(bytes)?)
    }

    pub fn load_from_disk(path: &str) -> Result<Self, BentoError> {
        let bytes = fs::read(path)?;
        Self::from_bytes(&bytes)
    }
}

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

pub struct Compiler {
    compiler: ShadercCompiler,
}

impl Compiler {
    pub fn new() -> Self {
        let compiler =
            ShadercCompiler::new().expect("Unable to construct shaderc compiler instance");

        Self { compiler }
    }

    pub fn compile(
        &self,
        shader: &[u8],
        request: &Request,
    ) -> Result<CompilationResult, BentoError> {
        let source = std::str::from_utf8(shader)
            .map_err(|_| BentoError::InvalidInput("Shader source is not valid UTF-8".into()))?;

        let mut options = CompileOptions::new()
            .ok_or_else(|| BentoError::ShaderCompilation("Failed to create options".into()))?;

        options.set_source_language(source_language(request.lang)?);
        options.set_target_env(TargetEnv::Vulkan, EnvVersion::Vulkan1_2 as u32);
        options.set_optimization_level(shaderc_optimization(request.optimization));

        if request.debug_symbols {
            options.set_generate_debug_info();
        }

        let shader_kind = shader_stage(request.stage)?;

        let artifact = self
            .compiler
            .compile_into_spirv(
                source,
                shader_kind,
                request.name.as_deref().unwrap_or("shader"),
                "main",
                Some(&options),
            )
            .map_err(|e| BentoError::ShaderCompilation(e.to_string()))?;

        let spirv = artifact.as_binary().to_vec();
        let variables = reflect_bindings(artifact.as_binary_u8())?;

        Ok(CompilationResult {
            name: request.name.clone(),
            file: None,
            lang: request.lang,
            stage: request.stage,
            variables,
            spirv,
        })
    }

    pub fn compile_from_file(
        &self,
        path: &str,
        request: &Request,
    ) -> Result<CompilationResult, BentoError> {
        let bytes = fs::read(path)?;
        let mut result = self.compile(&bytes, request)?;
        result.file = Some(path.to_string());

        Ok(result)
    }
}

fn shader_stage(stage: dashi::ShaderType) -> Result<ShaderKind, BentoError> {
    match stage {
        dashi::ShaderType::Vertex => Ok(ShaderKind::Vertex),
        dashi::ShaderType::Fragment => Ok(ShaderKind::Fragment),
        dashi::ShaderType::Compute => Ok(ShaderKind::Compute),
        dashi::ShaderType::All => Err(BentoError::InvalidInput(
            "ShaderType::All is not supported for compilation".into(),
        )),
    }
}

fn source_language(lang: ShaderLang) -> Result<SourceLanguage, BentoError> {
    match lang {
        ShaderLang::Glsl => Ok(SourceLanguage::GLSL),
        ShaderLang::Hlsl | ShaderLang::Slang => Ok(SourceLanguage::HLSL),
        ShaderLang::Other => Err(BentoError::InvalidInput(
            "Unsupported shader language".into(),
        )),
    }
}

fn shaderc_optimization(level: OptimizationLevel) -> ShadercOpt {
    match level {
        OptimizationLevel::None => ShadercOpt::Zero,
        OptimizationLevel::FileSize => ShadercOpt::Size,
        OptimizationLevel::Performance => ShadercOpt::Performance,
    }
}

fn reflect_bindings(spirv_bytes: &[u8]) -> Result<Vec<ShaderVariable>, BentoError> {
    use rspirv_reflect::{BindingCount, DescriptorType, Reflection};

    let reflection = Reflection::new_from_spirv(spirv_bytes)
        .map_err(|e| BentoError::ShaderCompilation(e.to_string()))?;

    let mut variables = Vec::new();

    let descriptor_sets = reflection
        .get_descriptor_sets()
        .map_err(|e| BentoError::ShaderCompilation(e.to_string()))?;

    for bindings in descriptor_sets.values() {
        for (binding, info) in bindings.iter() {
            let var_type = match info.ty {
                DescriptorType::UNIFORM_BUFFER => dashi::BindGroupVariableType::Uniform,
                DescriptorType::UNIFORM_BUFFER_DYNAMIC => {
                    dashi::BindGroupVariableType::DynamicUniform
                }
                DescriptorType::STORAGE_BUFFER => dashi::BindGroupVariableType::Storage,
                DescriptorType::STORAGE_BUFFER_DYNAMIC => {
                    dashi::BindGroupVariableType::DynamicStorage
                }
                DescriptorType::SAMPLED_IMAGE => dashi::BindGroupVariableType::SampledImage,
                DescriptorType::STORAGE_IMAGE => dashi::BindGroupVariableType::StorageImage,
                DescriptorType::COMBINED_IMAGE_SAMPLER => {
                    dashi::BindGroupVariableType::SampledImage
                }
                _ => dashi::BindGroupVariableType::Uniform,
            };

            let count = match info.binding_count {
                BindingCount::One => 1,
                BindingCount::StaticSized(value) => value as u32,
                BindingCount::Unbounded => 0,
            };

            variables.push(ShaderVariable {
                name: info.name.clone(),
                kind: dashi::BindGroupVariable {
                    var_type,
                    binding: *binding,
                    count,
                },
            });
        }
    }

    variables.sort_by_key(|v| v.kind.binding);

    Ok(variables)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{
        fs,
        time::{SystemTime, UNIX_EPOCH},
    };

    fn sample_compilation_result() -> CompilationResult {
        CompilationResult {
            name: Some("example".to_string()),
            file: Some("shader.glsl".to_string()),
            lang: ShaderLang::Glsl,
            stage: dashi::ShaderType::Compute,
            variables: vec![ShaderVariable {
                name: "u_time".to_string(),
                kind: dashi::BindGroupVariable {
                    var_type: dashi::BindGroupVariableType::Uniform,
                    binding: 0,
                    count: 1,
                },
            }],
            spirv: vec![0x0723_0203, 1, 2, 3],
        }
    }

    #[test]
    fn round_trips_with_binary_serialization() -> Result<(), BentoError> {
        let original = sample_compilation_result();
        let bytes = original.to_bytes()?;
        let restored = CompilationResult::from_bytes(&bytes)?;

        assert_eq!(original, restored);

        Ok(())
    }

    #[test]
    fn saves_and_loads_from_disk() -> Result<(), BentoError> {
        let original = sample_compilation_result();
        let unique_suffix = format!(
            "{}_{}",
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos(),
            std::process::id()
        );
        let path = std::env::temp_dir()
            .join("bento_tests")
            .join(format!("compilation_result_{}.bin", unique_suffix));

        original.save_to_disk(path.to_str().unwrap())?;
        let loaded = CompilationResult::load_from_disk(path.to_str().unwrap())?;

        assert_eq!(original, loaded);

        fs::remove_file(&path).ok();

        Ok(())
    }

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
    fn compiles_shader_source() -> Result<(), BentoError> {
        let compiler = Compiler::new();
        let shader = include_str!("../tests/fixtures/simple_compute.glsl");
        let request = sample_request();

        let result = compiler.compile(shader.as_bytes(), &request)?;

        assert_eq!(result.name, request.name);
        assert_eq!(result.file, None);
        assert_eq!(result.stage, dashi::ShaderType::Compute);
        assert_eq!(result.lang, ShaderLang::Glsl);
        assert!(!result.spirv.is_empty());
        assert!(!result.variables.is_empty());
        assert_eq!(result.variables[0].kind.binding, 0);
        assert_eq!(
            result.variables[0].kind.var_type,
            dashi::BindGroupVariableType::Storage
        );

        Ok(())
    }

    #[test]
    fn compiles_shader_from_file() -> Result<(), BentoError> {
        let compiler = Compiler::new();
        let request = sample_request();
        let path = "tests/fixtures/simple_compute.glsl";

        let result = compiler.compile_from_file(path, &request)?;

        assert_eq!(result.file.as_deref(), Some(path));
        assert!(!result.spirv.is_empty());

        Ok(())
    }

    #[test]
    fn returns_error_for_missing_file() {
        let compiler = Compiler::new();
        let request = sample_request();
        let missing_path = "tests/fixtures/does_not_exist.glsl";

        let err = compiler
            .compile_from_file(missing_path, &request)
            .unwrap_err();

        assert!(matches!(err, BentoError::Io(_)));
    }

    #[test]
    fn returns_error_for_invalid_shader() {
        let compiler = Compiler::new();
        let request = sample_request();
        let shader = b"#version 450\nvoid main() {";

        let err = compiler.compile(shader, &request).unwrap_err();

        assert!(matches!(err, BentoError::ShaderCompilation(_)));
    }
}
