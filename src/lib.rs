pub mod error;
use std::{collections::HashMap, fs, path::Path};

use serde::{Deserialize, Serialize};
use shaderc::{
    CompileOptions, Compiler as ShadercCompiler, EnvVersion, OptimizationLevel as ShadercOpt,
    ShaderKind, SourceLanguage, SpirvVersion, TargetEnv,
};

pub use error::*;

/// Supported input languages for Bento shader compilation.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum ShaderLang {
    Slang,
    Glsl,
    Hlsl,
    Other,
}

/// Controls how aggressively Bento optimizes shader bytecode.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum OptimizationLevel {
    None,
    FileSize,
    Performance,
}

/// Representation of a bind group variable discovered during reflection.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ShaderVariable {
    pub name: String,
    #[serde(default)]
    pub set: u32,
    pub kind: dashi::BindGroupVariable,
}

/// Stage-specific metadata discovered during reflection.
#[derive(Default, Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ShaderMetadata {
    pub entry_points: Vec<String>,
    pub inputs: Vec<InterfaceVariable>,
    pub outputs: Vec<InterfaceVariable>,
    pub workgroup_size: Option<[u32; 3]>,
}

/// Representation of a shader interface variable (inputs/outputs).
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct InterfaceVariable {
    pub name: String,
    pub location: Option<u32>,
}

/// Parameters describing how a shader should be compiled into a Bento File.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Request {
    pub name: Option<String>,
    pub lang: ShaderLang,
    pub stage: dashi::ShaderType,
    pub optimization: OptimizationLevel,
    pub debug_symbols: bool,
}

/// Serialized result produced after compiling a shader into the Bento Format.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CompilationResult {
    pub name: Option<String>,
    pub file: Option<String>,
    pub lang: ShaderLang,
    pub stage: dashi::ShaderType,
    pub variables: Vec<ShaderVariable>,
    pub metadata: ShaderMetadata,
    pub spirv: Vec<u32>,
}

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
impl ShaderMetadata {
    pub fn vertex_inputs(&self) -> Vec<dashi::VertexEntryInfo> {
        self.inputs.iter().map(|a| {
            // TODO: Get format from input, location, and calculate offsets assuming data is
            // packed.
            dashi::VertexEntryInfo {
                format: todo!(),
                location: todo!(),
                offset: todo!(),
            }
        }).collect()
    }
}

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

    pub fn bind_group_variables(&self) -> Vec<dashi::BindGroupVariable> {
        let s: Vec<dashi::BindGroupVariable> =
            self.variables.iter().map(|a| a.kind.clone()).collect();

        return s;
    }
}

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

/// High-level wrapper around shaderc that emits Bento Files.
pub struct Compiler {
    compiler: ShadercCompiler,
}

impl Compiler {
    pub fn new() -> Result<Self, BentoError> {
        let compiler = ShadercCompiler::new()
            .ok_or_else(|| BentoError::ShaderCompilation("Failed to initialize compiler".into()))?;

        Ok(Self { compiler })
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
        options.set_target_spirv(SpirvVersion::V1_3);
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
        let spirv_bytes = artifact.as_binary_u8();
        let variables = reflect_bindings(spirv_bytes)?;
        let metadata = reflect_metadata(spirv_bytes)?;

        Ok(CompilationResult {
            name: request.name.clone(),
            file: None,
            lang: request.lang,
            stage: request.stage,
            variables,
            metadata,
            spirv,
        })
    }

    pub fn compile_from_file(
        &self,
        path: &str,
        request: &Request,
    ) -> Result<CompilationResult, BentoError> {
        let bytes = fs::read(path)
            .map_err(|e| BentoError::Io(std::io::Error::new(e.kind(), format!("{path}: {e}"))))?;
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

    for (set, bindings) in descriptor_sets.iter() {
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
                set: *set,
                kind: dashi::BindGroupVariable {
                    var_type,
                    binding: *binding,
                    count,
                },
            });
        }
    }

    variables.sort_by(|a, b| a.set.cmp(&b.set).then_with(|| a.kind.binding.cmp(&b.kind.binding)));

    Ok(variables)
}

fn reflect_metadata(spirv_bytes: &[u8]) -> Result<ShaderMetadata, BentoError> {
    use rspirv_reflect::{spirv, Reflection};

    let reflection = Reflection::new_from_spirv(spirv_bytes)
        .map_err(|e| BentoError::ShaderCompilation(e.to_string()))?;
    let module = &reflection.0;

    let mut names = HashMap::new();
    for instruction in &module.debug_names {
        if instruction.class.opcode == spirv::Op::Name {
            if let (Some(rspirv_reflect::rspirv::dr::Operand::IdRef(id)), Some(rspirv_reflect::rspirv::dr::Operand::LiteralString(name))) =
                (instruction.operands.get(0), instruction.operands.get(1))
            {
                let id = *id;
                names.insert(id, name.clone());
            }
        }
    }

    let mut locations = HashMap::new();
    for instruction in &module.annotations {
        if instruction.class.opcode == spirv::Op::Decorate {
            if let (
                Some(rspirv_reflect::rspirv::dr::Operand::IdRef(id)),
                Some(rspirv_reflect::rspirv::dr::Operand::Decoration(decoration)),
                Some(rspirv_reflect::rspirv::dr::Operand::LiteralBit32(location)),
            ) = (
                instruction.operands.get(0),
                instruction.operands.get(1),
                instruction.operands.get(2),
            ) {
                if *decoration == spirv::Decoration::Location {
                    let id = *id;
                    locations.insert(id, *location);
                }
            }
        }
    }

    let mut inputs = Vec::new();
    let mut outputs = Vec::new();
    for instruction in &module.types_global_values {
        if instruction.class.opcode != spirv::Op::Variable {
            continue;
        }

        let Some(id) = instruction.result_id else { continue };
        let Some(rspirv_reflect::rspirv::dr::Operand::StorageClass(storage_class)) =
            instruction.operands.get(0)
        else {
            continue;
        };

        let name = names
            .get(&id)
            .cloned()
            .unwrap_or_else(|| format!("var_{id}"));
        let location = locations.get(&id).copied();
        let variable = InterfaceVariable { name, location };

        match storage_class {
            spirv::StorageClass::Input => inputs.push(variable),
            spirv::StorageClass::Output => outputs.push(variable),
            _ => {}
        }
    }

    inputs.sort_by(|a, b| a.location.cmp(&b.location).then_with(|| a.name.cmp(&b.name)));
    outputs.sort_by(|a, b| a.location.cmp(&b.location).then_with(|| a.name.cmp(&b.name)));

    let entry_points = module
        .entry_points
        .iter()
        .filter_map(|instruction| {
            if instruction.class.opcode != spirv::Op::EntryPoint {
                return None;
            }

            match instruction.operands.get(2) {
                Some(rspirv_reflect::rspirv::dr::Operand::LiteralString(name)) => {
                    Some(name.clone())
                }
                _ => None,
            }
        })
        .collect();

    let workgroup_size = reflection
        .get_compute_group_size()
        .map(|(x, y, z)| [x, y, z]);

    Ok(ShaderMetadata {
        entry_points,
        inputs,
        outputs,
        workgroup_size,
    })
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
                set: 0,
                kind: dashi::BindGroupVariable {
                    var_type: dashi::BindGroupVariableType::Uniform,
                    binding: 0,
                    count: 1,
                },
            }],
            metadata: ShaderMetadata {
                entry_points: vec!["main".to_string()],
                inputs: vec![],
                outputs: vec![],
                workgroup_size: Some([1, 1, 1]),
            },
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
        let compiler = Compiler::new()?;
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
        assert!(result.metadata.entry_points.contains(&"main".to_string()));
        assert_eq!(result.metadata.workgroup_size, Some([1, 1, 1]));

        Ok(())
    }

    #[test]
    fn compiles_shader_from_file() -> Result<(), BentoError> {
        let compiler = Compiler::new()?;
        let request = sample_request();
        let path = "tests/fixtures/simple_compute.glsl";

        let result = compiler.compile_from_file(path, &request)?;

        assert_eq!(result.file.as_deref(), Some(path));
        assert!(!result.spirv.is_empty());
        assert!(result.metadata.entry_points.contains(&"main".to_string()));

        Ok(())
    }

    #[test]
    fn returns_error_for_missing_file() {
        let compiler = Compiler::new().unwrap();
        let request = sample_request();
        let missing_path = "tests/fixtures/does_not_exist.glsl";

        let err = compiler
            .compile_from_file(missing_path, &request)
            .unwrap_err();

        assert!(matches!(err, BentoError::Io(_)));
    }

    #[test]
    fn returns_error_for_invalid_shader() {
        let compiler = Compiler::new().unwrap();
        let request = sample_request();
        let shader = b"#version 450\nvoid main() {";

        let err = compiler.compile(shader, &request).unwrap_err();

        assert!(matches!(err, BentoError::ShaderCompilation(_)));
    }
}
