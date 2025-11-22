pub mod error;
use std::{fs, path::Path};

use serde::{Deserialize, Serialize};

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

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        todo!()
    }

    pub fn compile(
        &self,
        shader: &[u8],
        request: &Request,
    ) -> Result<CompilationResult, BentoError> {
        todo!()
    }

    pub fn compile_from_file(
        &self,
        path: &str,
        request: &Request,
    ) -> Result<CompilationResult, BentoError> {
        todo!()
    }
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
}
