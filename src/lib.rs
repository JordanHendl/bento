pub mod error;
use std::path::Path;

pub use error::*;

pub enum ShaderLang {
    Slang,
    Glsl,
    Hlsl,
    Other,
}

pub enum OptimizationLevel {
    None,
    FileSize,
    Performance,
}

pub struct ShaderVariable {
    pub name: String,
    pub kind: dashi::BindGroupVariable,
}

pub struct Request {
    pub name: Option<String>,
    pub lang: ShaderLang,
    pub stage: dashi::ShaderType,
    pub optimization: OptimizationLevel,
    pub debug_symbols: bool,
}

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
        todo!()
    }

    pub fn to_bytes(&self) -> Result<Vec<u8>, BentoError> {
        todo!()
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<Self, BentoError> {
        todo!()
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
