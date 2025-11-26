use bento::CompilationResult;
use clap::{ArgAction, Parser};

/// CLI arguments for inspecting Bento Files and their metadata.
#[derive(Debug, Parser)]
#[command(author, version, about = "Inspect Bento shader artifacts", long_about = None)]
struct Args {
    /// Path to the Bento artifact to inspect
    file: String,

    /// Emit the artifact contents as pretty-printed JSON
    #[arg(long, action = ArgAction::SetTrue)]
    json: bool,
}

fn main() {
    if let Err(err) = run() {
        eprintln!("Error: {err}");
        std::process::exit(1);
    }
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let artifact = CompilationResult::load_from_disk(&args.file)?;

    if args.json {
        let json = serde_json::to_string_pretty(&artifact)?;
        println!("{json}");
    } else {
        print_summary(&artifact);
    }

    Ok(())
}

fn print_summary(result: &CompilationResult) {
    println!(
        "Entry: {}",
        result.name.as_deref().unwrap_or("<unnamed shader>")
    );

    if let Some(file) = &result.file {
        println!("Source: {file}");
    }

    println!("Language: {:?}", result.lang);
    println!("Stage: {:?}", result.stage);
    println!("Variables:");
    for var in &result.variables {
        println!(
            "  {} -> binding {} ({:?}), count {}",
            var.name, var.kind.binding, var.kind.var_type, var.kind.count
        );
    }

    println!("SPIR-V words: {}", result.spirv.len());
    let byte_size = result.spirv.len() * std::mem::size_of::<u32>();
    println!("Output size: {} bytes", byte_size);
}
