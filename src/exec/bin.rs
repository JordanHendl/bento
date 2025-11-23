use bento::*;

fn main() {
    let args = std::env::args();
    let compiler = Compiler::new().expect("Unable to construct compiler");
    
    // Fill out request from args
    let request = Request {
        name: todo!(),
        lang: todo!(),
        stage: todo!(),
        optimization: todo!(),
        debug_symbols: todo!(),
    };
    let verbose = todo!(); // From args...
    let path = todo!(); // From Args...

    let res = compiler.compile_from_file(path, &request).expect("Unable to compile file!");

    if verbose {
        // print result information...
        todo!()
    }
    
    res.save_to_disk(todo!());
}
