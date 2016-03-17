// Represents the data associated with a compilation
// session for a single crate.
pub struct Session;

// Err(0) means compilation was stopped, but no errors were found.
// This would be better as a dedicated enum, but using try! is so convenient.
pub type CompileResult = Result<(), usize>;

pub fn compile_result_from_err_count(err_count: usize) -> CompileResult {
    if err_count == 0 {
        Ok(())
    } else {
        Err(err_count)
    }
}