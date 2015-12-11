#[no_mangle]
pub extern "C" fn test_func2(x: f64) -> f64
{
  x
}

#[no_mangle]
pub extern "C" fn test_func3(x: &[f64]) -> f64
{
  x[0]
}