pub static ALIGNED_SIZE: usize = 16;

pub fn compute_aligned_size(size: usize) -> usize { 
  let remain = size % ALIGNED_SIZE;
  
  if remain > 0 {
    size + (ALIGNED_SIZE - remain)
  } else {
    size
  }
}