/// Cache align size (TODO - should be improved to consider the platform)
pub static CACHE_LINE_SIZE : usize = 16;

pub fn get_aligned_size(size: usize) -> usize { 
  let remain = size % CACHE_LINE_SIZE;
  
  if remain > 0 {
    size + (CACHE_LINE_SIZE - remain)
  } else {
    size
  }
}