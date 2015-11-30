//! Collection Utility

pub mod vec {  
  /// Clone a vector consisting of item references to 
  /// a new vector owning cloned items.
  #[inline]
  pub fn to_owned<T: Clone>(v: &Vec<&T>) -> Vec<T> 
  {
    v.iter().map(|e| (*e).clone()).collect::<Vec<T>>()
  }
  
  /// Convert Vec<T> into Vec<Box<T>>
  ///
  /// It is usually used for arguments of nested Struct or Enum.
  pub fn to_boxed<T>(v: Vec<T>) -> Vec<Box<T>>
  {
    v.into_iter().map(|e| Box::new(e)).collect::<Vec<Box<T>>>()
  }
  
  /// Convert Vec<Box<T>> to Vec<T>
  pub fn strip_box<T>(v: Vec<Box<T>>) -> Vec<T>
  {
    v.into_iter().map(|e: Box<T>| *e).collect::<Vec<T>>()
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  
  #[test]
  pub fn test() {
    let x = 3; let y = 4;
    
    let ref_vec : Vec<&usize> = vec![&x, &y];  
    let expected: Vec<usize> = vec![x, y];  
    assert_eq!(vec::to_owned(&ref_vec), expected);
  }
}