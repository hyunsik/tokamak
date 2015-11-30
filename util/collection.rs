//! Collection Utility

/// Clone a vector consisting of item references to 
/// a new vector owning cloned items.
#[inline]
pub fn to_owned_vec<T: Clone>(r: &Vec<&T>) -> Vec<T> {
  r.iter().map(|v| (*v).clone()).collect::<Vec<T>>()
}

#[test]
pub fn test() {
  let x = 3; let y = 4;
  
  let ref_vec : Vec<&usize> = vec![&x, &y];  
  let expected: Vec<usize> = vec![x, y];  
  assert_eq!(to_owned_vec(&ref_vec), expected);
}