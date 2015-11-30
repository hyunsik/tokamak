//! Collection Utility

/// Clone a vector consisting of item references to 
/// a new vector owning cloned items.
#[inline]
pub fn to_owned_vec<T: Clone>(v: &Vec<&T>) -> Vec<T> 
{
  v.iter().map(|e| (*e).clone()).collect::<Vec<T>>()
}

/// Convert Vec<T> into Vec<Box<T>>
///
/// It is usually used for arguments of nested Struct or Enum.
pub fn to_boxed_vec<T>(v: Vec<T>) -> Vec<Box<T>>
{
  v.into_iter().map(|e| Box::new(e)).collect::<Vec<Box<T>>>()
}

/// Convert Vec<Box<T>> to Vec<T>
pub fn strip_box_from_vec<T>(v: Vec<Box<T>>) -> Vec<T>
{
  v.into_iter().map(|e: Box<T>| *e).collect::<Vec<T>>()
}

#[inline]

#[test]
pub fn test() {
  let x = 3; let y = 4;
  
  let ref_vec : Vec<&usize> = vec![&x, &y];  
  let expected: Vec<usize> = vec![x, y];  
  assert_eq!(to_owned_vec(&ref_vec), expected);
}