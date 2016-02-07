//! Utility for Strings

pub fn to_owned_vec(strs: &Vec<&str>) -> Vec<String> {
  strs.iter()
      .map(|x| x.to_string())
      .collect::<Vec<String>>()
}
