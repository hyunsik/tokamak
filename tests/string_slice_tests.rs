extern crate tajo;

use tajo::common::StringSlice;

#[test]
fn test_init() {
  let s1 = StringSlice::new_from_str("yate");
  let s2 = StringSlice::new_from_str("가나다");
  let s3 = StringSlice::new_from_str("hyunsik");
  assert_eq!(s1.len(), 4);
  assert_eq!(s2.len(), 9);
  assert_eq!(s3.len(), 7);
}

#[test]
fn test_cmp_eq() {
  let s1 = StringSlice::new_from_str("rust");
  let s2 = StringSlice::new_from_str("rust");
  let s3 = StringSlice::new_from_str("hyunsik");

  assert_eq!(s1, s2);
  assert!(!(s2 == s3));
}

#[test]
fn test_cmp_ne() {
  let s1 = StringSlice::new_from_str("rust");
  let s2 = StringSlice::new_from_str("yate");
  let s3 = StringSlice::new_from_str("hyunsik");
  let s4 = StringSlice::new_from_str("rust");

  assert!(s1 != s2);
  assert!(s1 != s3);
  assert!(!(s1 != s4));
}

#[test]
fn test_cmp_lt() {
  let str1 = StringSlice::new_from_str("aaaaa");
  let str2 = StringSlice::new_from_str("bbbbb");
  assert!(str1 < str2);

  let str3 = StringSlice::new_from_str("aaaaa");
  let str4 = StringSlice::new_from_str("bbbb");
  assert!(str3 < str4);

  let str5 = StringSlice::new_from_str("aaaa");
  let str6 = StringSlice::new_from_str("bbbbb");
  assert!(str5 < str6);

  let str7 = StringSlice::new_from_str("bbbbb");
  let str8 = StringSlice::new_from_str("aaaaa");
  assert!(!(str7 < str8));

  let str9 = StringSlice::new_from_str("bbbb");
  let str10 = StringSlice::new_from_str("aaaaa");
  assert!(!(str9 < str10));

  let str11 = StringSlice::new_from_str("bbbbb");
  let str12 = StringSlice::new_from_str("aaaa");
  assert!(!(str11 < str12));
}

#[test]
fn test_cmp_le() {
  let str1 = StringSlice::new_from_str("aaaaa");
  let str2 = StringSlice::new_from_str("aaaaa");
  assert!(str1 <= str2);

  let str3 = StringSlice::new_from_str("aaaaa");
  let str4 = StringSlice::new_from_str("aaaab");
  assert!(str3 <= str4);

  let str5 = StringSlice::new_from_str("aaaa");
  let str6 = StringSlice::new_from_str("aaaab");
  assert!(str5 <= str6);

  let str7 = StringSlice::new_from_str("aaaaa");
  let str8 = StringSlice::new_from_str("aaab");
  assert!(str7 <= str8);
}

#[test]
fn test_cmp_gt() {
  let str1 = StringSlice::new_from_str("aaaab");
  let str2 = StringSlice::new_from_str("aaaaa");
  assert!(str1 > str2);

  let str3 = StringSlice::new_from_str("abaaa");
  let str4 = StringSlice::new_from_str("aaaa");
  assert!(str3 > str4);

  let str5 = StringSlice::new_from_str("ab");
  let str6 = StringSlice::new_from_str("aaaa");
  assert!(str5 > str6);

  let str7 = StringSlice::new_from_str("aaaaa");
  let str8 = StringSlice::new_from_str("aaaab");
  assert!(!(str7 > str8));

  let str9 = StringSlice::new_from_str("aaaa");
  let str10 = StringSlice::new_from_str("abaaa");
  assert!(!(str9 > str10));

  let str11 = StringSlice::new_from_str("aaaa");
  let str12 = StringSlice::new_from_str("abaaa");
  assert!(!(str11 > str12));
}

#[test]
fn test_cmp_ge() {
  let str1 = StringSlice::new_from_str("aaaaa");
  let str2 = StringSlice::new_from_str("aaaaa");
  assert!(str1 >= str2);

  let str3 = StringSlice::new_from_str("aaaab");
  let str4 = StringSlice::new_from_str("aaaaa");
  assert!(str3 >= str4);

  let str5 = StringSlice::new_from_str("aaaab");
  let str6 = StringSlice::new_from_str("aaaa");
  assert!(str5 >= str6);

  let str7 = StringSlice::new_from_str("aaab");
  let str8 = StringSlice::new_from_str("aaaaa");
  assert!(str7 >= str8);
}