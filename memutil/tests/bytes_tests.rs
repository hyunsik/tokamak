extern crate memutil;

use memutil::{ByteSize};

#[test]
fn test_arithmetic() {
  let x = ByteSize::mb(1);
  let y = ByteSize::kb(100);

  assert_eq!(
    (x + y).as_usize(),
    1100000
  );
  assert_eq!(
    (x + (100*1000)).as_usize(),
    1100000
  );
  
  assert_eq!(
    (x - y).as_usize(),
    900000
  );
  assert_eq!(
    (x - (100*1000)).as_usize(),
    900000
  );
}