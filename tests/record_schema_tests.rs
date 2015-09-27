extern crate tajo;
extern crate common;

use common::schema::*;
use common::types::*;

#[test]
fn test_schema1() {
    let mut fields = Vec::new();
    fields.push(Field::scalar("col1".to_owned(), *INT4_TY));
    fields.push(Field::record("col2".to_owned(), Record::new(vec![
        Field::scalar("col3".to_owned(), *INT4_TY),
        Field::scalar("col4".to_owned(), *INT4_TY)
    ])));

    let r = Record::new(fields);
    println!("{}", r);
}
