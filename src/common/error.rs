#[allow(non_camel_case_types)]

#[derive(Clone, Copy, Debug)]
pub enum Error {  
  COLUMN_NOT_FOUND,
  TABLE_NOT_FOUND,
  DATABASE_NOT_FOUND,

  LINE_DELIMITER_NOT_FOUND,
}