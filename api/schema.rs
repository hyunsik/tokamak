use rustc_serialize::{Encoder, Encodable, Decodable};

pub struct SchemaBuilder {
  types: Vec<String>
}

pub enum SchemaBuilderError {
  Unsupported
}

impl Encoder for SchemaBuilder {
  type Error = SchemaBuilderError;
  
  fn emit_nil(&mut self) -> Result<(), Self::Error> 
  { 
    println!("nil");
    Ok(())
  } 
  
  fn emit_usize(&mut self, v: usize) -> Result<(), Self::Error> 
  {
    println!("usize");
    Ok(())
  }
  
  fn emit_u64(&mut self, v: u64) -> Result<(), Self::Error> 
  {
    println!("u64");
    Ok(())
  }

  fn emit_u32(&mut self, v: u32) -> Result<(), Self::Error> 
  {
    println!("u32");
    Ok(())
  }
  
  fn emit_u16(&mut self, v: u16) -> Result<(), Self::Error> 
  {
    println!("u16");
    Ok(())
  }
    
  fn emit_u8(&mut self, v: u8) -> Result<(), Self::Error> 
  {
    println!("u8");
    Ok(())
  }
    
  fn emit_isize(&mut self, v: isize) -> Result<(), Self::Error>
  {
    println!("isize");
    Ok(())
  }
    
  fn emit_i64(&mut self, v: i64) -> Result<(), Self::Error>
  {
    println!("i64");
    Ok(())
  }
    
  fn emit_i32(&mut self, v: i32) -> Result<(), Self::Error>
  {
    println!("i32");
    Ok(())
  }
    
  fn emit_i16(&mut self, v: i16) -> Result<(), Self::Error>
  {
    println!("i16");
    Ok(())
  }
    
  fn emit_i8(&mut self, v: i8) -> Result<(), Self::Error>
  {
    println!("i8");
    Ok(())
  }
    
  fn emit_bool(&mut self, v: bool) -> Result<(), Self::Error>
  {
    println!("u8");
    Ok(())
  }
    
  fn emit_f64(&mut self, v: f64) -> Result<(), Self::Error>
  {
    println!("f64");
    Ok(())
  }
    
  fn emit_f32(&mut self, v: f32) -> Result<(), Self::Error>
  {
    println!("f32");
    Ok(())
  }
    
  fn emit_char(&mut self, v: char) -> Result<(), Self::Error>
  {
    println!("char");
    Ok(())
  }
    
  fn emit_str(&mut self, v: &str) -> Result<(), Self::Error>
  {
    println!("str");
    Ok(())
  }

  fn emit_enum<F>(&mut self, name: &str, f: F) -> Result<(), Self::Error>
    where F: FnOnce(&mut Self) -> Result<(), Self::Error>
  {
    println!("enum");
    Ok(())
  }
        
  fn emit_enum_variant<F>(&mut self, v_name: &str,
                          v_id: usize,
                          len: usize,
                          f: F) -> Result<(), Self::Error>
    where F: FnOnce(&mut Self) -> Result<(), Self::Error> 
  {
    println!("enum_variant");
    Ok(())
  }
        
  fn emit_enum_variant_arg<F>(&mut self, a_idx: usize, f: F) -> Result<(), Self::Error>
    where F: FnOnce(&mut Self) -> Result<(), Self::Error>
  {
    println!("enum_variant_arg");
    Ok(())
  }

  fn emit_enum_struct_variant<F>(&mut self, v_name: &str,
                                 v_id: usize,
                                 len: usize,
                                 f: F) -> Result<(), Self::Error>
    where F: FnOnce(&mut Self) -> Result<(), Self::Error>
  {
    println!("enum_struct_variant");
    Ok(())
  }
        
  fn emit_enum_struct_variant_field<F>(&mut self,
                                       f_name: &str,
                                       f_idx: usize,
                                       f: F) -> Result<(), Self::Error>
    where F: FnOnce(&mut Self) -> Result<(), Self::Error>
  {
    println!("enum_struct_variant_field");
    Ok(())
  }

  fn emit_struct<F>(&mut self, name: &str, len: usize, f: F)
                    -> Result<(), Self::Error>
    where F: FnOnce(&mut Self) -> Result<(), Self::Error> 
  {
    println!("struct enter: {}", name);
    let res = f(self);
    println!("struct leave: {}", name);
    
    res
  }
  
  fn emit_struct_field<F>(&mut self, f_name: &str, f_idx: usize, f: F)
                          -> Result<(), Self::Error>
    where F: FnOnce(&mut Self) -> Result<(), Self::Error>
  {
    println!("struct_field: {}::{}", f_idx, f_name);
    f(self)
  }

  fn emit_tuple<F>(&mut self, len: usize, f: F) -> Result<(), Self::Error>
    where F: FnOnce(&mut Self) -> Result<(), Self::Error>
  {
    println!("tuple");
    Ok(())
  }
  
  fn emit_tuple_arg<F>(&mut self, idx: usize, f: F) -> Result<(), Self::Error>
    where F: FnOnce(&mut Self) -> Result<(), Self::Error>
  {
    println!("tuple_arg");
    Ok(())
  }

  fn emit_tuple_struct<F>(&mut self, name: &str, len: usize, f: F)
                          -> Result<(), Self::Error>
    where F: FnOnce(&mut Self) -> Result<(), Self::Error>
  {
    println!("tuple_struct");
    Ok(())
  }

  fn emit_tuple_struct_arg<F>(&mut self, f_idx: usize, f: F)
                              -> Result<(), Self::Error>
    where F: FnOnce(&mut Self) -> Result<(), Self::Error>
  {
    println!("tuple_struct_arg");
    Ok(())
  }

  // Specialized types:
  fn emit_option<F>(&mut self, f: F) -> Result<(), Self::Error>
    where F: FnOnce(&mut Self) -> Result<(), Self::Error>
  {
    println!("option");
    Ok(())
  }
        
  fn emit_option_none(&mut self) -> Result<(), Self::Error>
  {
    println!("option_none");
    Ok(())
  }
  
  fn emit_option_some<F>(&mut self, f: F) -> Result<(), Self::Error>
    where F: FnOnce(&mut Self) -> Result<(), Self::Error>
  {
    println!("option_some");
    Ok(())
  }

  fn emit_seq<F>(&mut self, len: usize, f: F) -> Result<(), Self::Error>
    where F: FnOnce(&mut Self) -> Result<(), Self::Error>
  {
    println!("seq");
    Ok(())
  }
  
  fn emit_seq_elt<F>(&mut self, idx: usize, f: F) -> Result<(), Self::Error>
    where F: FnOnce(&mut Self) -> Result<(), Self::Error>
   {
     println!("seq_elt");
     Ok(())
   }

  fn emit_map<F>(&mut self, len: usize, f: F) -> Result<(), Self::Error>
    where F: FnOnce(&mut Self) -> Result<(), Self::Error>
  {
    println!("emit_map");
    Ok(())
  }
  
  fn emit_map_elt_key<F>(&mut self, idx: usize, f: F) -> Result<(), Self::Error>
    where F: FnOnce(&mut Self) -> Result<(), Self::Error>
  {
    println!("map_elt_key");
    Ok(())
  }
  
  fn emit_map_elt_val<F>(&mut self, idx: usize, f: F) -> Result<(), Self::Error>
    where F: FnOnce(&mut Self) -> Result<(), Self::Error>
  {
    println!("map_elt_val");
    Ok(())
  }
}

#[derive(RustcEncodable)]
pub struct Test {
  id  : usize,
  name: String
} 

#[test]
pub fn test() {
  let mut b = SchemaBuilder {types: Vec::new()};
  let t = Test {id: 1, name: "xxx".to_string()};
  //let x: Encodable = &t;
  t.encode(&mut b);
}


