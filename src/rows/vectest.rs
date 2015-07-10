use std::marker;

trait Vector {
  fn size(&self) -> usize;  
}

struct ArrayVector {
  array: [i32; 1]
}

impl Vector for ArrayVector {
 fn size(&self) -> usize {
  self.array.len()
 } 
}

trait RowBlock<'b> {
  fn vector(&'b self, cid: usize) -> &'b Vector;
}

struct XRowBlock<'b> {
  vectors: Vec<Box<&'b Vector>>
}

impl<'b> RowBlock<'b> for XRowBlock<'b> {
 fn vector(&self, cid: usize) -> &'b Vector {
   *self.vectors[cid]
 } 
}

struct YRowBlock<'a> {
  vectors: Vec<Box<Vector>>,
  _marker: marker::PhantomData<&'a ()>
}

impl<'a> YRowBlock<'a> {
  fn set_vector(&mut self, v: Box<Vector>) {
    self.vectors.push(v);
  }
}

impl<'a> RowBlock<'a> for YRowBlock<'a> {
 fn vector(&'a self, cid: usize) -> &'a Vector {   
   &*self.vectors[cid]
 }
}

#[test]
fn test_yrowblock() {
  let mut y = YRowBlock {vectors: Vec::new(), _marker: marker::PhantomData};
  let vector = ArrayVector {array: [0]};
  y.set_vector(Box::new(vector));
  let vec: &Vector = y.vector(0);
}