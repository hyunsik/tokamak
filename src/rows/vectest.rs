use std::marker;

trait Vector {
  fn size(&self) -> usize;

  //fn as_array<T: Sized>() -> &[T]
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

struct AssemblyRowBlock<'b> {
  vectors: Vec<Box<&'b Vector>>
}

impl<'b> AssemblyRowBlock<'b> {
  fn set_vector(&mut self, v: &'b Vector) {
    self.vectors.push(Box::new(v));
  }
}

impl<'b> RowBlock<'b> for AssemblyRowBlock<'b> {
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

  let mut ass = AssemblyRowBlock {vectors: Vec::new()};
  ass.set_vector(y.vector(0));
  let v: &Vector = ass.vector(0);
}