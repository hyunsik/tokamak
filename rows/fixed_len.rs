pub struct FixedLenVector<'a> {
  ptr: *mut u8,
  size: u32,     // allocated size
  _marker: marker::PhantomData<&'a ()>  
}

impl<'a> FixedLenVector<'a> {
  pub fn new(fixed_len: usize) -> FixedLenVector<'a> {
    let alloc_size = compute_aligned_size(fixed_len * ROWBATCH_SIZE);

    let ptr = unsafe { heap::allocate(alloc_size, 16) };

    FixedLenVector {
      ptr: ptr,
      size: alloc_size as u32,
      _marker: marker::PhantomData
    }
  }
  
  #[inline]
  pub fn as_ptr(&self) -> *const u8 {
    self.ptr
  }
  
  #[inline]
  pub fn as_mut_ptr(&mut self) -> *mut u8 {
    self.ptr
  }
}

impl<'a> Drop for FixedLenVector<'a> {
  fn drop(&mut self) {
    unsafe {
      heap::deallocate(self.ptr as *mut u8, self.size as usize, 16);
    }
  }
}

#[inline]
fn read_fixed_len_value<T>(ptr: *const u8, pos: RowId) -> T where T: Copy {
  debug_assert!(pos < ROWBATCH_SIZE);
  unsafe {
    let array: &[T] = slice::from_raw_parts(ptr as *const T, 1024);
    *array.get_unchecked(pos)
  }
}

impl<'a> Vector for FixedLenVector<'a> {
  fn size_in_bytes(&self) -> u32 {
    self.size
  }
  
  fn read_i8(&self, pos: RowId) -> i8 {
    read_fixed_len_value(self.ptr, pos)
  }
  
  fn read_i16(&self, pos: RowId) -> i16 {
    read_fixed_len_value(self.ptr, pos)
  }
  
  fn read_i32(&self, pos: RowId) -> i32 {
    read_fixed_len_value(self.ptr, pos)
  }
  
  fn read_i64(&self, pos: RowId) -> i64 {
    read_fixed_len_value(self.ptr, pos)
  }
  
  fn read_f32(&self, pos: RowId) -> f32 {
    read_fixed_len_value(self.ptr, pos)
  }
  
  fn read_f64(&self, pos: RowId) -> f64 {
    read_fixed_len_value(self.ptr, pos)
  }
}

pub struct FixedLenVectorBuilder<'a> {
  fixed_len: usize,
  pos : RowId,
  vector: FixedLenVector<'a>    
}

impl<'a> FixedLenVectorBuilder<'a> 
{
  pub fn new(fixed_len: usize) -> FixedLenVectorBuilder<'a> {
    
    FixedLenVectorBuilder {
      fixed_len: fixed_len,
      pos: 0,
      vector: FixedLenVector::new(fixed_len)
    }
  }
}

#[inline]
fn write_fixed_value<T>(vec: &mut FixedLenVector, pos: usize, val: T) {
  debug_assert!(pos < ROWBATCH_SIZE);
  unsafe {
    let array: &mut [T] = slice::from_raw_parts_mut(vec.as_mut_ptr() as *mut T, ROWBATCH_SIZE);
    (*array.get_unchecked_mut(pos)) = val;
  }
}

impl<'a> VectorWriter for FixedLenVectorBuilder<'a> 
{
  #[inline]
  fn write_i8(&mut self, v: i8) {
    write_fixed_value(&mut self.vector, self.pos, v);
    self.pos = self.pos + 1;
  }
  
  #[inline]
  fn write_i16(&mut self, v: i16) {
    write_fixed_value(&mut self.vector, self.pos, v);
    self.pos = self.pos + 1;
  }
  
  #[inline]
  fn write_i32(&mut self, v: i32) {
    write_fixed_value(&mut self.vector, self.pos, v);
    self.pos = self.pos + 1;
  }
  
  #[inline]
  fn write_i64(&mut self, v: i64) {
    write_fixed_value(&mut self.vector, self.pos, v);
    self.pos = self.pos + 1;
  }
  
  #[inline]
  fn write_f32(&mut self, v: f32) {
    write_fixed_value(&mut self.vector, self.pos, v);
    self.pos = self.pos + 1;
  }

  #[inline]  
  fn write_f64(&mut self, v: f64) {
    write_fixed_value(&mut self.vector, self.pos, v);
    self.pos = self.pos + 1;
  }
  
  #[inline]
  fn write_bytes(&mut self, v: &[u8]) {
  }
  
  #[inline]
  fn reset(&mut self) {
    self.pos = 0;
  }
  
  #[inline]
  fn finalize(&mut self) {
    
  }
}