/// This row implementation is basically based on Pax, 
/// but it has a variation in terms of variable-length blocks.

use alloc::heap;
use std::marker;

use types::Type;

pub static ALIGNED_SIZE: usize = 16;

pub type ColumnId = u16;

pub fn compute_aligned_size(size: usize) -> usize { 
  let remain = size % ALIGNED_SIZE;
  
  if remain > 0 {
    size + (ALIGNED_SIZE - remain)
  } else {
    size
  }
}  

pub trait PageBuilder {
  fn get_minipage(&self, cid: ColumnId) -> &MiniPage;
  fn build(&mut self) -> &mut Page;
}

pub struct DefaultPageBuilder {
  page : Page   
}

impl DefaultPageBuilder 
{
  pub fn new(types: &Vec<Box<Type>>) -> DefaultPageBuilder {
    
    let mini_pages = types
      .iter()
      .map(|ty| {
        ty.create_minipage()
      })
      .collect::<Vec<Box<MiniPage>>>();
    
    DefaultPageBuilder {
      page: Page { mini_pages: mini_pages }
    }
  }
}

impl PageBuilder for DefaultPageBuilder {
  fn get_minipage(&self, cid: ColumnId) -> &MiniPage {
    &*self.page.mini_pages[cid as usize]
  }
  
  fn build(&mut self) -> &mut Page {
    &mut self.page    
  }
}

pub struct Page {
  mini_pages: Vec<Box<MiniPage>> 
}

impl Page {
  fn column_num(&self) -> u32 { self.mini_pages.len() as u32 }
}

pub trait MiniPage {
  fn size_in_bytes(&self) -> u32;
}

pub struct FMiniPage<'a> {
  ptr: *mut u8,
  len: u32,
  _marker: marker::PhantomData<&'a ()>  
}

impl<'a> FMiniPage<'a> {
  pub fn new(ty: Box<Type>) -> FMiniPage<'a> {
    let alloc_size = compute_aligned_size(4 as usize * 1024);

    let ptr = unsafe { heap::allocate(alloc_size, 16) };

    FMiniPage {
      ptr: ptr,
      len: alloc_size as u32,
      _marker: marker::PhantomData
    }
  }
}

impl<'a> Drop for FMiniPage<'a> {
  fn drop(&mut self) {
    unsafe {
      heap::deallocate(self.ptr as *mut u8, self.len as usize, 16);
    }
  }
}

impl<'a> MiniPage for FMiniPage<'a> {
  fn size_in_bytes(&self) -> u32 {
    self.len
  }
}