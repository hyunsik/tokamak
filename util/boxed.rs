/// Utilities for Boxed Objects

pub trait ToBoxedVec<T>
{
	/// Covert self into a vector of boxed elements without clones and allocation.
	fn to_boxed(self) -> Vec<Box<T>>;
}

impl<T> ToBoxedVec<T> for Vec<T>
{
	fn to_boxed(self) -> Vec<Box<T>> 
	{
		self.into_iter().map(|e| Box::new(e)).collect::<Vec<Box<T>>>()
	}
}

pub trait ToUnboxedVec<T>
{
	fn to_unboxed(self) -> Vec<T>;
}

impl<T> ToUnboxedVec<T> for Vec<Box<T>>
{
	fn to_unboxed(self) -> Vec<T>
	{
		self.into_iter().map(|e| *e).collect::<Vec<T>>()
	}
}