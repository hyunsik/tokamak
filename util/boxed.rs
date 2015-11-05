//! Utilities for Boxed Objects

/// Trait to covert self into Vec<Box<T>> 
pub trait ToBoxedVec<T>
{
	/// Covert self into Vec<Box<T>>
	fn to_boxed(self) -> Vec<Box<T>>;
}

impl<T> ToBoxedVec<T> for Vec<T>
{
	fn to_boxed(self) -> Vec<Box<T>> 
	{
		self.into_iter().map(|e| Box::new(e)).collect::<Vec<Box<T>>>()
	}
}

/// Trait to covert self into Vec<T>
pub trait ToVec<T>
{
	/// Covert self into Vec<T>
	fn to_vec(self) -> Vec<T>;
}

impl<T> ToVec<T> for Vec<Box<T>>
{
	fn to_vec(self) -> Vec<T>
	{
		self.into_iter().map(|e| *e).collect::<Vec<T>>()
	}
}