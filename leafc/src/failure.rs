use std::{
	fmt::{Debug, Display, Formatter, self},
	error::{Error as StdError}
};

use backtrace::Backtrace;

pub struct Error<T: Failure> {
	error: T,
	bt: Backtrace,
}

impl<T: StdError> Error<T> {
	pub fn new(error: T) -> Self {
		Error {
			error,
			bt: Backtrace::new(),
		}
	}
}

impl<T: StdError> Display for Error<T> {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "{}\n{:?}", self.error, self.bt)
	}
}

impl<T: StdError> Debug for Error<T> {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "{}\n{:?}", self.error, self.bt)
	}
}


impl<T: StdError> From<T> for Error<T> where T: Failure + 'static {
	fn from(t: T) -> Error<T> {
		Error::new(t)
	}
}

pub trait Failure: Display {

}

impl<T> Failure for T where T: StdError {
	
}

pub struct GenericError {
	error: Box<Failure>,
	bt: Backtrace,
}

impl GenericError {
	pub fn new<T: Failure + 'static>(error: T) -> Self {
		GenericError {
			error: Box::new(error),
			bt: Backtrace::new(),
		}
	}
}

impl<T> From<T> for GenericError where T: Failure + 'static {
	fn from(t: T) -> GenericError {
		GenericError::new(t)
	}
}

impl Display for GenericError {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "{}\n{:?}", self.error, self.bt)
	}
}

impl Debug for GenericError {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "{}\n{:?}", self.error, self.bt)
	}
}
