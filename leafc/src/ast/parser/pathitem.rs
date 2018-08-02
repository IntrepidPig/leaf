use ast::parser::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PathItem<T> {
	pub module_path: ModulePath,
	pub item: T,
}

impl<T> PathItem<T> {
	pub fn map<O, F: Fn(T) -> O>(self, f: F) -> PathItem<O> {
		PathItem {
			module_path: self.module_path,
			item: f(self.item),
		}
	}
}

impl ::std::fmt::Display for PathItem<Identifier> {
	fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
		if !self.module_path.relative {
			write!(f, "::")?;
		}
		for module in &self.module_path.path {
			write!(f, "{}::", module.name)?;
		}
		write!(f, "{}", self.item.name)?;

		Ok(())
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModulePath {
	pub relative: bool,
	pub path: Vec<Identifier>,
}

impl ModulePath {
	pub fn new(relative: bool, path: Vec<Identifier>) -> Self {
		ModulePath { relative, path }
	}
}

pub fn next_path(stream: &mut TokenStream) -> ParseResult<ModulePath> {
	unimplemented!()
}
