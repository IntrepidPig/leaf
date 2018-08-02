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

pub fn next_pathitem<T>(
	stream: &mut TokenStream,
	f: fn(&mut TokenStream) -> ParseResult<T>,
) -> ParseResult<PathItem<T>> {
	let module_path = if let Some(path) = next_path(stream)? {
		path
	} else {
		return Err(ParseError::expected(vec![Expected::ModulePath], &*stream).into()); // unreachable as of now
	};

	let item = if let Some(item) = f(stream)? {
		item
	} else {
		return Ok(None);
	};

	Ok(Some(PathItem { module_path, item }))
}

pub fn next_path(stream: &mut TokenStream) -> ParseResult<ModulePath> {
	let mut relative = true;

	let old_position = stream.get_position();
	if let TokenTree::Token(Token {
		kind: TokenKind::Symbol(Symbol::Namespace),
		..
	}) = stream.take_tokentree()?
	{
		relative = false;
	} else {
		stream.seek(old_position);
	}

	let mut path: Vec<Identifier> = Vec::new();
	let mut old_position;
	loop {
		old_position = stream.get_position();
		let name = if let Some(TokenTree::Token(Token {
			kind: TokenKind::Name(ref name),
			..
		})) = stream.opt_next_tokentree()?
		{
			Identifier::try_from_str(name)
		} else {
			break;
		};

		if let Some(TokenTree::Token(Token {
			kind: TokenKind::Symbol(Symbol::Namespace),
			..
		})) = stream.opt_next_tokentree()?
		{

		} else {
			break;
		};
		path.push(name);
	}
	stream.seek(old_position);

	Ok(Some(ModulePath { relative, path }))
}
