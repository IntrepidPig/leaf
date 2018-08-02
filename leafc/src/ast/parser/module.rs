use ast::parser::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
	pub uses: Vec<PathItem<Identifier>>,
	pub types: Vec<TypeDefinition>,
	pub functions: Vec<FunctionDefinition>,
	pub extern_fns: Vec<ExternFunction>,
	pub modules: Vec<(Identifier, Module)>,
}

impl Module {
	pub fn new() -> Self {
		Module {
			uses: Vec::new(),
			types: Vec::new(),
			functions: Vec::new(),
			extern_fns: Vec::new(),
			modules: Vec::new(),
		}
	}
	
	pub fn traverse_mut<F: FnMut(&ModulePath, &mut Module)>(&mut self, f: &mut F, start_path: &mut ModulePath) {
		f(&start_path, self);
		for (name, module) in &mut self.modules {
			start_path.path.push(name.clone());
			module.traverse_mut(f, start_path);
			start_path.path.pop().unwrap();
		}
	}

	pub fn traverse<F: FnMut(&ModulePath, &Module)>(&self, f: &mut F, start_path: &mut ModulePath) {
		f(&start_path, self);
		for (name, module) in &self.modules {
			start_path.path.push(name.clone());
			module.traverse(f, start_path);
			start_path.path.pop().unwrap();
		}
	}
}

pub fn next_module(stream: &mut TokenStream) -> ParseResult<(Identifier, Module)> {
	if let TokenTree::Token(Token { kind: TokenKind::Keyword(Keyword::Module), .. }) = stream.take_tokentree()? { } else {
		return Ok(None);
	}
	
	let name = if let TokenTree::Token(Token { kind: TokenKind::Name(ref name), .. }) = stream.take_tokentree()? {
		Identifier::try_from_str(name)
	} else {
		return Err(ParseError::expected(vec![Expected::Block], &*stream).into());
	};
	
	let module = if let TokenTree::Block(Bracket::Curly, ref mut block_token_stream, _, _) = stream.take_tokentree()? {
		parse_module(block_token_stream)?
	} else {
		return Err(ParseError::expected(vec![Expected::Block], &*stream).into());
	};
	
	Ok(Some((name, module)))
}

pub fn parse_module(stream: &mut TokenStream) -> Result<Module, Error<ParseError>> {
	let mut module = Module::new();
	
	while !stream.is_empty() {
		if let Some((name, submodule)) = next_module(stream)? {
			stream.commit();
			module.modules.push((name, submodule));
			continue;
		} else {
			stream.reset();
		}
		
		if let Some(functiondef) = next_functiondef(stream)? {
			stream.commit();
			module.functions.push(functiondef);
			continue;
		} else {
			stream.reset();
		}
		
		return Err(ParseError {
			kind: ParseErrorKind::UnexpectedToken(stream.next_token().unwrap().clone()),
			span: stream.current_span,
		}.into());
	}
	
	Ok(module)
}