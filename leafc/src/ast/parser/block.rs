use ast::parser::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
	pub statements: Vec<Expression>,
	pub output: Option<Expression>,
}

impl Block {
	pub fn new() -> Self {
		Block {
			statements: Vec::new(),
			output: None,
		}
	}

	pub fn traverse_expressions_mut<F: FnMut(&mut Expression)>(&mut self, f: &mut F) {
		for expression in &mut self.statements {
			expression.traverse_expressions_mut(f);
		}
		if let Some(ref mut expr) = self.output {
			expr.traverse_expressions_mut(f)
		};
	}
}

pub fn parse_block(stream: &mut TokenStream) -> Result<Block, Error<ParseError>> {
	let mut block = Block::new();
	
	while !stream.is_empty() {
		unimplemented!()
	}
	
	Ok(block)
}

/// Gets the next block from a token list, starting from an open brace ('{') and parsing everything
/// until the close brace, and returning everything after the last close brace as leftovers
pub struct BlockTaker;

impl ExpressionTaker for BlockTaker {
	type Args = ();

	fn next_expression<'a>(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		unimplemented!()
	}
}
