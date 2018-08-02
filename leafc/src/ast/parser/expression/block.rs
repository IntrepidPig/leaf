use ast::parser::*;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
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
		let (mut expr_tokenstream, output) = stream.split_when(|t| t.is_semicolon(), true)?;
		if output {
			if expr_tokenstream.is_empty() {
				break;
			}
			let expr = operation::parse_expression(&mut expr_tokenstream)?;
			block.output = Some(expr);
			break;
		} else {
			let expr = operation::parse_expression(&mut expr_tokenstream)?;
			block.statements.push(expr);
		}
	}

	Ok(block)
}

/// Gets the next block from a token list, starting from an open brace ('{') and parsing everything
/// until the close brace, and returning everything after the last close brace as leftovers
pub struct BlockTaker;

impl ExpressionTaker for BlockTaker {
	type Args = ();

	fn next_expression(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		Ok(
			if let TokenTree::Block(Bracket::Curly, ref mut block_token_stream, _, _) = stream.take_tokentree()? {
				Some(Expression::Block(Box::new(parse_block(
					block_token_stream,
				)?)))
			} else {
				None
			},
		)
	}
}
