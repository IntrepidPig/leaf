use ast::parser::*;

/// Gets the next block from a token list, starting from an open brace ('{') and parsing everything
/// until the close brace, and returning everything after the last close brace as leftovers
pub struct BlockTaker;

impl ExpressionTaker for BlockTaker {
	type Args = ();

	fn take_expression<'a>(&self, in_tokens: &'a [TokenTree], _args: Self::Args) -> ParseResult<'a, Expression> {
		if in_tokens.is_empty() {
			return Ok(None);
		}
		match in_tokens[0] {
			TokenTree::Block(BlockType::Brace, ref tokens, _, _) => {
				let ast = parse_block(tokens)?;
				Ok(Some((Expression::Block(Box::new(ast)), &in_tokens[1..])))
			},
			_ => Ok(None),
		}
	}
}
