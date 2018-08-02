use ast::parser::*;

/// Gets the next break statement
pub struct FunctionCallTaker;

impl ExpressionTaker for FunctionCallTaker {
	type Args = ();

	fn next_expression(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		let name = if let Some(path_ident) = next_pathitem(stream, next_ident)? {
			path_ident
		} else {
			return Ok(None);
		};

		let args = if let Some(TokenTree::Block(Bracket::Paren, ref mut args_token_stream, _, _)) =
			stream.opt_next_tokentree()?
		{
			parse_args(args_token_stream)?
		} else {
			return Ok(None); // It could be an identifier or a type
		};

		Ok(Some(Expression::FunctionCall { name, args }))
	}
}

fn parse_args(stream: &mut TokenStream) -> Result<Vec<Expression>, Error<ParseError>> {
	let args_token_streams = separated::parse_separated(stream, |t| t.is_comma())?;
	let mut args: Vec<Expression> = Vec::new();
	for mut arg_token_stream in args_token_streams {
		args.push(operation::parse_expression(&mut arg_token_stream)?);
	}
	Ok(args)
}
