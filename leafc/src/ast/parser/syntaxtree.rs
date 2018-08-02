use ast::lexer::Span;
use ast::tokenizer::Symbol as TokenSymbol;
use ast::parser::*;
use ast::parser::operators::*;
use failure::Error;

pub use self::structures::*;
pub use self::errors::*;

/// Gets statements until it can't from the tokens, and then gets a final expression if there is one
/// TODO don't return option
pub fn parse_block(stream: &mut TokenStream) -> Result<Block, Error<ParseError>> {
	let mut block = Block::new();
	
	while !stream.is_empty() {
		let (expr, exhausted) = next_expression(stream, |token| token.is_semicolon(), false)?;
		
		
		if exhausted {
			block.output = Some(expr);
			break;
		} else {
			block.block.push(expr);
		}
	}
	
	if !stream.is_empty() {
		return Err(ParseError {
			kind: ParseErrorKind::UnexpectedToken(stream.next_token().unwrap().clone()),
			span: stream.current_span,
		}.into());
	}
	
	Ok(block)
}

pub fn next_expression<F: FnMut(TokenTree) -> bool>(stream: &mut TokenStream, end_predicate: F, exclusive: bool) -> Result<(Expression, bool), Error<ParseError>> {
	let (mut expr_stream, exhausted) = stream.until(end_predicate, exclusive)?;
	eprintln!("Expression tokens: {:?}\nexhausted: {}", expr_stream, exhausted);
	let item = if let Some(item) = operation::OperationTaker::new().take_expression(&mut expr_stream, ())? {
		stream.commit();
		item
	} else {
		return Err(ParseError {
			kind: ParseErrorKind::UnexpectedToken(expr_stream.next_token().unwrap().clone()),
			span: expr_stream.current_span,
		}.into());
	};
	if !expr_stream.is_empty() && exclusive {
		return Err(ParseError {
			kind: ParseErrorKind::UnexpectedToken(expr_stream.next_token().unwrap().clone()),
			span: expr_stream.current_span,
		}.into());
	}
	Ok((item, exhausted))
}
/*
/// Gets the next statement requiring a terminating semicolon
pub fn next_statement(stream: &mut TokenStream) -> ParseResult<Expression> {
	if let Some(expr) = next_expression(stream, Box::new(|token| token.is_semicolon()))? {
		if let Some(TokenTree::Token(Token {
			kind: TokenKind::Symbol(TokenSymbol::Semicolon),
			..
		})) = stream.opt_next_tokentree()?
		{
			Ok(Some(expr))
		} else {
			Ok(None)
		}
	} else {
		Ok(None)
	}
}

pub fn next_expression<'a>(
	stream: &mut TokenStream,
	end_predicate: Box<FnMut(TokenTree) -> bool>,
) -> ParseResult<Expression> {
	if let Some(expression) = operation::OperationTaker::new().take_expression(stream, end_predicate)? {
		return Ok(Some(expression));
	}

	Err(ParseError {
		kind: ParseErrorKind::UnexpectedToken(stream.next_token().unwrap().clone()),
		span: stream.current_span,
	}.into()) // Could not parse the next expression
}
*/