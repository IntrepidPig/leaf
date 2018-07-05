
use ast::parser::*;

pub fn take_functiondef(in_tokens: &[TokenTree]) -> Result<Option<(Function, &[TokenTree])>, Error<ParseError>> {
	let mut tokens = in_tokens;
	
	match tokens.get(0) {
		Some(TokenTree::Token(Token::Keyword(Keyword::Function))) => {
			tokens = &tokens[1..];
		},
		_ => return Ok(None),
	}
	
	let name = match tokens.get(0) {
		Some(TokenTree::Token(Token::Name(ref name))) => {
			tokens = &tokens[1..];
			name.clone()
		},
		_ => return Err(ParseError::Other.into()) // needed a function name
	};
	
	let args = match tokens.get(0) {
		Some(TokenTree::Paren(ref args_tokens)) => {
			tokens = &tokens[1..];
			parse_args(args_tokens)?
		},
		_ => return Err(ParseError::Other.into()) // needed parens with arguments inside
	};
	
	let return_type = match tokens.get(0) {
		Some(TokenTree::Token(Token::Name(ref name))) => {
			tokens = &tokens[1..];
			Some(name.clone())
		},
		_ => None,
	};
	
	let block_taker = block::BlockTaker { };
	let (block, leftovers) = if let Some(block) = block_taker.take_expression(tokens, ())? {
		block
	} else {
		return Err(ParseError::Other.into()) // needed a block after the args or return type
	};
	
	let block = match block {
		Expression::Block(block) => *block,
		_ => return Err(ParseError::Other.into()) // Got an expression that wasn't a block
	};
	
	tokens = leftovers;
	
	Ok(Some((Function {
		name,
		args,
		return_type,
		body: block,
	}, tokens)))
}

fn parse_args(in_tokens: &[TokenTree]) -> Result<Vec<String>, Error<ParseError>> {
	let mut each_args_tokens = Vec::new();
	let mut last_end = 0;
	for (i, token) in in_tokens.iter().enumerate() {
		match token {
			TokenTree::Token(Token::Symbol(TokenSymbol::Comma)) => {
				each_args_tokens.push(&in_tokens[last_end..i]);
				last_end = i + 1;
			},
			_ => {},
		}
	}
	
	let mut args = Vec::new();
	
	for tokens in each_args_tokens {
		match tokens.get(0) {
			Some(TokenTree::Token(Token::Name(ref name))) => {
				args.push(name.clone())
			},
			_ => return Err(ParseError::Other.into()) // got an argument that wasn't a name
		}
	}
	
	Ok(args)
}