use super::*;

pub type ParseResult<T> = Result<Option<T>, Error<ParseError>>;

/// An error that occurs while parsing
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseErrorKind {
	UnexpectedToken(Token),
	Expected(Vec<Expected>),
	LoopWithOutput,
	UnexpectedEndOfInput,
}

/// An error that occurs while parsing
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
	pub kind: ParseErrorKind,
	pub span: Span,
}

impl ParseError {
	pub fn expected(expected: Vec<Expected>, stream: &TokenStream) -> Self {
		ParseError {
			kind: ParseErrorKind::Expected(expected),
			span: stream.current_span,
		}
	}

	pub fn unexpected(stream: &TokenStream) -> Self {
		ParseError {
			kind: ParseErrorKind::UnexpectedToken(stream.next_token().unwrap().clone()),
			span: stream.current_span,
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expected {
	Identifier,
	Semicolon,
	Colon,
	Typename,
	Block,
	Brackets,
	Parentheses,
	Keyword(Keyword),
	Symbol(Symbol),
	Expression,
	Operator,
	ModulePath,
}

impl ::std::fmt::Display for ParseError {
	fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
		write!(
			f,
			"{}\n{}",
			match self.kind {
				ParseErrorKind::UnexpectedToken(ref token) => format!("Unexpected token: {:?}", token), // TODO not debug of token, display
				ParseErrorKind::Expected(ref expected) => {
					let mut items_str = "Expected one of: ".to_owned();
					if expected.len() > 1 {
						for (i, item) in expected.iter().enumerate() {
							items_str.push_str(&item.to_string());
							if i < expected.len() - 2 {
								items_str.push_str(", ");
							} else if i == expected.len() - 2 {
								items_str.push_str(", or ");
							}
						}
					} else {
						items_str = format!("Expected: {}", &expected[0].to_string());
					}
					items_str
				},
				ParseErrorKind::LoopWithOutput => "Loop blocks cannot have an output".to_owned(),
				ParseErrorKind::UnexpectedEndOfInput => "Unexpected end of input".to_owned(),
			},
			&self.span
		)
	}
}

impl ::std::fmt::Display for Expected {
	fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
		write!(
			f,
			"{}",
			match *self {
				Expected::Identifier => "identifier".to_owned(),
				Expected::Semicolon => "semicolon".to_owned(),
				Expected::Colon => "colon".to_owned(),
				Expected::Typename => "type name".to_owned(),
				Expected::Block => "block".to_owned(),
				Expected::Brackets => "brackets".to_owned(),
				Expected::Parentheses => "parentheses".to_owned(),
				Expected::Keyword(keyword) => keyword.to_string(),
				Expected::Symbol(symbol) => symbol.to_string(),
				Expected::Expression => "expression".to_owned(),
				Expected::Operator => "operator".to_owned(),
				Expected::ModulePath => "module path".to_owned(),
			}
		)
	}
}

impl ::std::error::Error for ParseError {}
