use ast::tokenizer::{Keyword, Symbol, Token, TokenKind};
use ast::lexer::{Bracket, BracketState, Location, Span};
use ast::parser::errors::{ParseError, ParseErrorKind};
use failure::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenStream<'a> {
	all_tokens: &'a [Token],
	position: usize,
	pub last_span: Span,
	pub current_span: Span,
}

impl<'a> TokenStream<'a> {
	pub fn new(tokens: &'a [Token]) -> Self {
		TokenStream {
			all_tokens: tokens,
			position: 0,
			last_span: Span {
				start: Location { line: 1, col: 0 },
				end: Location { line: 1, col: 0 },
			},
			current_span: Span {
				start: Location { line: 1, col: 0 },
				end: Location { line: 1, col: 0 },
			},
		}
	}

	pub fn get_leftovers(&self) -> &'a [Token] {
		&self.all_tokens[self.position..]
	}

	pub fn duplicate(&self) -> TokenStream<'a> {
		TokenStream {
			all_tokens: self.all_tokens,
			position: self.position,
			last_span: self.last_span,
			current_span: self.current_span,
		}
	}

	pub fn new_with(other: &TokenStream<'a>) -> TokenStream<'a> {
		TokenStream {
			all_tokens: other.get_leftovers(),
			position: 0,
			last_span: other.last_span,
			current_span: other.current_span,
		}
	}

	pub fn merge(&mut self, other: &TokenStream<'a>) {
		self.position += other.position;
	}

	/// Take the next tokentree from the stream, moving the position up. Returns an error if there was no token
	pub fn take_tokentree(&mut self) -> Result<TokenTree<'a>, Error<ParseError>> {
		if let Some(tokentree) = self.opt_next_tokentree()? {
			Ok(tokentree)
		} else {
			Err(ParseError {
				kind: ParseErrorKind::UnexpectedEndOfInput,
				span: self.current_span,
			}.into())
		}
	}

	/// Get the next tokentree, or None if the TokenStream is empty
	pub fn opt_next_tokentree(&mut self) -> Result<Option<TokenTree<'a>>, Error<ParseError>> {
		let token = if !self.is_empty() {
			self.take_token()
		} else {
			return Ok(None);
		};
		let tokentree = match token {
			Token {
				kind: TokenKind::Bracket(bracket, BracketState::Open),
				..
			} => {
				let mut outer_span = Span {
					start: token.span.start,
					end: token.span.end,
				};
				let mut inner_span = Span {
					start: token.span.end,
					end: token.span.end,
				};
				let mut count: i32 = 0;
				// inner
				let start = self.position;
				// inner
				let mut end = self.position;
				while let Some(test_token) = self.next_token() {
					if let Token {
						kind: TokenKind::Bracket(test_bracket, test_bracket_state),
						..
					} = test_token
					{
						if bracket == test_bracket {
							match test_bracket_state {
								BracketState::Open => count += 1,
								BracketState::Close => count -= 1,
							}
						}
					}

					outer_span.end = test_token.span.end;
					inner_span.end = test_token.span.start;

					self.take_token();

					if count < 0 {
						break;
					}

					end += 1;
				}

				if count >= 0 {
					return Err(ParseError {
						kind: ParseErrorKind::UnexpectedEndOfInput,
						span: outer_span,
					}.into());
				}

				let sub_tokens = &self.all_tokens[start..end];

				TokenTree::Block(
					*bracket,
					TokenStream::new(sub_tokens),
					outer_span,
					inner_span,
				)
			},
			t => TokenTree::Token(t),
		};
		Ok(Some(tokentree))
	}

	pub fn split_when<F: FnMut(TokenTree) -> bool>(
		&mut self,
		mut pred: F,
		exclusive: bool,
	) -> Result<(TokenStream<'a>, bool), Error<ParseError>> {
		let start_pos = self.position;
		let (split_pos, exhausted) = loop {
			let old_position = self.position;
			if let Some(tokentree) = self.opt_next_tokentree()? {
				if pred(tokentree) {
					self.position = old_position;
					break (self.position, false);
				}
			} else {
				break (self.position, true);
			};
		};

		Ok((
			if exhausted {
				TokenStream::new(&self.all_tokens[start_pos..split_pos])
			} else {
				if exclusive {
					self.take_tokentree()?;
				}
				TokenStream::new(&self.all_tokens[start_pos..split_pos])
			},
			exhausted,
		))
	}

	pub fn split_here(&mut self, exclusive: bool) -> Result<(TokenStream<'a>, TokenStream<'a>), Error<ParseError>> {
		let split_pos = self.get_position();
		if exclusive {
			self.opt_next_tokentree()?;
		}
		Ok((
			TokenStream::new(&self.all_tokens[..split_pos]),
			TokenStream::new(&self.all_tokens[self.position..]),
		))
	}

	/// Peek the next token
	pub fn next_token(&self) -> Option<&'a Token> {
		self.get_leftovers().get(0)
	}

	/// Take the next token, moving the positition up one
	/// Panics if there is none
	pub fn take_token(&mut self) -> &'a Token {
		let token = &self.get_leftovers()[0];
		self.position += 1;
		token
	}

	/// Peek the next `amt` of tokens
	pub fn peek_tokens(&self, amt: usize) -> &'a [Token] {
		&self.get_leftovers()[..::std::cmp::min(amt, self.left())]
	}

	/// Check if there are any more tokens left in this token stream
	pub fn is_empty(&self) -> bool {
		self.position >= self.all_tokens.len()
	}

	/// Set the position to 0
	pub fn reset(&mut self) {
		self.position = 0;
	}

	/// Cut off the beginning of the main token list to the current position and set the current position to 0
	pub fn commit(&mut self) {
		self.all_tokens = self.get_leftovers();
		self.position = 0;
	}

	/// The amount of tokens left
	pub fn left(&self) -> usize {
		self.all_tokens.len() - self.position
	}

	pub fn get_position(&self) -> usize {
		self.position
	}

	pub fn seek(&mut self, position: usize) {
		self.position = position;
	}
}

impl<'a> Iterator for TokenStream<'a> {
	type Item = Result<TokenTree<'a>, Error<ParseError>>;

	fn next(&mut self) -> Option<Self::Item> {
		unimplemented!()
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenTree<'a> {
	Token(&'a Token),
	/// Type of block, tokens in block, outer span, inner span
	Block(Bracket, TokenStream<'a>, Span, Span),
}

impl<'a> TokenTree<'a> {
	pub fn is_semicolon(&self) -> bool {
		match self {
			TokenTree::Token(Token {
				kind: TokenKind::Symbol(Symbol::Semicolon),
				..
			}) => true,
			_ => false,
		}
	}

	pub fn is_then(&self) -> bool {
		match self {
			TokenTree::Token(Token {
				kind: TokenKind::Keyword(Keyword::Then),
				..
			}) => true,
			_ => false,
		}
	}

	pub fn is_else(&self) -> bool {
		match self {
			TokenTree::Token(Token {
				kind: TokenKind::Keyword(Keyword::Else),
				..
			}) => true,
			_ => false,
		}
	}

	pub fn is_end(&self) -> bool {
		match self {
			TokenTree::Token(Token {
				kind: TokenKind::Keyword(Keyword::End),
				..
			}) => true,
			_ => false,
		}
	}

	pub fn is_brace_expr(&self) -> bool {
		match self {
			TokenTree::Block(Bracket::Curly, _, _, _) => true,
			_ => false,
		}
	}

	pub fn is_comma(&self) -> bool {
		match self {
			TokenTree::Token(Token {
				kind: TokenKind::Symbol(Symbol::Comma),
				..
			}) => true,
			_ => false,
		}
	}

	pub fn get_location(&self) -> Location {
		match self {
			TokenTree::Token(token) => token.span.start,
			TokenTree::Block(_, _, outer, _inner) => outer.start,
		}
	}

	pub fn get_span(&self) -> Span {
		match self {
			TokenTree::Token(token) => token.span,
			TokenTree::Block(_, _, outer, _inner) => *outer,
		}
	}
}

pub trait Locate {
	fn get_location(&self) -> Location;
}

impl Locate for Token {
	fn get_location(&self) -> Location {
		self.span.start
	}
}

impl<'a> Locate for TokenTree<'a> {
	fn get_location(&self) -> Location {
		self.get_location()
	}
}

pub trait Spanned {
	fn get_span(&self) -> Span;
}

impl Spanned for Token {
	fn get_span(&self) -> Span {
		self.span
	}
}

impl<'a> Spanned for TokenTree<'a> {
	fn get_span(&self) -> Span {
		self.get_span()
	}
}
