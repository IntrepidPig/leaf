
/// A list of lexemes
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lexemes {
	lexemes: Vec<Lexeme>
}

impl Lexemes {
	pub fn new(lexemes: Vec<Lexeme>) -> Self {
		Lexemes {
			lexemes
		}
	}

	/// Convert this Lexemes instance into it's would-be string representation
	pub fn de_lex(&self) -> String {
		let mut out = String::new();
		for lexeme in &self.lexemes {
			match *lexeme {
				Lexeme::Word(ref word) => {
					out.push_str(word);
				},
				Lexeme::Bracket(bracket, state) => {
					out.push_str(bracket.de_lex(state));
				},
				Lexeme::String(ref string) => {
					out.push('"');
					for c in string.chars() {
						let c = match c {
							'"' => "\\\"",
							'\\' => "\\\\",
							'\n' => "\\n",
							'\t' => "\\t",
							c => {
								out.push(c);
								continue
							}
						};
						out.push_str(c);
					}
					out.push('"');
				},
				Lexeme::Number(ref num_string) => {
					out.push_str(num_string)
				},
				Lexeme::Colon => out.push(':'),
				Lexeme::Comma => out.push(','),
				Lexeme::Dot => out.push('.'),
				Lexeme::Equals => out.push('='),
				Lexeme::Semicolon => out.push(';'),
				Lexeme::Asterisk => out.push('*'),
				Lexeme::Greater => out.push('>'),
				Lexeme::Less => out.push('<'),
				Lexeme::Ampersand => out.push('&'),
				Lexeme::Question => out.push('?'),
				Lexeme::Exclamation => out.push('!'),
				Lexeme::Whitespace { whitespace_type, amount } => {
					for _ in 0..amount {
						out.push(whitespace_type.to_char())
					}
				}
			}
		}

		out
	}
}

/// A lexeme is a category of characters and the characters involved. It involves symbols, words, literals, and whitespace.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lexeme {
	Word(String),
	Bracket(Bracket, BracketState),
	String(String),
	Number(String),
	Colon,
	Comma,
	Dot,
	Equals,
	Semicolon,
	Asterisk,
	Greater,
	Less,
	Ampersand,
	Question,
	Exclamation,
	Whitespace {
		whitespace_type: WhitespaceType,
		amount: u32,
	},
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WhitespaceType {
	Newline,
	Space,
	Tab,
}

impl WhitespaceType {
	pub fn from_char(c: char) -> Option<Self> {
		Some(match c {
			' ' => WhitespaceType::Space,
			'\n' => WhitespaceType::Newline,
			'\t' => WhitespaceType::Tab,
			_ => return None,
		})
	}

	pub fn to_char(self) -> char {
		match self {
			WhitespaceType::Newline => '\n',
			WhitespaceType::Space => ' ',
			WhitespaceType::Tab => '\t',
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Bracket {
	Paren,
	Curly,
	Square
}

impl Bracket {
	pub fn de_lex(self, state: BracketState) -> &'static str {
		match state {
			BracketState::Open => match self {
				Bracket::Curly => "{",
				Bracket::Paren => "(",
				Bracket::Square => "[",
			},
			BracketState::Close => match self {
				Bracket::Curly => "}",
				Bracket::Paren => ")",
				Bracket::Square => "]",
			}
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BracketState {
	Open,
	Close,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexError {
	Unexpected(usize),
	UnknownEscape(char),
	UnterminatedStringLiteral(usize),
	Empty,
	Other,
}

/// Return a Vec<Lexeme> from an input string.
pub fn lex(old_input: &str) -> Result<Lexemes, LexError> {
	let mut input = old_input;
	
	let mut lexemes = Vec::new();
	let lexeme_takers: &[&LexemeTaker] = &[&WhitespaceTaker, &BracketTaker, &NumLiteralTaker, &SymbolTaker, &WordTaker, &StringTaker];
	
	'outer: while !input.is_empty() {
		//lexeme_takers = next_lexeme_takers(&lexemes);
		for lexeme_taker in lexeme_takers {
			let (lexeme_opt, remaining) = lexeme_taker.next_lexeme(input, &lexemes)?;
			input = remaining;
			if let Some(lexeme) = lexeme_opt {
				println!("Got lexeme {:?}", lexeme);
				lexemes.push(lexeme);
				continue 'outer;
			}
		}

		
		
		return Err(LexError::Unexpected(old_input.len() - input.len()))
	}
	
	Ok(Lexemes::new(lexemes))
}

pub trait LexemeTaker: ::std::fmt::Debug {
	fn next_lexeme<'a>(&self, input: &'a str, current: &[Lexeme]) -> Result<(Option<Lexeme>, &'a str), LexError>;
}

#[derive(Debug)]
struct WhitespaceTaker;
impl LexemeTaker for WhitespaceTaker {
	fn next_lexeme<'a>(&self, input: &'a str, _current: &[Lexeme]) -> Result<(Option<Lexeme>, &'a str), LexError> {
		let mut whitespace_type = None;
		let mut end = 0;
		
		for c in input.chars() {
			if let Some(new_whitespace_type) = WhitespaceType::from_char(c) {
				if let Some(old_whitespace_type) = whitespace_type.clone() {
					if new_whitespace_type == old_whitespace_type {
						end += c.len_utf8();
					} else {
						break;
					}
				} else {
					whitespace_type = Some(new_whitespace_type);
					end += c.len_utf8();
				}
			} else {
				break;
			}
		}
		
		if end > 0 {
			Ok((Some(Lexeme::Whitespace {
				whitespace_type: whitespace_type.unwrap(),
				amount: input[0..end].chars().count() as u32,
			}), &input[end..input.len()]))
		} else {
			Ok((None, input))
		}
	}
}

#[derive(Debug)]
struct WordTaker;
impl LexemeTaker for WordTaker {
	fn next_lexeme<'a>(&self, input: &'a str, _current: &[Lexeme]) -> Result<(Option<Lexeme>, &'a str), LexError> {
		let mut end = 0;
		
		for c in input.chars() {
			if c.is_ascii_alphabetic() || c == '_' {
				end += c.len_utf8();
			} else if c.is_ascii_alphanumeric() && end > 0 {
				end += c.len_utf8();
			} else {
				break;
			}
		}
		
		if end > 0 {
			Ok((Some(Lexeme::Word(input[0..end].to_owned())), &input[end..input.len()]))
		} else {
			Ok((None, input))
		}
	}
}

/// Returns a bracket if the next character in the input is a bracket
#[derive(Debug)]
struct BracketTaker;
impl LexemeTaker for BracketTaker {
	fn next_lexeme<'a>(&self, input: &'a str, _current: &[Lexeme]) -> Result<(Option<Lexeme>, &'a str), LexError> {
		let c = if let Some(c) = input.chars().next() {
			c
		} else {
			return Ok((None, input))
		};
		
		Ok((Some(match c {
			'{' => Lexeme::Bracket(Bracket::Curly, BracketState::Open),
			'[' => Lexeme::Bracket(Bracket::Square, BracketState::Open),
			'(' => Lexeme::Bracket(Bracket::Paren, BracketState::Open),
			'}' => Lexeme::Bracket(Bracket::Curly, BracketState::Close),
			']' => Lexeme::Bracket(Bracket::Square, BracketState::Close),
			')' => Lexeme::Bracket(Bracket::Paren, BracketState::Close),
			_ => return Ok((None, input))
		}), &input[c.len_utf8()..input.len()]))
	}
}

/// If the next characters in the input successfully parse as a string literl, it returns a string literal.
// TODO: r##"off \n #"nice""## syntax should parse into 'off \n #"nice"'
#[derive(Debug)]
struct StringTaker;
impl LexemeTaker for StringTaker {
	fn next_lexeme<'a>(&self, input: &'a str, _current: &[Lexeme]) -> Result<(Option<Lexeme>, &'a str), LexError> {
		let chars = input.chars();
		// start of string after quote marks marks
		let mut start = 0;
		
		// set the start variable to the index after the first quote mark
		for c in chars {
			if c == '"' {
				start += c.len_utf8();
				break;
			} else {
				// If it wasn't a quote mark this isn't a string literal
				return Ok((None, input))
			}
		}
		
		// now start parsing from after the first quote
		let input = &input[start..input.len()];
		// chars iter afterf first quote mark
		let chars = input.chars();
		// end of string literal before final quote mark
		let mut finish = 0;
		// end of string literal after final quote mark
		let mut end = 0;
		// the string literal contents
		let mut string = String::new();
		
		// true if loop finished after finding final quote
		let mut found_end = false;
		// true if the previous character was a backslash (used for escaping)
		let mut backslash = false;
		// Read each char and add the escaped version to `string` and stop when the final quote is reached
		for c in chars {
			// if this is after a backslash
			if backslash {
				// no longer escaping escaping
				backslash = false;
				match c {
					'"' => string.push('"'), // non-terminating quotes
					'n' => string.push('\n'), // newlines
					't' => string.push('\t'), // tab characters
					'\\' => string.push('\\'), // actual backslashes
					c => return Err(LexError::UnknownEscape(c)) // fail if an unknown character was escaped
				}
			} else {
				// if its a quote the string is over
				if c == '"' {
					// set the end to be after the quote mark
					end = finish + c.len_utf8();
					found_end = true;
					break;
				} else {
					// if we got a backslash we start escaping
					if c == '\\' {
						backslash = true;
					} else {
						// otherwise it's any old character
						string.push(c);
					}
				}
			}
			finish += c.len_utf8();
		}
		
		if found_end {
			Ok((Some(Lexeme::String(string)), &input[end..input.len()]))
		} else {
			Err(LexError::UnterminatedStringLiteral(0)) // TODO return actual position
		}
	}
}

#[derive(Debug)]
struct SymbolTaker;
impl LexemeTaker for SymbolTaker {
	fn next_lexeme<'a>(&self, input: &'a str, _current: &[Lexeme]) -> Result<(Option<Lexeme>, &'a str), LexError> {
		let mut end = 0;

		for c in input.chars() {
			end += c.len_utf8();
			return Ok((Some(match c {
				':' => Lexeme::Colon,
				',' => Lexeme::Comma,
				'.' => Lexeme::Dot,
				'=' => Lexeme::Equals,
				';' => Lexeme::Semicolon,
				'*' => Lexeme::Asterisk,
				'>' => Lexeme::Greater,
				'<' => Lexeme::Less,
				'&' => Lexeme::Ampersand,
				'?' => Lexeme::Question,
				'!' => Lexeme::Exclamation,
				_ => return Ok((None, input))
			}), &input[end..input.len()]))
		}

		Ok((None, input))
	}
}

#[derive(Debug)]
struct NumLiteralTaker;
impl LexemeTaker for NumLiteralTaker {
	fn next_lexeme<'a>(&self, input: &'a str, _current: &[Lexeme]) -> Result<(Option<Lexeme>, &'a str), LexError> {
		let mut end = 0;

		for c in input.chars() {
			if c.is_numeric() {
				end += c.len_utf8();
			} else {
				break;
			}
		}

		if end > 0 {
			let num_string = input[0..end].to_owned();
			Ok((Some(Lexeme::Number(num_string)), &input[end..input.len()]))
		} else {
			Ok((None, input))
		}
	}
}