use std::fmt;

// TODO include index information with each lexeme

/// A list of lexemes
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lexemes<'a> {
	pub lexemes: Vec<Lexeme<'a>>,
}

impl<'a> Lexemes<'a> {
	pub fn new(lexemes: Vec<Lexeme<'a>>) -> Self {
		Lexemes { lexemes }
	}

	/// Convert this Lexemes instance into it's would-be string representation
	pub fn de_lex(&self) -> String {
		let mut out = String::new();
		for lexeme in &self.lexemes {
			match *lexeme {
				Lexeme::Word(word) => {
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
								continue;
							},
						};
						out.push_str(c);
					}
					out.push('"');
				},
				Lexeme::Number(num_string) => out.push_str(num_string),
				Lexeme::Colon => out.push(':'),
				Lexeme::Comma => out.push(','),
				Lexeme::Dot => out.push('.'),
				Lexeme::Equals => out.push('='),
				Lexeme::Semicolon => out.push(';'),
				Lexeme::Asterisk => out.push('*'),
				Lexeme::Greater => out.push('>'),
				Lexeme::Less => out.push('<'),
				Lexeme::Assign => out.push_str(":="),
				Lexeme::Equality => out.push_str("=="),
				Lexeme::Ampersand => out.push('&'),
				Lexeme::Question => out.push('?'),
				Lexeme::Exclamation => out.push('!'),
				Lexeme::Plus => out.push('+'),
				Lexeme::Minus => out.push('-'),
				Lexeme::Slash => out.push('/'),
				Lexeme::Namespace => out.push_str("::"),
				Lexeme::Whitespace {
					whitespace_type,
					amount,
				} => for _ in 0..amount {
					out.push(whitespace_type.to_char())
				},
			}
		}

		out
	}
}

/// A lexeme is a category of characters and the characters involved. It involves symbols, words, literals, and whitespace.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lexeme<'a> {
	Word(&'a str),
	Bracket(Bracket, BracketState),
	String(String),
	Number(&'a str),
	Colon,
	Comma,
	Dot,
	Equals,
	Semicolon,
	Asterisk,
	Greater,
	Less,
	Assign,
	Equality,
	Ampersand,
	Question,
	Exclamation,
	Plus,
	Minus,
	Slash,
	Namespace,
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
	Square,
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
			},
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BracketState {
	Open,
	Close,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
	line: u32,
	col: u32,
}

impl fmt::Display for Location {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "At line {} column {}", self.line, self.col)
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError {
	kind: LexErrorKind,
	location: Location,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexErrorKind {
	Unexpected,
	UnknownEscape(char),
	UnterminatedStringLiteral,
	Empty,
	Other,
}

impl fmt::Display for LexError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(
			f,
			"{}\n{}",
			match self.kind {
				LexErrorKind::Unexpected => "Unexpected character".to_owned(),
				LexErrorKind::UnknownEscape(c) => format!("Unknown escape character '{}'", c),
				LexErrorKind::UnterminatedStringLiteral => "Unterminated string literal".to_owned(),
				LexErrorKind::Empty => "Unexpected end of input".to_owned(),
				LexErrorKind::Other => "Unknown error occurred while lexing".to_owned(),
			},
			&self.location,
		)
	}
}

impl ::std::error::Error for LexError {}

/// Return a Vec<Lexeme> from an input string.
pub fn lex(old_input: &str) -> Result<Lexemes, LexError> {
	let mut input = old_input;

	let mut lexemes = Vec::new();
	// Order of lexemes to try to parse the next as
	let lexeme_takers: &[&LexemeTaker] = &[
		&WhitespaceTaker,
		&BracketTaker,
		&NumLiteralTaker,
		&SymbolTaker,
		&WordTaker,
		&StringTaker,
	];
	
	let mut location = Location {
		line: 1,
		col: 0,
	};

	// Keep cutting down the input string slice with lexeme takers until it's empty
	'outer: while !input.is_empty() {
		for lexeme_taker in lexeme_takers {
			if let Some((lexeme, remaining)) = lexeme_taker.next_lexeme(input).map_err(|e| LexError { kind: e, location })? {
				if let Lexeme::Whitespace { whitespace_type, amount } = lexeme {
					if whitespace_type == WhitespaceType::Newline {
						location.line += amount;
						location.col = 0;
					}
				} else {
					location.col += (input.len() - remaining.len()) as u32;
				}
				input = remaining;
				lexemes.push(lexeme);
				continue 'outer;
			}
		}

		// If none of the lexeme takers succeeded, then something was wrong with the input
		return Err(LexError {
			kind: LexErrorKind::Unexpected,
			location,
		});
	}

	Ok(Lexemes::new(lexemes))
}

pub trait LexemeTaker: ::std::fmt::Debug {
	fn next_lexeme<'a>(&self, input: &'a str) -> Result<Option<(Lexeme<'a>, &'a str)>, LexErrorKind>;
}

/// Takes next available whitespace
/// Returns a stuct enum variant that contains the type of whitespace (tab, newline, or space) and how many occurrences of it
#[derive(Debug)]
struct WhitespaceTaker;
impl LexemeTaker for WhitespaceTaker {
	fn next_lexeme<'a>(&self, input: &'a str) -> Result<Option<(Lexeme<'a>, &'a str)>, LexErrorKind> {
		// The whitespace type we are getting (tab | newline | space)
		let mut whitespace_type = None;
		// The end of the whitespace
		let mut end = 0;

		for c in input.chars() {
			// If this char is whitespace
			if let Some(new_whitespace_type) = WhitespaceType::from_char(c) {
				// If we already have a type of whitespace
				if let Some(old_whitespace_type) = whitespace_type {
					// If they're the same type of whitespce
					if new_whitespace_type == old_whitespace_type {
						end += c.len_utf8();
					} else {
						// We found all the whitespace of this type
						break;
					}
				} else {
					// Otherwise this is the type of whitespace we will find from now on
					whitespace_type = Some(new_whitespace_type);
					end += c.len_utf8();
				}
			} else {
				// We found all the whitespace available (possibly 0)
				break;
			}
		}

		// If we have a nonzero whitespace amount
		if end > 0 {
			Ok(Some((
				Lexeme::Whitespace {
					whitespace_type: whitespace_type.unwrap(), // If the end is nonzero then the whitespace_type was set
					amount: input[0..end].chars().count() as u32, // Count the chars in the substring to get the amount
				},
				&input[end..],
			)))
		} else {
			// No whitespace was found
			Ok(None)
		}
	}
}

#[derive(Debug)]
struct WordTaker;
impl LexemeTaker for WordTaker {
	fn next_lexeme<'a>(&self, input: &'a str) -> Result<Option<(Lexeme<'a>, &'a str)>, LexErrorKind> {
		// The end of the word
		let mut end = 0;

		for c in input.chars() {
			if c.is_ascii_alphabetic() || c == '_' || (c.is_ascii_alphanumeric() && end > 0) {
				// Letters and underscores are allowed in words
				// Numbers are allowed so long as they are not the first character
				end += c.len_utf8();
			} else {
				// The word has ended
				break;
			}
		}

		if end > 0 {
			Ok(Some((Lexeme::Word(&input[0..end]), &input[end..])))
		} else {
			Ok(None)
		}
	}
}

/// Returns a bracket if the next character in the input is a bracket
#[derive(Debug)]
struct BracketTaker;
impl LexemeTaker for BracketTaker {
	fn next_lexeme<'a>(&self, input: &'a str) -> Result<Option<(Lexeme<'a>, &'a str)>, LexErrorKind> {
		// Make sure there is another char available (always should be, except lexemetakers can modify the input without returning a token TODO)
		let c = if let Some(c) = input.chars().next() {
			c
		} else {
			return Err(LexErrorKind::Empty);
		};

		Ok(Some((
			match c {
				'{' => Lexeme::Bracket(Bracket::Curly, BracketState::Open),
				'[' => Lexeme::Bracket(Bracket::Square, BracketState::Open),
				'(' => Lexeme::Bracket(Bracket::Paren, BracketState::Open),
				'}' => Lexeme::Bracket(Bracket::Curly, BracketState::Close),
				']' => Lexeme::Bracket(Bracket::Square, BracketState::Close),
				')' => Lexeme::Bracket(Bracket::Paren, BracketState::Close),
				_ => return Ok(None),
			},
			&input[c.len_utf8()..],
		)))
	}
}

/// If the next characters in the input successfully parse as a string literl, it returns a string literal.
// TODO: r##"off \n #"nice""## syntax should parse into 'off \n #"nice"'
#[derive(Debug)]
struct StringTaker;
impl LexemeTaker for StringTaker {
	fn next_lexeme<'a>(&self, input: &'a str) -> Result<Option<(Lexeme<'a>, &'a str)>, LexErrorKind> {
		let mut chars = input.chars();
		// start of string after quote marks marks
		let mut start = 0;

		// set the start variable to the index after the first quote mark
		match chars.next() {
			Some('"') => {
				start += '"'.len_utf8();
			},
			// If it wasn't a quote mark this isn't a string literal
			_ => return Ok(None),
		}

		// now start parsing from after the first quote
		let input = &input[start..];
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
					'"' => string.push('"'),                     // non-terminating quotes
					'n' => string.push('\n'),                    // newlines
					't' => string.push('\t'),                    // tab characters
					'\\' => string.push('\\'),                   // actual backslashes
					c => return Err(LexErrorKind::UnknownEscape(c)), // fail if an unknown character was escaped
				}
			// if its a quote the string is over
			} else if c == '"' {
				// set the end to be after the quote mark
				end = finish + c.len_utf8();
				found_end = true;
				break;
			// if we got a backslash we start escaping
			} else if c == '\\' {
				backslash = true;
			} else {
				// otherwise it's any old character
				string.push(c);
			}
			finish += c.len_utf8();
		}

		if found_end {
			Ok(Some((Lexeme::String(string), &input[end..])))
		} else {
			Err(LexErrorKind::UnterminatedStringLiteral)
		}
	}
}

/// Get next symbols available from the input &str
/// Currently gets:
/// - ':' Colon
/// - ',' Comma
/// - '.' Dot
/// - '=' Equals
/// - ';' Semicolon
/// - '*' Asterisk
/// - '>' Greater
/// - '<' Less
/// - '&' Ampersand
/// - '?' Question
/// - '!' Exclamation
/// - ':=' Assign,
/// - '==' Equality
#[derive(Debug)]
struct SymbolTaker;
impl LexemeTaker for SymbolTaker {
	fn next_lexeme<'a>(&self, input: &'a str) -> Result<Option<(Lexeme<'a>, &'a str)>, LexErrorKind> {
		// Make sure there's at least one character
		let mut chars = input.chars();
		let c1 = if let Some(c1) = chars.next() {
			c1
		} else {
			return Err(LexErrorKind::Empty);
		};

		// Optionally get the second
		let c2 = chars.next();

		// The length of the second char to add to the amount taken away from the input slice
		let mut char2_len = 0;

		// If there's a second char, set the char2_len
		Ok(Some((
			match (c1, c2) {
				(':', Some('=')) => {
					char2_len = c2.unwrap().len_utf8();
					Lexeme::Assign
				},
				('=', Some('=')) => {
					char2_len = c2.unwrap().len_utf8();
					Lexeme::Equality
				},
				(':', Some(':')) => {
					char2_len = c2.unwrap().len_utf8();
					Lexeme::Namespace
				},
				(':', _) => Lexeme::Colon,
				(',', _) => Lexeme::Comma,
				('.', _) => Lexeme::Dot,
				('=', _) => Lexeme::Equals,
				(';', _) => Lexeme::Semicolon,
				('*', _) => Lexeme::Asterisk,
				('>', _) => Lexeme::Greater,
				('<', _) => Lexeme::Less,
				('&', _) => Lexeme::Ampersand,
				('?', _) => Lexeme::Question,
				('!', _) => Lexeme::Exclamation,
				('+', _) => Lexeme::Plus,
				('-', _) => Lexeme::Minus,
				('/', _) => Lexeme::Slash,
				(_, _) => return Ok(None),
			},
			&input[c1.len_utf8() + char2_len..],
		)))
	}
}

/// Gets the next numeric literal from the input
/// Currently only supports decimal integers
#[derive(Debug)]
struct NumLiteralTaker;
impl LexemeTaker for NumLiteralTaker {
	fn next_lexeme<'a>(&self, input: &'a str) -> Result<Option<(Lexeme<'a>, &'a str)>, LexErrorKind> {
		let mut end = 0;

		for c in input.chars() {
			if c.is_numeric() {
				end += c.len_utf8();
			} else {
				break;
			}
		}

		if end > 0 {
			let num_str = &input[0..end];
			Ok(Some((Lexeme::Number(num_str), &input[end..])))
		} else {
			Ok(None)
		}
	}
}
