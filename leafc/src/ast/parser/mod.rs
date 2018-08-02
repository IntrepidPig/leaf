pub mod errors;
pub mod expression;
pub mod separated;
pub mod module;
pub mod functiondef;
pub mod typedef;
pub mod uses;
pub mod typename;
pub mod pathitem;
pub mod operators;
pub mod typeann;

pub use self::errors::*;
pub use self::expression::*;
pub use self::separated::*;
pub use self::module::*;
pub use self::functiondef::*;
pub use self::typedef::*;
pub use self::uses::*;
pub use self::typename::*;
pub use self::pathitem::*;
pub use self::operators::*;

pub use ast::tokenizer::*;
pub use ast::stream::*;
pub use ast::lexer::{Bracket, BracketState, Span};
pub use failure::Error;

pub fn parse(input: &mut TokenStream) -> Result<Module, Error<ParseError>> {
	parse_module(input)
}
