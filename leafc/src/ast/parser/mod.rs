/*pub mod syntaxtree;
pub mod operators;
pub mod debug;
pub mod breakexpr;
pub mod block;
pub mod binding;
pub mod ifexpr;
pub mod loopexpr;
pub mod operation;
pub mod literal;
pub mod identifier;
pub mod functiondef;
pub mod typedef;
pub mod functioncall;
pub mod separated;
pub mod instantiation;
pub mod pathitem;
pub mod uses;
pub mod module;
pub mod errors;
pub mod stream;


pub use ast::tokenizer::{Token, TokenKind, Keyword, Symbol as TokenSymbol};
pub use ast::lexer::{Location, Span, Bracket, BracketState};
pub use self::syntaxtree::*;
pub use self::errors::*;
pub use self::operators::*;
pub use self::stream::*;
//pub use ast::treeify::*;
pub use self::pathitem::*;
pub use failure::Error;


/// Parse a block from the tokens (will use all of the tokens or error)
pub fn parse(stream: &mut TokenStream) -> Result<SyntaxTree, Error<ParseError>> {
	let mut stree = SyntaxTree::new(Vec::new(), Vec::new(), Vec::new(), Vec::new(), Vec::new());

	while !stream.is_empty() {
		stream.commit();
		if let Some(func) = functiondef::take_functiondef(stream)? {
			stree.functions.push(func);
			continue;
		} else {
			stream.reset();
		}
		
		if let Some(typedef) = typedef::take_typedef(stream)? {
			stree.types.push(typedef);
			continue;
		} else {
			stream.reset();
		}
		
		if let Some(u) = uses::take_use(stream)? {
			stree.uses.push(u);
			continue;
		} else {
			stream.reset();
		}
		
		if let Some(module) = module::take_module(stream)? {
			stree.modules.push(module);
			continue;
		} else {
			stream.reset();
		}
		
		if let Some(extern_fn) = functiondef::take_externfn(stream)? {
			stree.extern_fns.push(extern_fn);
			continue;
		} else {
			stream.reset();
		}

		return Err(ParseError {
			kind: ParseErrorKind::UnexpectedToken(stream.next_token().unwrap().clone()),
			span: stream.get_leftovers()[0].get_span(),
		}.into()); // Didn't get a function or a typedef in the root
	}

	Ok(stree)
}
*/

pub mod errors;
pub mod block;
pub mod module;
pub mod identifier;
pub mod ifexpr;
pub mod expression;
pub mod operators;
pub mod pathitem;
pub mod binding;
pub mod assignment;
pub mod typename;
pub mod functiondef;
pub mod typedef;

pub use self::block::*;
pub use self::errors::*;
pub use self::module::*;
pub use self::identifier::*;
pub use self::ifexpr::*;
pub use self::expression::*;
pub use self::operators::*;
pub use self::pathitem::*;
pub use self::binding::*;
pub use self::assignment::*;
pub use self::typename::*;
pub use self::functiondef::*;
pub use self::typedef::*;

pub use ast::tokenizer::*;
pub use ast::stream::*;
pub use ast::lexer::{Span, Bracket, BracketState};
pub use failure::Error;

pub fn parse(input: &mut TokenStream) -> Result<Module, Error<ParseError>> {
	parse_module(input)
}