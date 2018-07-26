pub mod syntaxtree;
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

pub use ast::tokenizer::{Keyword, Symbol as TokenSymbol};
pub use self::syntaxtree::*;
pub use self::operators::*;
pub use ast::treeify::*;
pub use self::pathitem::*;
pub use failure::Error;

/// Parse a block from the tokens (will use all of the tokens or error)
pub fn parse(in_tokens: &[TokenTree]) -> Result<SyntaxTree, Error<ParseError>> {
	let mut stree = SyntaxTree::new(Vec::new(), Vec::new(), Vec::new(), Vec::new());
	let mut tokens = in_tokens;

	while !tokens.is_empty() {
		if let Some((func, leftovers)) = functiondef::take_functiondef(tokens)? {
			tokens = leftovers;
			stree.functions.push(func);
			continue;
		} else if let Some((typedef, leftovers)) = typedef::take_typedef(tokens)? {
			tokens = leftovers;
			stree.types.push(typedef);
			continue;
		} else if let Some((u, leftovers)) = uses::take_use(tokens)? {
			tokens = leftovers;
			stree.uses.push(u);
			continue;
		} else if let Some((module, leftovers)) = module::take_module(tokens)? {
			tokens = leftovers;
			stree.modules.push(module);
			continue;
		}

		return Err(ParseError::Other.into()); // Didn't get a function or a typedef in the root
	}

	Ok(stree)
}
