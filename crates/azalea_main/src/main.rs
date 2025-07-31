/// The Azalea compiler
/// Copyright (c) 2024 Rem
use azalea_parse::{lexer::lex_tokens, parse::Parser};

fn main() {
    let src = include_str!("../../../example/types.az");
    let tokens = lex_tokens(src);

    // Debug: print all tokens
    println!("All tokens:");
    for (i, token) in tokens.clone().enumerate() {
        println!(
            "{}: {:?} at {:?} = '{}'",
            i, token.kind, token.location, token.literal
        );
    }
    println!("\nParsing...\n");

    let parse = Parser::parse(tokens).unwrap();
    println!("AST: {:#?}", parse.ast);
    println!("Errors: {:#?}", parse.errors());
}
