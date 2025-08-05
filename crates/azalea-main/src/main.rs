/// The Azalea compiler
/// Copyright (c) 2024 Rem
use azalea_core::{lexer::lex_tokens, parse::Parser};

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    let tokens = lex_tokens(&src);

    // Debug: print all tokens
    println!("All tokens:");
    for (i, lex_result) in tokens.clone().enumerate() {
        match lex_result {
            Ok(token) => {
                println!(
                    "{}: {:?} at {:?} = '{}'",
                    i, token.kind, token.location, token.literal
                );
            }
            Err(lex_error) => {
                println!("{}: Lexical Error: {:?}", i, lex_error);
            }
        }
    }
    println!("\nParsing...\n");

    let parse = Parser::parse(tokens).unwrap();
    println!("AST: {:#?}", parse.ast);
    println!("Errors: {:#?}", parse.errors());
}
