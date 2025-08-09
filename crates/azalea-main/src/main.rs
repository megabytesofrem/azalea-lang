/// The Azalea compiler
/// Copyright (c) 2024 Rem
use azalea_core::{
    codegen::js::{Emit, JSCodegen},
    lexer::lex_tokens,
    parse::Parser,
    typeck::typecheck::Typechecker,
};

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    let tokens = lex_tokens(&src);

    // Debug: print all tokens
    // println!("All tokens:");
    // for (i, lex_result) in tokens.clone().enumerate() {
    //     match lex_result {
    //         Ok(token) => {
    //             println!(
    //                 "{}: {:?} at {:?} = '{}'",
    //                 i, token.kind, token.location, token.literal
    //             );
    //         }
    //         Err(lex_error) => {
    //             println!("{}: Lexical Error: {:?}", i, lex_error);
    //         }
    //     }
    // }
    println!("\nParsing...\n");

    let parse = Parser::parse(tokens).unwrap();
    println!("AST: {:#?}", parse.ast);
    println!("Errors: {:#?}", parse.errors());

    // Type check the AST
    let mut typeck = Typechecker::new();
    let typeck_result = typeck.walk_ast(parse.ast.clone());

    println!("Typecheck Result: {:#?}", typeck_result);

    // Codegen time!
    let mut js = JSCodegen::new();
    js.emit_code(parse.ast);

    println!("\nGenerated JS Code:\n{}", js.js_code);
}
