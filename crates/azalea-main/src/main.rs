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

    println!("Parsing source...\n");

    let parse = Parser::parse(tokens).unwrap();
    // println!("AST: {:#?}", parse.ast);

    for error in parse.errors() {
        eprintln!("{}", error.report().show_pretty_message());
    }

    // Type check the AST
    let mut typeck = Typechecker::new();
    let typeck_result = typeck.walk_ast(parse.ast.clone());

    if let Err(err) = typeck_result {
        eprintln!("Type checking failed: {:?}", err);
        return;
    }

    // Codegen time!
    let mut js = JSCodegen::new();
    js.emit_code(parse.ast);

    println!("\nGenerated JS Code:\n{}", js.js_code);
}
