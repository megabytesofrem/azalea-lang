/// The Azalea compiler
/// Copyright (c) 2024 Rem
use azalea_parse::lexer::lex_tokens;

fn main() {
    let src = include_str!("../../../example/types.az");
    let tokens = lex_tokens(src).collect::<Vec<_>>();

    println!("{:#?}", tokens);
}
