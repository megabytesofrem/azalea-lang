/// The Azalea compiler
/// Copyright (c) 2024 Rem
use azalea_front::lexer::lex_tokens;

fn main() {
    let src = include_str!("../../../example/count.az");
    let tokens = lex_tokens(src).collect::<Vec<_>>();

    println!("{:#?}", tokens);
}
