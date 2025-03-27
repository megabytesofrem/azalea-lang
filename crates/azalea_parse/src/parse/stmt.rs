use super::Parser;
use super::base as parser;
use crate::ast::Block;
use crate::ast::Stmt;

impl<'a> Parser<'a> {
    pub(crate) fn parse_block(&mut self) -> parser::Return<Block> {
        todo!()
    }

    pub(crate) fn parse_stmt(&mut self) -> parser::Return<Stmt> {
        todo!()
    }

    fn parse_let(&mut self) -> parser::Return<Stmt> {
        todo!()
    }
}
