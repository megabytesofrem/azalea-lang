use type_error::TypeError;

pub mod resolver;
pub mod type_error;
pub mod typecheck;

pub type Return<'a, T> = Result<T, TypeError>;
