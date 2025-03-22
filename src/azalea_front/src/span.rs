use super::lexer::SourceLoc;

#[derive(Debug, Clone)]
pub struct Span<T> {
    pub target: T,
    pub loc: SourceLoc,
}

impl<T> Span<T> {
    pub fn new(target: T, loc: SourceLoc) -> Self {
        Self { target, loc }
    }
}

impl<T> Span<T> {
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Span<U> {
        Span {
            target: f(self.target),
            loc: self.loc,
        }
    }

    pub fn flat_map<U, F: FnOnce(T) -> Span<U>>(self, f: F) -> Span<U> {
        let Span = f(self.target);
        Span {
            target: Span.target,
            loc: self.loc,
        }
    }

    // Map a function that returns a Result over the target of the Span type
    pub fn map_with_span<U, E, F>(self, f: F) -> Result<Span<U>, E>
    where
        F: FnOnce(T) -> Result<U, E>,
    {
        let Span = f(self.target)?;
        Ok(Span {
            target: Span,
            loc: self.loc,
        })
    }
}
