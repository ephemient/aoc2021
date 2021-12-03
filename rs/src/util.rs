use std::error;
use std::fmt;
use std::io;
use std::str::FromStr;

#[derive(Clone, Debug)]
pub struct Error;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "error")
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

pub fn to_ioerror<E>(error: E) -> io::Error
where
    E: Into<Box<dyn error::Error + Send + Sync>>,
{
    io::Error::new(io::ErrorKind::Other, error)
}

pub fn parse_many<'a, F, I, S>(lines: I) -> Result<Vec<F>, <F as FromStr>::Err>
where
    F: FromStr,
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    lines
        .into_iter()
        .map(|s| s.as_ref().parse::<F>())
        .collect::<Result<_, _>>()
}

pub struct Transpose<I> {
    iterators: Vec<I>,
}

pub fn transpose<T>(iterators: T) -> Transpose<<<T as Iterator>::Item as IntoIterator>::IntoIter>
where
    T: Iterator,
    <T as Iterator>::Item: IntoIterator,
{
    Transpose {
        iterators: iterators.map(|it| it.into_iter()).collect(),
    }
}

impl<I> Iterator for Transpose<I>
where
    I: Iterator,
{
    type Item = Vec<<I as Iterator>::Item>;
    fn next(&mut self) -> Option<Self::Item> {
        Some(
            self.iterators
                .iter_mut()
                .filter_map(|it| it.next())
                .collect::<Vec<_>>(),
        )
        .filter(|v| !v.is_empty())
    }
}

pub struct Unpeekable<T>
where
    T: Iterator,
{
    iterator: T,
    last: Option<<T as Iterator>::Item>,
}

impl<T> Unpeekable<T>
where
    T: Iterator,
{
    pub fn new(iterator: T) -> Unpeekable<T> {
        Unpeekable {
            iterator,
            last: None,
        }
    }
}

impl<T> Unpeekable<T>
where
    T: Iterator,
{
    pub fn unpeek(&mut self, last: <T as Iterator>::Item) {
        if self.last.is_some() {
            panic!()
        }
        self.last = Some(last);
    }
}

impl<T> Iterator for Unpeekable<T>
where
    T: Iterator,
{
    type Item = <T as Iterator>::Item;
    fn next(&mut self) -> Option<Self::Item> {
        self.last.take().or_else(|| self.iterator.next())
    }
}
