use super::util;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::num::ParseIntError;
use std::str::FromStr;

#[derive(Clone, Debug)]
enum ParseMoveError {
    Prefix(String),
    Suffix(ParseIntError),
}

impl Display for ParseMoveError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ParseMoveError::Prefix(string) => {
                write!(f, "no parse: {:?}", string)
            }
            ParseMoveError::Suffix(error) => error.fmt(f),
        }
    }
}

impl Error for ParseMoveError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ParseMoveError::Suffix(error) => Some(error),
            _ => None,
        }
    }
}

impl From<ParseIntError> for ParseMoveError {
    fn from(error: ParseIntError) -> Self {
        ParseMoveError::Suffix(error)
    }
}

enum Move {
    Horizontal(i32),
    Vertical(i32),
}

impl FromStr for Move {
    type Err = ParseMoveError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(suffix) = s.strip_prefix("forward ") {
            Ok(Move::Horizontal(suffix.parse()?))
        } else if let Some(suffix) = s.strip_prefix("down ") {
            Ok(Move::Vertical(suffix.parse()?))
        } else if let Some(suffix) = s.strip_prefix("up ") {
            Ok(Move::Vertical(-suffix.parse()?))
        } else {
            Err(ParseMoveError::Prefix(s.to_string()))
        }
    }
}

pub fn part1<'a, I, S>(lines: I) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (mut x, mut y) = (0, 0);
    for m in util::parse_many::<'a, Move, _, _>(lines)?.iter() {
        match m {
            Move::Horizontal(d) => {
                x += d;
            }
            Move::Vertical(d) => {
                y += d;
            }
        }
    }
    Ok(x * y)
}

pub fn part2<'a, I, S>(lines: I) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (mut x, mut y, mut depth) = (0, 0, 0);
    for m in util::parse_many::<'a, Move, _, _>(lines)?.iter() {
        match m {
            Move::Horizontal(d) => {
                x += d;
                y += d * depth
            }
            Move::Vertical(d) => {
                depth += d;
            }
        }
    }
    Ok(x * y)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "forward 5",
        "down 5",
        "forward 8",
        "up 3",
        "down 8",
        "forward 2",
    ];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(150, part1(EXAMPLE)?);
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(900, part2(EXAMPLE)?);
        Ok(())
    }
}
