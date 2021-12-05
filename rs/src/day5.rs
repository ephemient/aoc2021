use super::util;
use std::cmp::{max, min};
use std::collections::HashMap;
use std::error::Error;
use std::str::FromStr;

struct Segment<Item> {
    x0: Item,
    y0: Item,
    x1: Item,
    y1: Item,
}

impl<Item> FromStr for Segment<Item>
where
    Item: FromStr,
    <Item as FromStr>::Err: Error + Send + Sync + 'static,
{
    type Err = Box<dyn Error + Send + Sync>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s
            .split(" -> ")
            .map(|part| -> Result<(Item, Item), Self::Err> {
                let mut iter = part.split(',');
                let x = iter.next().ok_or(util::Error)?.parse()?;
                let y = iter.next().ok_or(util::Error)?.parse()?;
                if iter.next().is_none() {
                    Ok((x, y))
                } else {
                    Err(util::Error.into())
                }
            });
        let (x0, y0) = iter.next().ok_or(util::Error)??;
        let (x1, y1) = iter.next().ok_or(util::Error)??;
        if iter.next().is_none() {
            Ok(Segment { x0, y0, x1, y1 })
        } else {
            Err(util::Error.into())
        }
    }
}

pub fn part1<'a, I, S>(lines: I) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut points = HashMap::<(i32, i32), usize>::new();
    for line in lines {
        let Segment { x0, y0, x1, y1 } = line.as_ref().parse()?;
        if x0 == x1 {
            for y in min(y0, y1)..max(y0, y1) + 1 {
                points.entry((x0, y)).and_modify(|n| *n += 1).or_insert(1);
            }
        } else if y0 == y1 {
            for x in min(x0, x1)..max(x0, x1) + 1 {
                points.entry((x, y0)).and_modify(|n| *n += 1).or_insert(1);
            }
        }
    }
    Ok(points.into_values().filter(|n| *n > 1).count())
}

pub fn part2<'a, I, S>(lines: I) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut points = HashMap::<(i32, i32), usize>::new();
    for line in lines {
        let Segment { x0, y0, x1, y1 } = line.as_ref().parse()?;
        if x0 == x1 {
            for y in min(y0, y1)..max(y0, y1) + 1 {
                points.entry((x0, y)).and_modify(|n| *n += 1).or_insert(1);
            }
        } else if y0 == y1 {
            for x in min(x0, x1)..max(x0, x1) + 1 {
                points.entry((x, y0)).and_modify(|n| *n += 1).or_insert(1);
            }
        } else if (x1 - x0).abs() == (y1 - y0).abs() {
            let dx = (x1 - x0).signum();
            let dy = (y1 - y0).signum();
            for i in 0..(x1 - x0).abs() + 1 {
                points
                    .entry((x0 + i * dx, y0 + i * dy))
                    .and_modify(|n| *n += 1)
                    .or_insert(1);
            }
        }
    }
    Ok(points.into_values().filter(|n| *n > 1).count())
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "0,9 -> 5,9",
        "8,0 -> 0,8",
        "9,4 -> 3,4",
        "2,2 -> 2,1",
        "7,0 -> 7,4",
        "6,4 -> 2,0",
        "0,9 -> 2,9",
        "3,4 -> 1,4",
        "0,0 -> 8,8",
        "5,5 -> 8,2",
    ];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(5, part1(EXAMPLE)?);
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(12, part2(EXAMPLE)?);
        Ok(())
    }
}
