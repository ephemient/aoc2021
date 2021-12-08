use super::util;
use std::cmp::{max, min, Ord};
use std::collections::HashSet;
use std::error::Error;
use std::ops::Add;
use std::str::FromStr;

struct Segment<Item> {
    x0: Item,
    y0: Item,
    x1: Item,
    y1: Item,
}

impl<Item> FromStr for Segment<Item>
where
    Item: FromStr + Ord,
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
            Ok(if x0 < x1 || x0 == x1 && y0 < y1 {
                Segment { x0, y0, x1, y1 }
            } else {
                Segment {
                    x0: x1,
                    y0: y1,
                    x1: x0,
                    y1: y0,
                }
            })
        } else {
            Err(util::Error.into())
        }
    }
}

struct SegmentIterator<Item> {
    x: Item,
    y: Item,
    dx: Item,
    dy: Item,
    len: usize,
}

impl<Item> Iterator for SegmentIterator<Item>
where
    Item: Copy + Add<Output = Item>,
{
    type Item = (Item, Item);
    fn next(&mut self) -> Option<Self::Item> {
        self.len = self.len.checked_sub(1)?;
        let item = (self.x, self.y);
        self.x = self.x + self.dx;
        self.y = self.y + self.dy;
        Some(item)
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }
}

impl Segment<i32> {
    fn intersection(&self, other: &Segment<i32>) -> Option<SegmentIterator<i32>> {
        let ret = if self.x0 == self.x1 && other.x0 == other.x1 {
            if self.x0 != other.x0 {
                return None;
            }
            let y0 = max(self.y0, other.y0);
            let y1 = min(self.y1, other.y1);
            SegmentIterator {
                x: self.x0,
                y: y0,
                dx: 0,
                dy: 1,
                len: usize::try_from(y1 - y0).ok()? + 1,
            }
        } else if self.x0 == self.x1 {
            if !(other.x0..=other.x1).contains(&self.x0) {
                return None;
            }
            let y = (other.y1 - other.y0) / (other.x1 - other.x0) * (self.x0 - other.x0) + other.y0;
            if !(self.y0..=self.y1).contains(&y) {
                return None;
            }
            SegmentIterator {
                x: self.x0,
                y,
                dx: 0,
                dy: 0,
                len: 1,
            }
        } else if other.x0 == other.x1 {
            if !(self.x0..=self.x1).contains(&other.x0) {
                return None;
            }
            let y = (self.y1 - self.y0) / (self.x1 - self.x0) * (other.x0 - self.x0) + self.y0;
            if !(other.y0..=other.y1).contains(&y) {
                return None;
            }
            SegmentIterator {
                x: other.x0,
                y,
                dx: 0,
                dy: 0,
                len: 1,
            }
        } else {
            let m0 = (self.y1 - self.y0) / (self.x1 - self.x0);
            let m1 = (other.y1 - other.y0) / (other.x1 - other.x0);
            let a0 = self.y0 - m0 * self.x0;
            let a1 = other.y0 - m1 * other.x0;
            if m0 == m1 {
                if a0 != a1 {
                    return None;
                }
                let x0 = max(self.x0, other.x0);
                let x1 = min(self.x1, other.x1);
                SegmentIterator {
                    x: x0,
                    y: m0 * x0 + a0,
                    dx: 1,
                    dy: m0,
                    len: usize::try_from(x1 - x0).ok()? + 1,
                }
            } else if (a1 - a0) % (m1 - m0) == 0 {
                let x = -(a1 - a0) / (m1 - m0);
                if !(max(self.x0, other.x0)..=min(self.x1, other.x1)).contains(&x) {
                    return None;
                }
                SegmentIterator {
                    x,
                    y: m0 * x + a0,
                    dx: 0,
                    dy: 0,
                    len: 1,
                }
            } else {
                return None;
            }
        };
        Some(ret)
    }
}

pub fn part1<'a, I, S>(lines: I) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let segments = lines
        .into_iter()
        .map(|line| line.as_ref().parse())
        .filter_map(|res| {
            res.map(|segment: Segment<i32>| {
                if segment.x0 == segment.x1 || segment.y0 == segment.y1 {
                    Some(segment)
                } else {
                    None
                }
            })
            .transpose()
        })
        .collect::<Result<Vec<_>, _>>()?;
    let points = segments
        .iter()
        .enumerate()
        .flat_map(|(i, segment0)| {
            segments[i + 1..]
                .iter()
                .filter_map(|segment1| segment0.intersection(segment1))
        })
        .flatten()
        .collect::<HashSet<_>>();
    Ok(points.len())
}

pub fn part2<'a, I, S>(lines: I) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let segments: Vec<Segment<i32>> = util::parse_many(lines)?;
    let points = segments
        .iter()
        .enumerate()
        .flat_map(|(i, segment0)| {
            segments[i + 1..]
                .iter()
                .filter_map(|segment1| segment0.intersection(segment1))
        })
        .flatten()
        .collect::<HashSet<_>>();
    Ok(points.len())
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
