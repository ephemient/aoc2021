use super::util;
use itertools::Itertools;
use std::cmp::{max, min};
use std::collections::HashSet;
use std::error::Error;

fn run<'a, I, S>(
    lines: I,
    limit: Option<usize>,
) -> Result<HashSet<(i32, i32)>, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut iter = lines.into_iter();
    let mut points = HashSet::new();
    for line in &mut iter {
        let line = line.as_ref();
        if line.is_empty() {
            break;
        }
        let mut iter = line.split(',');
        let x = iter.next().ok_or(util::Error)?.parse()?;
        let y = iter.next().ok_or(util::Error)?.parse()?;
        if iter.next().is_none() {
            points.insert((x, y));
        } else {
            return Err(util::Error.into());
        }
    }
    for (i, line) in iter.enumerate() {
        let line = line.as_ref();
        if let Some(fold_x) = line.strip_prefix("fold along x=") {
            let fold_x: i32 = fold_x.parse()?;
            points = points
                .drain()
                .map(|(x, y)| {
                    if x < fold_x {
                        (x, y)
                    } else {
                        (2 * fold_x - x, y)
                    }
                })
                .collect();
        } else if let Some(fold_y) = line.strip_prefix("fold along y=") {
            let fold_y: i32 = fold_y.parse()?;
            points = points
                .drain()
                .map(|(x, y)| {
                    if y < fold_y {
                        (x, y)
                    } else {
                        (x, 2 * fold_y - y)
                    }
                })
                .collect();
        } else {
            return Err(util::Error.into());
        }
        if limit.map_or(false, |limit| i + 1 >= limit) {
            break;
        }
    }
    Ok(points)
}

pub fn part1<'a, I, S>(lines: I) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    Ok(run(lines, Some(1))?.len())
}

pub fn part2<'a, I, S>(lines: I) -> Result<String, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let points = run(lines, None)?;
    let ((x0, y0), (x1, y1)) = points.iter().fold(
        ((i32::MAX, i32::MAX), (i32::MIN, i32::MIN)),
        |((x0, y0), (x1, y1)), &(x, y)| ((min(x0, x), min(y0, y)), (max(x1, x), max(y1, y))),
    );
    Ok((y0..=y1)
        .map(|y| {
            (x0..=x1)
                .map(|x| {
                    if points.contains(&(x, y)) {
                        '\u{2593}'
                    } else {
                        '\u{2591}'
                    }
                })
                .collect::<String>()
        })
        .join("\n"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::error::Error;

    static EXAMPLE: &[&str] = &[
        "6,10",
        "0,14",
        "9,10",
        "0,3",
        "10,4",
        "4,11",
        "6,0",
        "6,12",
        "4,1",
        "0,13",
        "10,12",
        "3,4",
        "3,0",
        "8,4",
        "1,10",
        "2,14",
        "8,10",
        "9,0",
        "",
        "fold along y=7",
        "fold along x=5",
    ];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(17, part1(EXAMPLE)?);
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(
            "▓▓▓▓▓\n▓░░░▓\n▓░░░▓\n▓░░░▓\n▓▓▓▓▓".to_string(),
            part2(EXAMPLE)?
        );
        Ok(())
    }
}
