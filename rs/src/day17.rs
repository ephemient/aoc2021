use super::util;
use regex::Regex;
use std::cmp::{max, min};
use std::error::Error;
use std::iter;

pub fn solve<'a, I, S>(lines: I) -> Result<Option<(i32, i32)>, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    lazy_static! {
        static ref RE: Regex =
            Regex::new(r"\Atarget area: x=(\d+)\.\.(\d+), y=(-\d+)\.\.(-\d+)").unwrap();
    };
    let cap = RE
        .captures(lines.into_iter().next().ok_or(util::Error)?.as_ref())
        .ok_or(util::Error)?;
    let x0: i32 = cap.get(1).ok_or(util::Error)?.as_str().parse()?;
    let x1: i32 = cap.get(2).ok_or(util::Error)?.as_str().parse()?;
    let y0: i32 = cap.get(3).ok_or(util::Error)?.as_str().parse()?;
    let y1: i32 = cap.get(4).ok_or(util::Error)?.as_str().parse()?;
    drop(cap);
    let (mut max_dy, mut count) = (None, 0);
    for dx in 0..=x1 {
        for dy in min(y0, 0)..=max(y0.abs(), y1.abs()) {
            if (1..=dx)
                .rev()
                .chain(iter::repeat(0))
                .scan(0, |x, dx| {
                    *x += dx;
                    Some(*x).filter(|x| *x <= x1)
                })
                .zip(
                    iter::successors(Some(dy), |dy| Some(dy - 1)).scan(0, |y, dy| {
                        *y += dy;
                        Some(*y).filter(|y| dy > 0 || *y >= y0)
                    }),
                )
                .any(|(x, y)| (x0..=x1).contains(&x) && (y0..=y1).contains(&y))
            {
                if max_dy < Some(dy) {
                    max_dy = Some(dy);
                }
                count += 1;
            }
        }
    }
    Ok(max_dy.map(|dy: i32| (dy * (dy + 1) / 2, count)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &["target area: x=20..30, y=-10..-5"];

    #[test]
    fn solve_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some((45, 112)), solve(EXAMPLE)?);
        Ok(())
    }
}
