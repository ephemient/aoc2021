use super::util;
use regex::Regex;
use std::cmp::max;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::iter;

pub fn solve<'a, I, S>(lines: I) -> Result<(i32, usize), Box<dyn Error + Send + Sync>>
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
    let mut max_t = 0;
    let mut dy_hits = HashMap::<usize, Vec<i32>>::new();
    for dy in y0..=-y0 {
        for (t, y) in iter::once(0)
            .chain(
                iter::successors(Some(dy), |dy| Some(dy - 1)).scan(0, |y, dy| {
                    *y += dy;
                    Some(*y).filter(|y| *y >= y0)
                }),
            )
            .enumerate()
        {
            if (y0..=y1).contains(&y) {
                max_t = max(max_t, t);
                dy_hits
                    .entry(t)
                    .and_modify(|s| s.push(dy))
                    .or_insert_with(|| vec![dy]);
            }
        }
    }
    let mut max_dy = 0;
    let mut count = 0;
    for dx in ((2.0 * x0 as f64 + 0.25).sqrt() - 0.5).ceil() as i32..=x1 {
        let mut all_dys = HashSet::new();
        for (t, x) in iter::once(0)
            .chain((dx - max_t as i32..=dx).rev().scan(0, |x, dx| {
                *x += max(dx, 0);
                Some(*x).filter(|x| *x <= x1)
            }))
            .enumerate()
        {
            if (x0..=x1).contains(&x) {
                if let Some(dys) = dy_hits.get(&t) {
                    for dy in dys {
                        all_dys.insert(dy);
                    }
                }
            }
        }
        count += all_dys.len();
        max_dy = all_dys.into_iter().fold(max_dy, |x, y| max(x, *y));
    }
    Ok((max_dy * (max_dy + 1) / 2, count))
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &["target area: x=20..30, y=-10..-5"];

    #[test]
    fn solve_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!((45, 112), solve(EXAMPLE)?);
        Ok(())
    }
}
