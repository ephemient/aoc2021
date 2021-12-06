use super::util;
use std::error::Error;

fn solve<'a, I, S>(times: usize, lines: I) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut fishes = [0; 9];
    for line in lines {
        for fish in line.as_ref().split(',') {
            *fishes.get_mut(fish.parse::<usize>()?).ok_or(util::Error)? += 1
        }
    }
    for _ in 0..times {
        let zero = fishes[0];
        fishes.copy_within(1..9, 0);
        fishes[6] += zero;
        fishes[8] = zero;
    }
    Ok(fishes.into_iter().sum())
}

pub fn part1<'a, I, S>(lines: I) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve(80, lines)
}

pub fn part2<'a, I, S>(lines: I) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve(256, lines)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &["3,4,3,1,2"];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(5934, part1(EXAMPLE)?);
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(26984457539, part2(EXAMPLE)?);
        Ok(())
    }
}
