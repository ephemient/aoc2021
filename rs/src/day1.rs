use super::util;
use std::error::Error;
use std::vec::Vec;

pub fn part1<'a, I, S>(lines: I) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let nums: Vec<i32> = util::parse_many(lines)?;
    Ok(nums
        .iter()
        .zip(nums.iter().skip(1))
        .filter(|(x, y)| x < y)
        .count())
}

pub fn part2<'a, I, S>(lines: I) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let nums: Vec<i32> = util::parse_many(lines)?;
    let sums: Vec<i32> = nums.windows(3).map(|w| w.iter().sum()).collect();
    Ok(sums
        .iter()
        .zip(sums.iter().skip(1))
        .filter(|(x, y)| x < y)
        .count())
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "199", "200", "208", "210", "200", "207", "240", "269", "260", "263",
    ];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(7, part1(EXAMPLE)?);
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(5, part2(EXAMPLE)?);
        Ok(())
    }
}
