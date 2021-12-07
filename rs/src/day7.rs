use std::cmp::min;
use std::error::Error;
use std::vec::Vec;

pub fn part1<'a, I, S>(lines: I) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut nums = lines
        .into_iter()
        .flat_map(|line| line.as_ref().split(','))
        .map(|s| s.parse())
        .collect::<Result<Vec<i32>, _>>()?;
    nums.sort_unstable();
    let median = nums[nums.len() / 2];
    Ok(nums.into_iter().map(|num| (num - median).abs()).sum())
}

pub fn part2<'a, I, S>(lines: I) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let nums = lines
        .into_iter()
        .flat_map(|line| line.as_ref().split(','))
        .map(|s| s.parse())
        .collect::<Result<Vec<i32>, _>>()?;
    let mean = nums.iter().sum::<i32>() / i32::try_from(nums.len())?;
    let f = |y: i32| -> i32 {
        nums.iter()
            .map(|x| {
                let z = (x - y).abs();
                z * (z + 1) / 2
            })
            .sum()
    };
    Ok(min(f(mean), f(mean + 1)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &["16,1,2,0,4,2,7,1,2,14"];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(37, part1(EXAMPLE)?);
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(168, part2(EXAMPLE)?);
        Ok(())
    }
}
