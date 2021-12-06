use super::util;
use std::error::Error;

build_const!("libaoc2021_day6");

fn solve<'a, I, S, const N: usize>(
    lut: &[usize; N],
    lines: I,
) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut ret = 0;
    for line in lines {
        for word in line.as_ref().split(',') {
            ret += lut.get(word.parse::<usize>()?).ok_or(util::Error)?;
        }
    }
    Ok(ret)
}

pub fn part1<'a, I, S>(lines: I) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve(&LUT_80, lines)
}

pub fn part2<'a, I, S>(lines: I) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve(&LUT_256, lines)
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
