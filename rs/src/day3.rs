use super::util::{self, transpose};
use std::char;
use std::collections::HashMap;
use std::error::Error;
use std::hash::Hash;

fn counter<I>(items: I) -> HashMap<I::Item, usize>
where
    I: Iterator,
    <I as Iterator>::Item: Eq,
    <I as Iterator>::Item: Hash,
{
    let mut counts = HashMap::new();
    for item in items {
        counts.entry(item).and_modify(|n| *n += 1).or_insert(1);
    }
    counts
}

pub fn part1<'a, I, S>(lines: I) -> Result<u32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let lines = lines
        .into_iter()
        .map(|line| line.as_ref().to_string())
        .collect::<Vec<_>>();
    let (gamma, epsilon): (String, Vec<_>) = transpose(lines.iter().map(|line| line.chars()))
        .filter_map(|column| {
            Some(
                *counter(column.into_iter())
                    .iter()
                    .max_by_key(|(_, count)| *count)?
                    .0,
            )
        })
        .map(|char| (char, char::from_u32(u32::from(char) ^ 1)))
        .unzip();
    let epsilon = epsilon
        .into_iter()
        .collect::<Option<String>>()
        .ok_or(util::Error)?;
    Ok(u32::from_str_radix(&gamma, 2)? * u32::from_str_radix(&epsilon, 2)?)
}

pub fn part2<'a, I, S>(lines: I) -> Result<u32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut o2 = lines
        .into_iter()
        .map(|line| line.as_ref().to_string())
        .collect::<Vec<_>>();
    let mut co2 = o2.clone();
    let mut i = 0;
    while o2.len() > 1 || co2.len() > 1 {
        let common = *counter(o2.iter().filter_map(|line| line.as_bytes().get(i)))
            .into_iter()
            .max_by_key(|(byte, count)| (*count, *byte))
            .ok_or(util::Error)?
            .0;
        let uncommon = *counter(co2.iter().filter_map(|line| line.as_bytes().get(i)))
            .into_iter()
            .min_by_key(|(byte, count)| (*count, *byte))
            .ok_or(util::Error)?
            .0;
        o2.retain(|line| line.as_bytes().get(i) == Some(&common));
        co2.retain(|line| line.as_bytes().get(i) == Some(&uncommon));
        i += 1;
    }
    Ok(u32::from_str_radix(o2.first().ok_or(util::Error)?, 2)?
        * u32::from_str_radix(co2.first().ok_or(util::Error)?, 2)?)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001",
        "00010", "01010",
    ];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(198, part1(EXAMPLE)?);
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(230, part2(EXAMPLE)?);
        Ok(())
    }
}
