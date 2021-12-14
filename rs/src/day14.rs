use std::collections::HashMap;
use std::iter;

fn solve<'a, I, S>(lines: I, times: usize) -> Option<u64>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut iter = lines.into_iter();
    let initial = iter.next()?.as_ref();
    if initial.len() < 2 {
        return None;
    }
    let initial_first = initial.chars().next()?;
    let initial_last = initial.chars().last()?;
    iter.next().filter(|line| line.as_ref().is_empty())?;
    let mut rules = HashMap::new();
    for line in iter {
        let mut chars = line.as_ref().chars();
        let x = chars.next()?;
        let y = chars.next()?;
        if !chars.as_str().starts_with(" -> ") {
            return None;
        }
        let mut chars = chars.skip(4);
        let z = chars.next()?;
        if chars.next().is_some() {
            return None;
        }
        rules.insert((x, y), [(x, z), (z, y)]);
    }
    let mut state = HashMap::new();
    for src in initial.chars().zip(initial.chars().skip(1)) {
        state.entry(src).and_modify(|n| *n += 1).or_insert(1);
    }
    for _ in 0..times {
        let mut new_state = HashMap::new();
        for (src, n) in state.drain() {
            for dst in rules.get(&src)? {
                new_state.entry(*dst).and_modify(|m| *m += n).or_insert(n);
            }
        }
        state = new_state;
    }
    let mut counts = HashMap::new();
    for ((x, y), n) in iter::once(((initial_first, initial_last), 1)).chain(state.drain()) {
        counts.entry(x).and_modify(|m| *m += n).or_insert(n);
        counts.entry(y).and_modify(|m| *m += n).or_insert(n);
    }
    Some((counts.values().max()? - counts.values().min()?) / 2)
}

pub fn part1<'a, I, S>(lines: I) -> Option<u64>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve(lines, 10)
}

pub fn part2<'a, I, S>(lines: I) -> Option<u64>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve(lines, 40)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::error::Error;

    static EXAMPLE: &[&str] = &[
        "NNCB", "", "CH -> B", "HH -> N", "CB -> H", "NH -> C", "HB -> C", "HC -> B", "HN -> C",
        "NN -> C", "BH -> H", "NC -> B", "NB -> B", "BN -> B", "BB -> N", "BC -> B", "CC -> N",
        "CN -> C",
    ];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(1588), part1(EXAMPLE));
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(2188189693529), part2(EXAMPLE));
        Ok(())
    }
}
