fn the<I>(mut iter: I) -> Option<<I as Iterator>::Item>
where
    I: Iterator,
{
    let item = iter.next()?;
    if iter.next().is_none() {
        Some(item)
    } else {
        None
    }
}

fn bits(s: &str) -> u32 {
    s.chars()
        .fold(0, |acc, c| acc | 1u32.wrapping_shl(c.into()))
}

pub fn part1<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut total = 0;
    for line in lines {
        total += line
            .as_ref()
            .rsplit(" | ")
            .next()?
            .split(' ')
            .filter(|s| s.len() == 2 || s.len() == 4 || s.len() == 3 || s.len() == 7)
            .count();
    }
    Some(total)
}

pub fn part2<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut total = 0;
    for line in lines {
        let mut iter = line.as_ref().split(" | ");
        let signals: Vec<_> = iter.next()?.split(' ').map(bits).collect();
        let one = the(signals.iter().filter(|s| s.count_ones() == 2))?;
        let seven = the(signals.iter().filter(|s| s.count_ones() == 3))?;
        let four = the(signals.iter().filter(|s| s.count_ones() == 4))?;
        let two = the(signals
            .iter()
            .filter(|&s| s.count_ones() == 5 && (s & !(four | seven)).count_ones() == 2))?;
        let three = the(signals
            .iter()
            .filter(|&s| s.count_ones() == 5 && (s & !two).count_ones() == 1))?;
        let five = the(signals
            .iter()
            .filter(|&s| s.count_ones() == 5 && (s & !two).count_ones() == 2))?;
        let six = the(signals
            .iter()
            .filter(|&s| s.count_ones() == 6 && one & !s != 0))?;
        let nine = the(signals
            .iter()
            .filter(|&s| s.count_ones() == 6 && !s & three == 0))?;
        let zero = the(signals
            .iter()
            .filter(|&s| s.count_ones() == 6 && !s & three & !one != 0))?;
        let eight = signals.iter().find(|s| s.count_ones() == 7)?;
        let digits = [zero, one, two, three, four, five, six, seven, eight, nine];
        let mut output = 0;
        for s in the(iter)?.split(' ') {
            let s = bits(s);
            output = 10 * output + digits.iter().position(|signal| **signal == s)?;
        }
        total += output;
    }
    Some(total)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE_1: &[&str] =
        &["acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"];
    static EXAMPLE_2: &[&str] = &[
        "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
        "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
        "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
        "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
        "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
        "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
        "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
        "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
        "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
        "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(Some(26), part1(EXAMPLE_2));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(5353), part2(EXAMPLE_1));
        assert_eq!(Some(61229), part2(EXAMPLE_2));
    }
}
