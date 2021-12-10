pub fn part1<'a, I, S>(lines: I) -> u32
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    lines
        .into_iter()
        .filter_map(|line| {
            let mut expected = Vec::new();
            for c in line.as_ref().chars() {
                if c == '(' {
                    expected.push(')')
                } else if c == '[' {
                    expected.push(']')
                } else if c == '{' {
                    expected.push('}')
                } else if c == '<' {
                    expected.push('>')
                } else if Some(c) == expected.pop() {
                } else if c == ')' {
                    return Some(3);
                } else if c == ']' {
                    return Some(57);
                } else if c == '}' {
                    return Some(1197);
                } else if c == '>' {
                    return Some(25137);
                } else {
                    return None;
                }
            }
            None
        })
        .sum()
}

pub fn part2<'a, I, S>(lines: I) -> Option<u64>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut scores = Vec::new();
    'outer: for line in lines {
        let mut expected = Vec::new();
        for c in line.as_ref().chars() {
            if c == '(' {
                expected.push(')')
            } else if c == '[' {
                expected.push(']')
            } else if c == '{' {
                expected.push('}')
            } else if c == '<' {
                expected.push('>')
            } else if Some(c) == expected.pop() {
            } else {
                continue 'outer;
            }
        }
        scores.push(
            expected
                .into_iter()
                .rev()
                .filter_map(|c| match c {
                    ')' => Some(1),
                    ']' => Some(2),
                    '}' => Some(3),
                    '>' => Some(4),
                    _ => None,
                })
                .fold(0, |acc, x| 5 * acc + x),
        );
    }
    scores.sort_unstable();
    scores.get(scores.len() / 2).cloned()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "[({(<(())[]>[[{[]{<()<>>",
        "[(()[<>])]({[<{<<[]>>(",
        "{([(<{}[<>[]}>{[]{[(<()>",
        "(((({<>}<{<{<>}{[]{[]{}",
        "[[<[([]))<([[{}[[()]]]",
        "[{[{({}]{}}([{[{{{}}([]",
        "{<[[]]>}<{[{[{[]{()[[[]",
        "[<(<(<(<{}))><([]([]()",
        "<{([([[(<>()){}]>(<<{{",
        "<{([{{}}[<[[[<>{}]]]>[]]",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(26397, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(288957), part2(EXAMPLE));
    }
}
