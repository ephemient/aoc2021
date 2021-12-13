fn solve<'a, I, S>(lines: I, bonus: bool) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut edges = Vec::new();
    let start;
    let end;
    {
        let mut names = Vec::new();
        for line in lines {
            let mut iter = line.as_ref().split('-');
            let lhs = iter.next()?;
            let rhs = iter.next()?;
            if iter.next().is_some() {
                return None;
            }
            let src = names
                .iter()
                .position(|name| name == &lhs)
                .unwrap_or_else(|| {
                    names.push(lhs);
                    names.len() - 1
                });
            let dst = names
                .iter()
                .position(|name| name == &rhs)
                .unwrap_or_else(|| {
                    names.push(rhs);
                    names.len() - 1
                });
            match edges.get_mut(src) {
                Some(adj) => adj,
                None => {
                    edges.push(Vec::new());
                    edges.last_mut()?
                }
            }
            .push((dst, rhs.chars().all(char::is_uppercase)));
            match edges.get_mut(dst) {
                Some(adj) => adj,
                None => {
                    edges.push(Vec::new());
                    edges.last_mut()?
                }
            }
            .push((src, lhs.chars().all(char::is_uppercase)));
        }
        if names.len() >= (u32::BITS as usize) - 1 {
            panic!("input too large");
        }
        start = names.iter().position(|name| name == &"start")?;
        end = names.iter().position(|name| name == &"end")?;
    }
    let mut stack = vec![(if bonus { 0 } else { 1u32 << (u32::BITS - 1) }, start)];
    let mut count = 0;
    while let Some((state, i)) = stack.pop() {
        if i == end {
            count += 1;
            continue;
        }
        for &(j, big) in edges.get(i)? {
            if j == start {
            } else if big {
                stack.push((state, j));
            } else if state & 1 << j == 0 {
                stack.push((state | 1 << j, j));
            } else if state & 1 << (u32::BITS - 1) == 0 {
                stack.push((state | 1 << (u32::BITS - 1), j));
            }
        }
    }
    Some(count)
}

pub fn part1<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve(lines, false)
}

pub fn part2<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve(lines, true)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::error::Error;

    static EXAMPLE_1: &[&str] = &["start-A", "start-b", "A-c", "A-b", "b-d", "A-end", "b-end"];
    static EXAMPLE_2: &[&str] = &[
        "dc-end", "HN-start", "start-kj", "dc-start", "dc-HN", "LN-dc", "HN-end", "kj-sa", "kj-HN",
        "kj-dc",
    ];
    static EXAMPLE_3: &[&str] = &[
        "fs-end", "he-DX", "fs-he", "start-DX", "pj-DX", "end-zg", "zg-sl", "zg-pj", "pj-he",
        "RW-he", "fs-DX", "pj-RW", "zg-RW", "start-pj", "he-WI", "zg-he", "pj-fs", "start-RW",
    ];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(10), part1(EXAMPLE_1));
        assert_eq!(Some(19), part1(EXAMPLE_2));
        assert_eq!(Some(226), part1(EXAMPLE_3));
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(36), part2(EXAMPLE_1));
        assert_eq!(Some(103), part2(EXAMPLE_2));
        assert_eq!(Some(3509), part2(EXAMPLE_3));
        Ok(())
    }
}
