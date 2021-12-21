fn solve<'a, I, S>(lines: I, count: usize) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut iter = lines.into_iter();
    let line1 = iter.next()?.as_ref();
    if line1.len() != 512 {
        return None;
    }
    let mut alg = ['.'; 512];
    for (i, c) in line1.chars().enumerate() {
        alg[i] = c;
    }
    if !iter.next()?.as_ref().is_empty() {
        return None;
    }
    let mut image: Vec<Vec<_>> = iter.map(|line| line.as_ref().chars().collect()).collect();
    let mut fill = '.';
    for _ in 0..count {
        image = (0..image.len() + 2)
            .map(|y| {
                let line0 = y.checked_sub(2).and_then(|y| image.get(y));
                let line1 = y.checked_sub(1).and_then(|y| image.get(y));
                let line2 = image.get(y);
                let width = [&line0, &line1, &line2]
                    .into_iter()
                    .filter_map(|line| line.map(|line| line.len()))
                    .max()
                    .unwrap_or(0);
                (0..width + 2)
                    .map(|x| {
                        alg[[
                            line0.and_then(|line| x.checked_sub(2).and_then(|x| line.get(x))),
                            line0.and_then(|line| x.checked_sub(1).and_then(|x| line.get(x))),
                            line0.and_then(|line| line.get(x)),
                            line1.and_then(|line| x.checked_sub(2).and_then(|x| line.get(x))),
                            line1.and_then(|line| x.checked_sub(1).and_then(|x| line.get(x))),
                            line1.and_then(|line| line.get(x)),
                            line2.and_then(|line| x.checked_sub(2).and_then(|x| line.get(x))),
                            line2.and_then(|line| x.checked_sub(1).and_then(|x| line.get(x))),
                            line2.and_then(|line| line.get(x)),
                        ]
                        .into_iter()
                        .fold(0, |acc, c| acc << 1 | *c.unwrap_or(&fill) as usize & 1)]
                    })
                    .collect()
            })
            .collect();
        fill = alg[(fill as usize & 1) * 511]
    }
    if fill == '.' {
        Some(
            image
                .into_iter()
                .map(|line| line.into_iter().filter(|c| *c == '#').count())
                .sum(),
        )
    } else {
        None
    }
}

pub fn part1<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve(lines, 2)
}

pub fn part2<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve(lines, 50)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::error::Error;

    static EXAMPLE: &[&str] = &[
        "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#",
        "",
        "#..#.",
        "#....",
        "##..#",
        "..#..",
        "..###",
    ];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(35), part1(EXAMPLE));
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(3351), part2(EXAMPLE));
        Ok(())
    }
}
