use std::cmp::Reverse;
use std::collections::BinaryHeap;

fn solve(risks: &[Vec<u32>]) -> Option<u32> {
    let mut best: Vec<Vec<_>> = risks
        .iter()
        .map(|row| row.iter().map(|_| u32::MAX).collect())
        .collect();
    best[0][0] = 0;
    let mut queue = BinaryHeap::<Reverse<(u32, (u16, u16))>>::new();
    queue.push(Reverse((0, (0, 0))));
    while let Some(Reverse((_, (x, y)))) = queue.pop() {
        let c = best[y as usize][x as usize];
        if (y as usize) + 1 == risks.len() && (x as usize) + 1 == risks[y as usize].len() {
            return Some(c);
        }
        let mut f = |c, x, y| {
            if c < best[y as usize][x as usize] {
                best[y as usize][x as usize] = c;
                queue.push(Reverse((c, (x, y))));
            }
        };
        if let Some(x) = x.checked_sub(1) {
            if let Some(risk) = risks[y as usize].get(x as usize) {
                f(c + risk, x, y);
            }
        }
        if let Some(y) = y.checked_sub(1) {
            if let Some(risk) = risks.get(y as usize).and_then(|row| row.get(x as usize)) {
                f(c + risk, x, y);
            }
        }
        if let Some(y) = y.checked_add(1) {
            if let Some(risk) = risks.get(y as usize).and_then(|row| row.get(x as usize)) {
                f(c + risk, x, y);
            }
        }
        if let Some(x) = x.checked_add(1) {
            if let Some(risk) = risks[y as usize].get(x as usize) {
                f(c + risk, x, y);
            }
        }
    }
    None
}

pub fn part1<'a, I, S>(lines: I) -> Option<u32>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let risks = lines
        .into_iter()
        .map(|line| line.as_ref().chars().map(|c| c.to_digit(10)).collect())
        .collect::<Option<Vec<_>>>()?;
    solve(&risks)
}

pub fn part2<'a, I, S>(lines: I) -> Option<u32>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let risks = lines
        .into_iter()
        .map(|line| line.as_ref().chars().map(|c| c.to_digit(10)).collect())
        .collect::<Option<Vec<Vec<_>>>>()?;
    let risks = (0..5)
        .flat_map(|dy| {
            risks.iter().map(move |row| {
                (0..5)
                    .flat_map(|dx| row.iter().map(move |n| (n - 1 + dx + dy) % 9 + 1))
                    .collect()
            })
        })
        .collect::<Vec<_>>();
    solve(&risks)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::error::Error;

    static EXAMPLE: &[&str] = &[
        "1163751742",
        "1381373672",
        "2136511328",
        "3694931569",
        "7463417111",
        "1319128137",
        "1359912421",
        "3125421639",
        "1293138521",
        "2311944581",
    ];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(40), part1(EXAMPLE));
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(315), part2(EXAMPLE));
        Ok(())
    }
}
