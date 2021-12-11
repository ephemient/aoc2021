pub fn solve<'a, I, S>(lines: I) -> Option<(usize, usize)>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut state: Vec<Vec<Option<u32>>> = lines
        .into_iter()
        .map(|line| line.as_ref().chars().map(|c| c.to_digit(10)).collect())
        .collect();
    let mut flashes = Vec::new();
    while state
        .iter()
        .any(|row| row.iter().any(|n| n.map_or(false, |n| n != 0)))
    {
        for row in state.iter_mut() {
            for n in row.iter_mut() {
                *n = Some(n.map_or(1, |n| n + 1));
            }
        }
        let mut count = 0;
        while state
            .iter()
            .any(|row| row.iter().any(|n| n.map_or(false, |n| n > 9)))
        {
            for y0 in 0..state.len() {
                for x0 in 0..state[y0].len() {
                    if state[y0][x0].map_or(true, |n| n <= 9) {
                        continue;
                    }
                    count += 1;
                    for y1 in y0.saturating_sub(1)..=y0 + 1 {
                        let row = match state.get_mut(y1) {
                            Some(row) => row,
                            _ => continue,
                        };
                        for x1 in x0.saturating_sub(1)..=x0 + 1 {
                            if let Some(n) = row.get_mut(x1) {
                                if x0 == x1 && y0 == y1 {
                                    *n = None;
                                } else if let Some(m) = *n {
                                    *n = Some(m + 1);
                                }
                            }
                        }
                    }
                }
            }
        }
        flashes.push(count);
    }
    Some((flashes.iter().take(100).sum(), flashes.len()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::error::Error;

    static EXAMPLE: &[&str] = &[
        "5483143223",
        "2745854711",
        "5264556173",
        "6141336146",
        "6357385478",
        "4167524645",
        "2176841721",
        "6882881134",
        "4846848554",
        "5283751526",
    ];

    #[test]
    fn solve_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some((1656, 195)), solve(EXAMPLE));
        Ok(())
    }
}
