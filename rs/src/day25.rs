pub fn part1<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut grid: Vec<Vec<char>> = lines
        .into_iter()
        .map(|line| line.as_ref().chars().collect())
        .collect();
    let width = grid.get(0)?.len();
    let height = grid.len();
    if grid.iter().skip(1).any(|row| row.len() != width) {
        return None;
    }
    let mut n = 1;
    loop {
        let mut a = false;
        let mut b = false;
        for row in &mut grid {
            let indices: Vec<_> = (0..width)
                .filter(|&i| row[i] == '>' && row[(i + 1) % width] == '.')
                .collect();
            if !indices.is_empty() {
                a = true;
            }
            for i in indices {
                row[i] = '.';
                row[(i + 1) % width] = '>';
            }
        }
        for col in 0..width {
            let indices: Vec<_> = (0..height)
                .filter(|&i| grid[i][col] == 'v' && grid[(i + 1) % height][col] == '.')
                .collect();
            if !indices.is_empty() {
                b = true;
            }
            for i in indices {
                grid[i][col] = '.';
                grid[(i + 1) % height][col] = 'v';
            }
        }
        if !a && !b {
            return Some(n);
        }
        n += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::error::Error;

    static EXAMPLE: &[&str] = &[
        "v...>>.vv>",
        ".vv>>.vv..",
        ">>.>v>...v",
        ">>v>>.>.v.",
        "v>v.vv.v..",
        ">.>>..v...",
        ".vv..>.>v.",
        "v.v..>>v.v",
        "....v..v.>",
    ];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(58), part1(EXAMPLE));
        Ok(())
    }
}
