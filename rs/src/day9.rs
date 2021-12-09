pub fn part1<'a, I, S>(lines: I) -> u32
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let lines: Vec<Vec<char>> = lines
        .into_iter()
        .map(|line| line.as_ref().chars().collect())
        .collect();
    lines
        .iter()
        .enumerate()
        .map(|(i, line)| -> u32 {
            line.iter()
                .enumerate()
                .filter_map(|(j, c)| -> Option<u32> {
                    if [
                        j.checked_sub(1).and_then(|j| line.get(j)),
                        j.checked_add(1).and_then(|j| line.get(j)),
                        i.checked_sub(1)
                            .and_then(|i| lines.get(i))
                            .and_then(|line| line.get(j)),
                        i.checked_add(1)
                            .and_then(|i| lines.get(i))
                            .and_then(|line| line.get(j)),
                    ]
                    .into_iter()
                    .all(|d| d.map_or(true, |d| c < d))
                    {
                        c.to_digit(10).map(|c| c + 1)
                    } else {
                        None
                    }
                })
                .sum()
        })
        .sum()
}

pub fn part2<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut visited: Vec<Vec<bool>> = lines
        .into_iter()
        .map(|line| {
            line.as_ref()
                .chars()
                .map(|c| c.to_digit(10).map_or(true, |d| d >= 9))
                .collect()
        })
        .collect();
    let mut basins = Vec::<usize>::new();
    for i in 0..visited.len() {
        for j in 0..visited[i].len() {
            if visited[i][j] {
                continue;
            }
            visited[i][j] = true;
            let mut basin = 0;
            let mut stack = vec![(i, j)];
            while let Some((i, j)) = stack.pop() {
                basin += 1;
                if let Some((i, visited)) = i.checked_sub(1).and_then(|i| {
                    visited
                        .get_mut(i)
                        .and_then(|row| row.get_mut(j))
                        .filter(|visited| !**visited)
                        .map(|visited| (i, visited))
                }) {
                    *visited = true;
                    stack.push((i, j));
                }
                if let Some((i, visited)) = i.checked_add(1).and_then(|i| {
                    visited
                        .get_mut(i)
                        .and_then(|row| row.get_mut(j))
                        .filter(|visited| !**visited)
                        .map(|visited| (i, visited))
                }) {
                    *visited = true;
                    stack.push((i, j));
                }
                if let Some((j, visited)) = j.checked_sub(1).and_then(|j| {
                    visited
                        .get_mut(i)
                        .and_then(|row| row.get_mut(j))
                        .filter(|visited| !**visited)
                        .map(|visited| (j, visited))
                }) {
                    *visited = true;
                    stack.push((i, j));
                }
                if let Some((j, visited)) = j.checked_add(1).and_then(|j| {
                    visited
                        .get_mut(i)
                        .and_then(|row| row.get_mut(j))
                        .filter(|visited| !**visited)
                        .map(|visited| (j, visited))
                }) {
                    *visited = true;
                    stack.push((i, j));
                }
            }
            basins.push(basin);
        }
    }
    basins.sort_unstable();
    basins[basins.len().saturating_sub(3)..].iter().product()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "2199943210",
        "3987894921",
        "9856789892",
        "8767896789",
        "9899965678",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(15, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(1134, part2(EXAMPLE));
    }
}
