use super::util;
use std::char;
use std::cmp::max;
use std::collections::HashMap;
use std::error::Error;

pub fn solve<'a, I, S>(lines: I) -> Result<Option<(i32, i32)>, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut iter = lines.into_iter();
    let draws = iter
        .next()
        .ok_or(util::Error)?
        .as_ref()
        .split(',')
        .map(|draw| draw.parse())
        .collect::<Result<Vec<_>, _>>()?;
    let mut draw_turns = HashMap::new();
    for (i, draw) in draws.iter().enumerate() {
        draw_turns.entry(draw).or_insert(i);
    }

    let mut min_score: Option<(usize, i32)> = None;
    let mut max_score: Option<(usize, i32)> = None;

    if let Some(line) = iter.next() {
        if !line.as_ref().is_empty() {
            return Err(util::Error.into());
        }

        while let Some(line) = iter.next() {
            let row = line
                .as_ref()
                .split(char::is_whitespace)
                .filter_map(|s| if s.is_empty() { None } else { Some(s.parse()) })
                .collect::<Result<Vec<_>, _>>()?;
            if row.is_empty() {
                return Err(util::Error.into());
            }
            let width = row.len();
            let mut board: Vec<Vec<i32>> = vec![row];
            for line in &mut iter {
                let line = line.as_ref();
                if line.is_empty() {
                    break;
                }
                let row = line
                    .split(char::is_whitespace)
                    .filter_map(|s| if s.is_empty() { None } else { Some(s.parse()) })
                    .collect::<Result<Vec<_>, _>>()?;
                if row.len() != width {
                    return Err(util::Error.into());
                }
                board.push(row);
            }
            let turns: Vec<Vec<Option<usize>>> = board
                .iter()
                .map(|row| {
                    row.iter()
                        .map(|cell| draw_turns.get(cell).cloned())
                        .collect()
                })
                .collect();
            if let Some(turn) = turns
                .iter()
                .filter_map(|row| {
                    row.iter()
                        .cloned()
                        .reduce(|x, y| x.zip(y).map(|(x, y)| max(x, y)))
                        .flatten()
                })
                .chain((0..width).filter_map(|col| {
                    turns
                        .iter()
                        .map(|row| row[col])
                        .reduce(|x, y| x.zip(y).map(|(x, y)| max(x, y)))
                        .flatten()
                }))
                .min()
            {
                let remaining: i32 = board
                    .into_iter()
                    .flat_map(|row| {
                        row.into_iter().filter(|cell| {
                            draw_turns
                                .get(&cell)
                                .map_or(false, |draw_turn| *draw_turn > turn)
                        })
                    })
                    .sum();
                if min_score.map_or(true, |(min_turn, _)| min_turn > turn) {
                    min_score = Some((turn, draws[turn] * remaining));
                }
                if max_score.map_or(true, |(max_turn, _)| max_turn < turn) {
                    max_score = Some((turn, draws[turn] * remaining));
                }
            }
        }
    }

    min_score.zip(max_score);
    Ok(min_score
        .zip(max_score)
        .map(|((_, min), (_, max))| (min, max)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
        "",
        "22 13 17 11  0",
        " 8  2 23  4 24",
        "21  9 14 16  7",
        " 6 10  3 18  5",
        " 1 12 20 15 19",
        "",
        " 3 15  0  2 22",
        " 9 18 13 17  5",
        "19  8  7 25 23",
        "20 11 10 24  4",
        "14 21 16 12  6",
        "",
        "14 21 17 24  4",
        "10 16 15  9 19",
        "18  8 23 26 20",
        "22 11 13  6  5",
        " 2  0 12  3  7",
    ];

    #[test]
    fn solve_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some((4512, 1924)), solve(EXAMPLE)?);
        Ok(())
    }
}
