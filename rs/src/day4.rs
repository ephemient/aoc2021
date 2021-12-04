use super::util;
use std::error::Error;
use std::iter;

#[allow(clippy::type_complexity)]
fn parse<'a, I, S>(
    lines: I,
) -> Result<(Vec<i32>, Vec<[[Option<i32>; 5]; 5]>), Box<dyn Error + Send + Sync>>
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
        .collect::<Result<_, _>>()?;
    let boards = iter::from_fn(|| {
        (|| {
            match iter.next() {
                Some(sep) => {
                    if !sep.as_ref().is_empty() {
                        return Err(Box::<dyn Error + Send + Sync>::from(util::Error));
                    }
                }
                None => return Ok(None),
            }
            let mut board = [[None; 5]; 5];
            for row in board.iter_mut() {
                for (i, word) in iter
                    .next()
                    .ok_or(util::Error)?
                    .as_ref()
                    .split(char::is_whitespace)
                    .filter(|word| !word.is_empty())
                    .enumerate()
                {
                    *row.get_mut(i).ok_or(util::Error)? = Some(word.parse()?);
                }
            }
            Ok(Some(board))
        })()
        .transpose()
    })
    .collect::<Result<_, _>>()?;

    Ok((draws, boards))
}

fn mark(board: &mut [[Option<i32>; 5]; 5], draw: i32) {
    for row in board.iter_mut() {
        for item in row.iter_mut() {
            if *item == Some(draw) {
                *item = None
            }
        }
    }
}

fn is_bingo(board: &[[Option<i32>; 5]; 5]) -> bool {
    board
        .iter()
        .any(|row| row.iter().all(|item| item.is_none()))
        || (0..5).any(|column| board.iter().all(|row| row[column].is_none()))
}

pub fn part1<'a, I, S>(lines: I) -> Result<Option<i32>, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (draws, mut boards) = parse(lines)?;
    for draw in draws {
        for board in boards.iter_mut() {
            mark(board, draw);
            if is_bingo(&*board) {
                let total = board
                    .iter()
                    .map(|row| row.iter().filter_map(|item| *item).sum::<i32>())
                    .sum::<i32>();
                return Ok(Some(draw * total));
            }
        }
    }
    Ok(None)
}

pub fn part2<'a, I, S>(lines: I) -> Result<Option<i32>, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (draws, mut boards) = parse(lines)?;
    let mut score = None;
    for draw in draws {
        for board in boards.iter_mut() {
            mark(board, draw);
        }
        boards.retain(|board| {
            if is_bingo(&*board) {
                let total = board
                    .iter()
                    .map(|row| row.iter().filter_map(|item| *item).sum::<i32>())
                    .sum::<i32>();
                score = Some(draw * total);
                false
            } else {
                true
            }
        });
    }
    Ok(score)
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
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(4512), part1(EXAMPLE)?);
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(1924), part2(EXAMPLE)?);
        Ok(())
    }
}
