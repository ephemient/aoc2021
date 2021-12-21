use super::util;
use std::cmp::max;
use std::error::Error;

fn parse<'a, I, S>(lines: I) -> Result<(u32, u32), Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut iter = lines.into_iter();
    let player1 = iter
        .next()
        .and_then(|line| line.as_ref().strip_prefix("Player 1 starting position: "))
        .ok_or(util::Error)?
        .parse()?;
    let player2 = iter
        .next()
        .and_then(|line| line.as_ref().strip_prefix("Player 2 starting position: "))
        .ok_or(util::Error)?
        .parse()?;
    Ok((player1, player2))
}

pub fn part1<'a, I, S>(lines: I) -> Result<u32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (mut player1, mut player2) = parse(lines)?;
    let (mut score1, mut score2, mut n) = (0, 0, 0);
    while score2 < 1000 {
        let tmp = (player1 + n % 100 + (n + 1) % 100 + (n + 2) % 100 + 2) % 10 + 1;
        player1 = player2;
        player2 = tmp;
        let tmp = score1 + tmp;
        score1 = score2;
        score2 = tmp;
        n += 3;
    }
    Ok(n * score1)
}

struct Score {
    data: [(u64, u64); 44100],
}
impl Score {
    fn new() -> Score {
        Score {
            data: [(0, 0); 44100],
        }
    }

    fn get(&mut self, player1: u32, player2: u32, score1: u32, score2: u32) -> (u64, u64) {
        let i = (player1 + 10 * player2 + 100 * score1 + 2100 * score2 - 11) as usize;
        if self.data[i] == (0, 0) {
            let (mut x, mut y) = (0, 0);
            for (d, n) in [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)] {
                let play = (player1 + d - 1) % 10 + 1;
                if score1 + play > 20 {
                    x += n;
                } else {
                    let (x2, y2) = self.get(player2, play, score2, score1 + play);
                    x += n * y2;
                    y += n * x2;
                }
            }
            self.data[i] = (x, y);
        }
        self.data[i]
    }
}

pub fn part2<'a, I, S>(lines: I) -> Result<u64, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (player1, player2) = parse(lines)?;
    let (x, y) = Score::new().get(player1, player2, 0, 0);
    Ok(max(x, y))
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::error::Error;

    static EXAMPLE: &[&str] = &[
        "Player 1 starting position: 4",
        "Player 2 starting position: 8",
    ];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(739785, part1(EXAMPLE)?);
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(444356092776315, part2(EXAMPLE)?);
        Ok(())
    }
}
