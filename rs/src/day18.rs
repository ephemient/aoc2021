use std::iter;

#[derive(Clone, Copy)]
enum SnailfishToken<T> {
    Open,
    Close,
    Value(T),
}

fn parse(line: &str) -> Vec<SnailfishToken<u32>> {
    line.chars()
        .filter_map(|c| match c {
            '[' => Some(SnailfishToken::Open),
            ']' => Some(SnailfishToken::Close),
            _ => c.to_digit(10).map(SnailfishToken::Value),
        })
        .collect()
}

fn add<I, J>(x: I, y: J) -> Vec<SnailfishToken<u32>>
where
    I: Iterator<Item = SnailfishToken<u32>>,
    J: Iterator<Item = SnailfishToken<u32>>,
{
    let mut vec: Vec<SnailfishToken<u32>> = iter::once(SnailfishToken::Open)
        .chain(x)
        .chain(y)
        .chain(iter::once(SnailfishToken::Close))
        .collect();
    'outer: loop {
        let mut depth = 0;
        for (i, window) in vec[..].windows(4).enumerate() {
            if depth > 3 {
                if let [SnailfishToken::Open, SnailfishToken::Value(x), SnailfishToken::Value(y), SnailfishToken::Close] =
                    *window
                {
                    vec.splice(i..i + 4, iter::once(SnailfishToken::Value(0)));
                    if let Some(t) = vec.iter_mut().take(i).rev().find_map(|t| match t {
                        SnailfishToken::Value(t) => Some(t),
                        _ => None,
                    }) {
                        *t += x;
                    }
                    if let Some(t) = vec.iter_mut().skip(i + 1).find_map(|t| match t {
                        SnailfishToken::Value(t) => Some(t),
                        _ => None,
                    }) {
                        *t += y;
                    }
                    continue 'outer;
                }
            }
            match window[0] {
                SnailfishToken::Open => depth += 1,
                SnailfishToken::Close => depth -= 1,
                _ => {}
            }
        }
        if let Some((i, t)) = vec.iter_mut().enumerate().find_map(|(i, t)| match t {
            SnailfishToken::Value(t) => Some((i, *t)).filter(|(_, t)| *t > 9),
            _ => None,
        }) {
            vec.splice(
                i..=i,
                [
                    SnailfishToken::Open,
                    SnailfishToken::Value(t / 2),
                    SnailfishToken::Value((t + 1) / 2),
                    SnailfishToken::Close,
                ],
            );
        } else {
            break vec;
        }
    }
}

fn magnitude<I>(tokens: &mut I) -> Option<u32>
where
    I: Iterator<Item = SnailfishToken<u32>>,
{
    match tokens.next()? {
        SnailfishToken::Open => {
            let lhs = magnitude(tokens)?;
            let rhs = magnitude(tokens)?;
            match tokens.next()? {
                SnailfishToken::Close => Some(3 * lhs + 2 * rhs),
                _ => None,
            }
        }
        SnailfishToken::Value(t) => Some(t),
        _ => None,
    }
}

pub fn part1<'a, I, S>(lines: I) -> Option<u32>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    lines
        .into_iter()
        .map(|line| parse(line.as_ref()))
        .reduce(|x, y| add(x.into_iter(), y.into_iter()))
        .and_then(|tokens| magnitude(&mut tokens.into_iter()))
}

pub fn part2<'a, I, S>(lines: I) -> Option<u32>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let vec: Vec<_> = lines.into_iter().map(|line| parse(line.as_ref())).collect();
    vec.iter()
        .enumerate()
        .flat_map(|(i, x)| {
            vec.iter().enumerate().filter_map(move |(j, y)| {
                if i != j {
                    magnitude(&mut add(x.iter().copied(), y.iter().copied()).into_iter())
                } else {
                    None
                }
            })
        })
        .max()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::error::Error;

    static EXAMPLE: &[&str] = &[
        "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]",
        "[[[5,[2,8]],4],[5,[[9,9],0]]]",
        "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]",
        "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]",
        "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]",
        "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]",
        "[[[[5,4],[7,7]],8],[[8,3],8]]",
        "[[9,3],[[9,9],[6,[4,9]]]]",
        "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]",
        "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]",
    ];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(4140), part1(EXAMPLE));
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(3993), part2(EXAMPLE));
        Ok(())
    }
}
