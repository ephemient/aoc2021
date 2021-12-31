use std::cmp::{max, min, Reverse};
use std::collections::{BinaryHeap, HashMap};

pub fn part1<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut state = Vec::new();
    for line in lines.into_iter() {
        for (i, c) in line.as_ref().chars().skip(1).enumerate() {
            match c {
                '.' => state.push(vec![0]),
                'A'..='D' => state.get_mut(i)?.push(c as u32 - 'A' as u32 + 1),
                _ => {}
            }
        }
    }
    let mut costs = HashMap::new();
    let mut heap = BinaryHeap::new();
    heap.push((Reverse(0), state));
    loop {
        let state = heap.pop()?.1;
        let cost = *costs.entry(state.clone()).or_insert(0);
        if state
            .iter()
            .enumerate()
            .all(|(x, stack)| stack.iter().all(|&a| a == 0 || x == 2 * a as usize))
        {
            return Some(cost);
        }
        let mut priority_moves = Vec::new();
        let mut other_moves = Vec::new();
        for (i, src) in state.iter().enumerate() {
            if let Some((x, &a)) = src.iter().enumerate().find(|(_, a)| **a != 0) {
                if i != 0
                    && i != state.len() - 1
                    && i % 2 == 0
                    && src.iter().all(|&a| a == 0 || i == 2 * a as usize)
                {
                    continue;
                }
                for (j, dst) in state.iter().enumerate().filter(|&(j, _)| i != j) {
                    if (min(i, j) + 1..max(i, j)).any(|k| state[k][0] != 0) {
                        continue;
                    }
                    if let Some((y, _)) = dst.iter().enumerate().rev().find(|(_, b)| **b == 0) {
                        if j == 2 * a as usize && y > 0 {
                            if dst.iter().all(|&b| b == 0 || j == 2 * b as usize) {
                                &mut priority_moves
                            } else {
                                continue;
                            }
                        } else if x > 0 && (j == 0 || j == state.len() - 1 || j % 2 != 0) {
                            &mut other_moves
                        } else {
                            continue;
                        }
                        .push((a, i, x, j, y));
                    }
                }
            }
        }
        let moves = if priority_moves.is_empty() {
            other_moves
        } else {
            priority_moves.into_iter().take(1).collect()
        };
        for (a, i, x, j, y) in moves {
            let mut state2 = state.clone();
            state2[i][x] = 0;
            state2[j][y] = a;
            let w = (1..a).fold(1, |w, _| 10 * w);
            let cost2 = cost + w * (max(i, j) - min(i, j) + x + y);
            if costs.get(&state2).filter(|&&c| c <= cost2).is_none() {
                costs.insert(state2.clone(), cost2);
                heap.push((Reverse(cost2), state2));
            }
        }
    }
}

pub fn part2<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut lines: Vec<_> = lines.into_iter().map(|s| s.as_ref()).collect();
    lines.insert(3, "  #D#C#B#A#");
    lines.insert(4, "  #D#B#A#C#");
    part1(&lines)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::error::Error;

    static EXAMPLE: &[&str] = &[
        "#############",
        "#...........#",
        "###B#C#B#D###",
        "  #A#D#C#A#",
        "  #########",
    ];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(12521), part1(EXAMPLE));
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(44169), part2(EXAMPLE));
        Ok(())
    }
}
