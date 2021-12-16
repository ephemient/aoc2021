use std::iter;

enum Packet {
    Literal {
        version: u32,
        value: u64,
    },
    Operator {
        version: u32,
        tag: u32,
        children: Vec<Packet>,
    },
}

fn parse(string: &str) -> Option<Packet> {
    parse_packet(
        &mut string
            .chars()
            .filter_map(|c| c.to_digit(16))
            .flat_map(|x| [x & 8 != 0, x & 4 != 0, x & 2 != 0, x & 1 != 0]),
    )
}

fn next_int<I, const N: u32>(iter: &mut I) -> Option<u32>
where
    I: Iterator<Item = bool>,
{
    let mut ret = 0;
    for _ in 0..N {
        ret = 2 * ret + if iter.next()? { 1 } else { 0 };
    }
    Some(ret)
}

fn parse_packet<I>(iter: &mut I) -> Option<Packet>
where
    I: Iterator<Item = bool>,
{
    let version = next_int::<_, 3>(iter)?;
    let tag = next_int::<_, 3>(iter)?;
    if tag == 4 {
        let mut value = 0;
        loop {
            let cont = iter.next()?;
            value = 16 * value + next_int::<_, 4>(iter)? as u64;
            if !cont {
                break;
            }
        }
        Some(Packet::Literal { version, value })
    } else {
        let children = if iter.next()? {
            (0..next_int::<_, 11>(iter)?)
                .map(|_| parse_packet(iter))
                .collect::<Option<_>>()?
        } else {
            let n = next_int::<_, 15>(iter)? as usize;
            let slice: Vec<bool> = iter.take(n).collect();
            if slice.len() != n {
                return None;
            }
            let mut iter = slice.into_iter();
            let children = iter::from_fn(|| parse_packet(&mut iter)).collect();
            if iter.next().is_some() {
                return None;
            }
            children
        };
        Some(Packet::Operator {
            version,
            tag,
            children,
        })
    }
}

fn sum_versions(packet: &Packet) -> u32 {
    match packet {
        Packet::Literal { version, .. } => *version,
        Packet::Operator {
            version, children, ..
        } => *version + children.iter().map(sum_versions).sum::<u32>(),
    }
}

fn eval(packet: &Packet) -> Option<u64> {
    match packet {
        Packet::Literal { value, .. } => Some(*value),
        Packet::Operator {
            tag: 0, children, ..
        } => Some(children.iter().filter_map(eval).sum()),
        Packet::Operator {
            tag: 1, children, ..
        } => Some(children.iter().filter_map(eval).product()),
        Packet::Operator {
            tag: 2, children, ..
        } => children.iter().filter_map(eval).min(),
        Packet::Operator {
            tag: 3, children, ..
        } => children.iter().filter_map(eval).max(),
        Packet::Operator {
            tag: 5, children, ..
        } => Some(if eval(children.get(0)?)? > eval(children.get(1)?)? {
            1
        } else {
            0
        }),
        Packet::Operator {
            tag: 6, children, ..
        } => Some(if eval(children.get(0)?)? < eval(children.get(1)?)? {
            1
        } else {
            0
        }),
        Packet::Operator {
            tag: 7, children, ..
        } => Some(if eval(children.get(0)?)? == eval(children.get(1)?)? {
            1
        } else {
            0
        }),
        _ => None,
    }
}

pub fn part1<'a, I, S>(lines: I) -> Option<u32>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    Some(sum_versions(&parse(lines.into_iter().next()?.as_ref())?))
}

pub fn part2<'a, I, S>(lines: I) -> Option<u64>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    eval(&parse(lines.into_iter().next()?.as_ref())?)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::error::Error;

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(16), part1(&["8A004A801A8002F478"]));
        assert_eq!(Some(12), part1(&["620080001611562C8802118E34"]));
        assert_eq!(Some(23), part1(&["C0015000016115A2E0802F182340"]));
        assert_eq!(Some(31), part1(&["A0016C880162017C3686B18A3D4780"]));
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(3), part2(&["C200B40A82"]));
        assert_eq!(Some(54), part2(&["04005AC33890"]));
        assert_eq!(Some(7), part2(&["880086C3E88112"]));
        assert_eq!(Some(9), part2(&["CE00C43D881120"]));
        assert_eq!(Some(1), part2(&["D8005AC2A8F0"]));
        assert_eq!(Some(0), part2(&["F600BC2D8F"]));
        assert_eq!(Some(0), part2(&["9C005AC2F8F0"]));
        assert_eq!(Some(1), part2(&["9C0141080250320F1802104A08"]));
        Ok(())
    }
}
