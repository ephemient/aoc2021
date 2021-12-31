use super::util;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::ops::{Index, IndexMut};
use std::str::FromStr;

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
enum Register {
    W,
    X,
    Y,
    Z,
}

impl Register {
    fn from_str(value: &str) -> Option<Register> {
        match value {
            "w" => Some(Register::W),
            "x" => Some(Register::X),
            "y" => Some(Register::Y),
            "z" => Some(Register::Z),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
enum BinaryOperator {
    Add,
    Mul,
    Div,
    Mod,
    Eql,
}

impl BinaryOperator {
    fn from_str(value: &str) -> Option<BinaryOperator> {
        match value {
            "add" => Some(BinaryOperator::Add),
            "mul" => Some(BinaryOperator::Mul),
            "div" => Some(BinaryOperator::Div),
            "mod" => Some(BinaryOperator::Mod),
            "eql" => Some(BinaryOperator::Eql),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Instruction<Item> {
    Input(Register),
    BinaryOperation(BinaryOperator, Register, Result<Item, Register>),
}

#[derive(Clone, Debug)]
enum ParseInstructionError<Err> {
    Syntax(String),
    Operand(Err),
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct Alu<Item> {
    w: Item,
    x: Item,
    y: Item,
    z: Item,
}

impl<Item: Default> Alu<Item> {
    fn new() -> Alu<Item> {
        Alu {
            w: Default::default(),
            x: Default::default(),
            y: Default::default(),
            z: Default::default(),
        }
    }
}

impl<Item> Index<Register> for Alu<Item> {
    type Output = Item;
    fn index(&self, index: Register) -> &Self::Output {
        match index {
            Register::W => &self.w,
            Register::X => &self.x,
            Register::Y => &self.y,
            Register::Z => &self.z,
        }
    }
}

impl<Item> IndexMut<Register> for Alu<Item> {
    fn index_mut(&mut self, index: Register) -> &mut Self::Output {
        match index {
            Register::W => &mut self.w,
            Register::X => &mut self.x,
            Register::Y => &mut self.y,
            Register::Z => &mut self.z,
        }
    }
}

impl<Err: Display> Display for ParseInstructionError<Err> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ParseInstructionError::Syntax(string) => write!(f, "bad syntax: {}", string),
            ParseInstructionError::Operand(err) => err.fmt(f),
        }
    }
}

impl<Err: Error> Error for ParseInstructionError<Err> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ParseInstructionError::Operand(err) => <Err as Error>::source(err),
            _ => None,
        }
    }
}

impl<Item: FromStr> FromStr for Instruction<Item> {
    type Err = ParseInstructionError<<Item as FromStr>::Err>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let err = || ParseInstructionError::Syntax(s.to_string());
        if let Some(suffix) = s.strip_prefix("inp ") {
            return Ok(Instruction::Input(
                Register::from_str(suffix).ok_or_else(err)?,
            ));
        }
        let mut groups = s.split(' ');
        let op = groups
            .next()
            .and_then(BinaryOperator::from_str)
            .ok_or_else(err)?;
        let lhs = groups.next().and_then(Register::from_str).ok_or_else(err)?;
        let rhs = groups.next().ok_or_else(err)?;
        if groups.next().is_some() {
            return Err(err());
        }
        let rhs = match Register::from_str(rhs) {
            Some(rhs) => Err(rhs),
            _ => Ok(rhs.parse().map_err(ParseInstructionError::Operand)?),
        };
        Ok(Instruction::BinaryOperation(op, lhs, rhs))
    }
}

fn solve(
    instructions: &[Instruction<i32>],
    nums: &[u32],
    prefix: u64,
    alu: Alu<i64>,
) -> Option<u64> {
    let mut alu = alu;
    for (i, instruction) in instructions.iter().enumerate() {
        match *instruction {
            Instruction::Input(lhs) => {
                return nums
                    .iter()
                    .filter_map(|&num| {
                        alu[lhs] = num as i64;
                        if check_range(&instructions[i + 1..], &alu) != Some(false) {
                            solve(
                                &instructions[i + 1..],
                                nums,
                                10 * prefix + num as u64,
                                alu.clone(),
                            )
                        } else {
                            None
                        }
                    })
                    .next()
            }
            Instruction::BinaryOperation(op, lhs, rhs) => {
                let a = alu[lhs];
                let b = rhs.map(|rhs| rhs as i64).unwrap_or_else(|rhs| alu[rhs]);
                alu[lhs] = match op {
                    BinaryOperator::Add => a.wrapping_add(b),
                    BinaryOperator::Mul => a.wrapping_mul(b),
                    BinaryOperator::Div => a.wrapping_div(b),
                    BinaryOperator::Mod => a.wrapping_rem(b),
                    BinaryOperator::Eql => {
                        if a == b {
                            1
                        } else {
                            0
                        }
                    }
                };
            }
        }
    }
    if alu.z == 0 {
        Some(prefix)
    } else {
        None
    }
}

fn check_range(instructions: &[Instruction<i32>], alu: &Alu<i64>) -> Option<bool> {
    let mut alu = Alu {
        w: (alu.w, alu.w),
        x: (alu.x, alu.x),
        y: (alu.y, alu.y),
        z: (alu.z, alu.z),
    };
    for instruction in instructions {
        match *instruction {
            Instruction::Input(lhs) => alu[lhs] = (1, 9),
            Instruction::BinaryOperation(op, lhs, rhs) => {
                let (a, b) = alu[lhs];
                let (c, d) = rhs
                    .map(|rhs| (rhs as i64, rhs as i64))
                    .unwrap_or_else(|rhs| alu[rhs]);
                alu[lhs] = match op {
                    BinaryOperator::Add => (a + c, b + d),
                    BinaryOperator::Mul => {
                        if a >= 0 && c >= 0 {
                            (a * c, b * d)
                        } else if b <= 0 && d <= 0 {
                            (b * d, a * c)
                        } else if a >= 0 && d <= 0 {
                            (a * d, b * c)
                        } else if b <= 0 && c >= 0 {
                            (b * c, a * d)
                        } else {
                            let xs = [0, a * c, a * d, b * c, b * d];
                            (*xs.iter().min()?, *xs.iter().max()?)
                        }
                    }
                    BinaryOperator::Div => {
                        if c > 0 {
                            (a / d, b / c)
                        } else if d < 0 {
                            (a / c, b / d)
                        } else {
                            return None;
                        }
                    }
                    BinaryOperator::Mod => {
                        if 0 < c && c == d {
                            if b - a + 1 < c && a % c <= b % c {
                                (a % c, b % c)
                            } else {
                                (0, c - 1)
                            }
                        } else {
                            return None;
                        }
                    }
                    BinaryOperator::Eql => {
                        if a == b && b == c && c == d {
                            (1, 1)
                        } else if a <= d && c <= b {
                            (0, 1)
                        } else {
                            (0, 0)
                        }
                    }
                }
            }
        }
    }
    Some(alu.z.0 <= 0 && 0 <= alu.z.1)
}

pub fn part1<'a, I, S>(lines: I) -> Result<Option<u64>, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let instructions = util::parse_many(lines)?;
    Ok(solve(
        &instructions[..],
        &[9, 8, 7, 6, 5, 4, 3, 2, 1],
        0,
        Alu::new(),
    ))
}

pub fn part2<'a, I, S>(lines: I) -> Result<Option<u64>, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let instructions = util::parse_many(lines)?;
    Ok(solve(
        &instructions[..],
        &[1, 2, 3, 4, 5, 6, 7, 8, 9],
        0,
        Alu::new(),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::error::Error;

    static EXAMPLE: &[&str] = &[
        "inp w",
        "mul z 31",
        "add z w",
        "mod z 16777216",
        "inp w",
        "mul z 31",
        "add z w",
        "mod z 16777216",
        "inp w",
        "mul z 31",
        "add z w",
        "mod z 16777216",
        "inp w",
        "mul z 31",
        "add z w",
        "mod z 16777216",
        "inp w",
        "mul z 31",
        "add z w",
        "mod z 16777216",
        "inp w",
        "mul z 31",
        "add z w",
        "mod z 16777216",
        "inp w",
        "mul z 31",
        "add z w",
        "mod z 16777216",
        "inp w",
        "mul z 31",
        "add z w",
        "mod z 16777216",
        "inp w",
        "mul z 31",
        "add z w",
        "mod z 16777216",
        "inp w",
        "mul z 31",
        "add z w",
        "mod z 16777216",
        "inp w",
        "mul z 31",
        "add z w",
        "mod z 16777216",
        "inp w",
        "mul z 31",
        "add z w",
        "mod z 16777216",
        "inp w",
        "mul z 31",
        "add z w",
        "mod z 16777216",
        "inp w",
        "mul z 31",
        "add z w",
        "mod z 16777216",
    ];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(99999993811817), part1(EXAMPLE)?);
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(11111128657365), part2(EXAMPLE)?);
        Ok(())
    }
}
