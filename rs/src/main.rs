#[macro_use]
extern crate build_const;

use aoc2021::{
    day1, day10, day11, day12, day13, day14, day15, day16, day17, day18, day19, day2, day20, day21,
    day22, day23, day24, day25, day3, day4, day5, day6, day7, day8, day9, util,
};
use std::collections::HashSet;
use std::env;
use std::io;

build_const!("aoc2021");

fn main() -> io::Result<()> {
    let args = env::args()
        .skip(1)
        .filter_map(|s| s.parse().ok())
        .collect::<HashSet<u32>>();

    if args.is_empty() || args.contains(&1) {
        println!("Day 1");
        println!("{:?}", day1::part1(DAY1).map_err(util::to_ioerror)?);
        println!("{:?}", day1::part2(DAY1).map_err(util::to_ioerror)?);
        println!();
    }

    if args.is_empty() || args.contains(&2) {
        println!("Day 2");
        println!("{:?}", day2::part1(DAY2).map_err(util::to_ioerror)?);
        println!("{:?}", day2::part2(DAY2).map_err(util::to_ioerror)?);
        println!();
    }

    if args.is_empty() || args.contains(&3) {
        println!("Day 3");
        println!("{:?}", day3::part1(DAY3).map_err(util::to_ioerror)?);
        println!("{:?}", day3::part2(DAY3).map_err(util::to_ioerror)?);
        println!();
    }

    if args.is_empty() || args.contains(&4) {
        println!("Day 4");
        let (part1, part2) = day4::solve(DAY4)
            .and_then(|x| Ok(x.ok_or(util::Error)?))
            .map_err(util::to_ioerror)?;
        println!("{:?}", part1);
        println!("{:?}", part2);
        println!();
    }

    if args.is_empty() || args.contains(&5) {
        println!("Day 5");
        println!("{:?}", day5::part1(DAY5).map_err(util::to_ioerror)?);
        println!("{:?}", day5::part2(DAY5).map_err(util::to_ioerror)?);
        println!();
    }

    if args.is_empty() || args.contains(&6) {
        println!("Day 6");
        println!("{:?}", day6::part1(DAY6).map_err(util::to_ioerror)?);
        println!("{:?}", day6::part2(DAY6).map_err(util::to_ioerror)?);
        println!();
    }

    if args.is_empty() || args.contains(&7) {
        println!("Day 7");
        println!("{:?}", day7::part1(DAY7).map_err(util::to_ioerror)?);
        println!("{:?}", day7::part2(DAY7).map_err(util::to_ioerror)?);
        println!();
    }

    if args.is_empty() || args.contains(&8) {
        println!("Day 8");
        println!(
            "{:?}",
            day8::part1(DAY8).ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!(
            "{:?}",
            day8::part2(DAY8).ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!();
    }

    if args.is_empty() || args.contains(&9) {
        println!("Day 9");
        println!("{:?}", day9::part1(DAY9));
        println!("{:?}", day9::part2(DAY9));
        println!();
    }

    if args.is_empty() || args.contains(&10) {
        println!("Day 10");
        println!("{:?}", day10::part1(DAY10));
        println!(
            "{:?}",
            day10::part2(DAY10).ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!();
    }

    if args.is_empty() || args.contains(&11) {
        println!("Day 11");
        println!(
            "{:?}",
            day11::solve(DAY11).ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!();
    }

    if args.is_empty() || args.contains(&12) {
        println!("Day 12");
        println!(
            "{:?}",
            day12::part1(DAY12).ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!(
            "{:?}",
            day12::part2(DAY12).ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!();
    }

    if args.is_empty() || args.contains(&13) {
        println!("Day 13");
        println!("{:?}", day13::part1(DAY13).map_err(util::to_ioerror)?);
        println!("{}", day13::part2(DAY13).map_err(util::to_ioerror)?);
        println!();
    }

    if args.is_empty() || args.contains(&14) {
        println!("Day 14");
        println!(
            "{:?}",
            day14::part1(DAY14).ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!(
            "{:?}",
            day14::part2(DAY14).ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!();
    }

    if args.is_empty() || args.contains(&15) {
        println!("Day 15");
        println!(
            "{:?}",
            day15::part1(DAY15).ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!(
            "{:?}",
            day15::part2(DAY15).ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!();
    }

    if args.is_empty() || args.contains(&16) {
        println!("Day 16");
        println!(
            "{:?}",
            day16::part1(DAY16).ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!(
            "{:?}",
            day16::part2(DAY16).ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!();
    }

    if args.is_empty() || args.contains(&17) {
        println!("Day 17");
        let (part1, part2) = day17::solve(DAY17).map_err(util::to_ioerror)?;
        println!("{:?}", part1);
        println!("{:?}", part2);
        println!();
    }

    if args.is_empty() || args.contains(&18) {
        println!("Day 18");
        println!(
            "{:?}",
            day18::part1(DAY18).ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!(
            "{:?}",
            day18::part2(DAY18).ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!();
    }

    if args.is_empty() || args.contains(&19) {
        println!("Day 19");
        let (part1, part2) = day19::solve(DAY19).map_err(util::to_ioerror)?;
        println!("{:?}", part1);
        println!("{:?}", part2);
        println!();
    }

    if args.is_empty() || args.contains(&20) {
        println!("Day 20");
        println!(
            "{:?}",
            day20::part1(DAY20).ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!(
            "{:?}",
            day20::part2(DAY20).ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!();
    }

    if args.is_empty() || args.contains(&21) {
        println!("Day 21");
        println!("{:?}", day21::part1(DAY21).map_err(util::to_ioerror)?);
        println!("{:?}", day21::part2(DAY21).map_err(util::to_ioerror)?);
        println!();
    }

    if args.is_empty() || args.contains(&22) {
        println!("Day 22");
        println!("{:?}", day22::part1(DAY22).map_err(util::to_ioerror)?);
        println!("{:?}", day22::part2(DAY22).map_err(util::to_ioerror)?);
        println!();
    }

    if args.is_empty() || args.contains(&23) {
        println!("Day 23");
        println!(
            "{:?}",
            day23::part1(DAY23).ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!(
            "{:?}",
            day23::part2(DAY23).ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!();
    }

    if args.is_empty() || args.contains(&24) {
        println!("Day 24");
        println!(
            "{:?}",
            day24::part1(DAY24)
                .map_err(util::to_ioerror)?
                .ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!(
            "{:?}",
            day24::part2(DAY24)
                .map_err(util::to_ioerror)?
                .ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!();
    }

    if args.is_empty() || args.contains(&25) {
        println!("Day 25");
        println!(
            "{:?}",
            day25::part1(DAY25).ok_or_else(|| util::to_ioerror(util::Error))?
        );
        println!();
    }

    Ok(())
}
