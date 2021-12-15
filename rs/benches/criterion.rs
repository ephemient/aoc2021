#[macro_use]
extern crate build_const;

use aoc2021::{
    day1, day10, day11, day12, day13, day14, day15, day2, day3, day4, day5, day6, day7, day8, day9,
};
use criterion::{black_box, criterion_group, criterion_main, Criterion};

build_const!("aoc2021");

fn aoc2021_bench(c: &mut Criterion) {
    c.bench_function("day 1 part 1", |b| b.iter(|| day1::part1(black_box(DAY1))));
    c.bench_function("day 1 part 2", |b| b.iter(|| day1::part2(black_box(DAY1))));
    c.bench_function("day 2 part 1", |b| b.iter(|| day2::part1(black_box(DAY2))));
    c.bench_function("day 2 part 2", |b| b.iter(|| day2::part2(black_box(DAY2))));
    c.bench_function("day 3 part 1", |b| b.iter(|| day3::part1(black_box(DAY3))));
    c.bench_function("day 3 part 2", |b| b.iter(|| day3::part2(black_box(DAY3))));
    c.bench_function("day 4 solve", |b| b.iter(|| day4::solve(black_box(DAY4))));
    c.bench_function("day 5 part 1", |b| b.iter(|| day5::part1(black_box(DAY5))));
    c.bench_function("day 5 part 2", |b| b.iter(|| day5::part2(black_box(DAY5))));
    c.bench_function("day 6 part 1", |b| b.iter(|| day6::part1(black_box(DAY6))));
    c.bench_function("day 6 part 2", |b| b.iter(|| day6::part2(black_box(DAY6))));
    c.bench_function("day 7 part 1", |b| b.iter(|| day7::part1(black_box(DAY7))));
    c.bench_function("day 7 part 2", |b| b.iter(|| day7::part2(black_box(DAY7))));
    c.bench_function("day 8 part 1", |b| b.iter(|| day8::part1(black_box(DAY8))));
    c.bench_function("day 8 part 2", |b| b.iter(|| day8::part2(black_box(DAY8))));
    c.bench_function("day 9 part 1", |b| b.iter(|| day9::part1(black_box(DAY9))));
    c.bench_function("day 9 part 2", |b| b.iter(|| day9::part2(black_box(DAY9))));
    c.bench_function("day 10 part 1", |b| {
        b.iter(|| day10::part1(black_box(DAY10)))
    });
    c.bench_function("day 10 part 2", |b| {
        b.iter(|| day10::part2(black_box(DAY10)))
    });
    c.bench_function("day 11 solve", |b| {
        b.iter(|| day11::solve(black_box(DAY11)))
    });
    c.bench_function("day 12 part 1", |b| {
        b.iter(|| day12::part1(black_box(DAY12)))
    });
    c.bench_function("day 12 part 2", |b| {
        b.iter(|| day12::part2(black_box(DAY12)))
    });
    c.bench_function("day 13 part 1", |b| {
        b.iter(|| day13::part1(black_box(DAY13)))
    });
    c.bench_function("day 13 part 2", |b| {
        b.iter(|| day13::part2(black_box(DAY13)))
    });
    c.bench_function("day 14 part 1", |b| {
        b.iter(|| day14::part1(black_box(DAY14)))
    });
    c.bench_function("day 14 part 2", |b| {
        b.iter(|| day14::part2(black_box(DAY14)))
    });
    c.bench_function("day 15 part 1", |b| {
        b.iter(|| day15::part1(black_box(DAY15)))
    });
    c.bench_function("day 15 part 2", |b| {
        b.iter(|| day15::part2(black_box(DAY15)))
    });
}

criterion_group!(aoc2021, aoc2021_bench);
criterion_main!(aoc2021);
