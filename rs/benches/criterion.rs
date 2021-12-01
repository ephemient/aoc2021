#[macro_use]
extern crate build_const;

use aoc2021::day1;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

build_const!("aoc2021");

fn aoc2021_bench(c: &mut Criterion) {
    c.bench_function("day 1 part 1", |b| b.iter(|| day1::part1(black_box(DAY1))));
    c.bench_function("day 1 part 2", |b| b.iter(|| day1::part2(black_box(DAY1))));
}

criterion_group!(aoc2021, aoc2021_bench);
criterion_main!(aoc2021);
