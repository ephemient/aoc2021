package com.github.ephemient.aoc2021

import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day14Bench {
    private lateinit var lines: List<String>

    @Setup
    fun prepare() {
        lines = getInput(14)
    }

    @Benchmark
    fun part1(): Long = Day14(lines).part1()

    @Benchmark
    fun part2(): Long = Day14(lines).part2()
}
