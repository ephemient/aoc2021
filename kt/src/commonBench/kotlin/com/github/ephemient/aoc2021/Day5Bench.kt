package com.github.ephemient.aoc2021

import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day5Bench {
    private lateinit var lines: List<String>

    @Setup
    fun prepare() {
        lines = getInput(5)
    }

    @Benchmark
    fun part1(): Int = Day5(lines).part1()

    @Benchmark
    fun part2(): Int = Day5(lines).part2()
}
