package com.github.ephemient.aoc2021

import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day18Bench {
    private lateinit var lines: List<String>

    @Setup
    fun prepare() {
        lines = getInput(18)
    }

    @Benchmark
    @ExperimentalStdlibApi
    fun part1(): Int = Day18(lines).part1()

    @Benchmark
    @ExperimentalStdlibApi
    fun part2(): Int = Day18(lines).part2()
}
