package com.github.ephemient.aoc2021

import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day9Bench {
    private lateinit var lines: List<String>

    @Setup
    fun prepare() {
        lines = getInput(9)
    }

    @Benchmark
    fun part1(): Int = Day9(lines).part1()

    @Benchmark
    fun part2(): Int = Day9(lines).part2()
}
