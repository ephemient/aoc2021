package com.github.ephemient.aoc2021

import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day21Bench {
    private lateinit var lines: List<String>

    @Setup
    fun prepare() {
        lines = getInput(21)
    }

    @Benchmark
    fun part1(): Int = Day21(lines).part1()

    @Benchmark
    fun part2(): Long = Day21(lines).part2()
}
