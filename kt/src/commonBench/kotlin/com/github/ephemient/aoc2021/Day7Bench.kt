package com.github.ephemient.aoc2021

import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day7Bench {
    private lateinit var lines: List<String>

    @Setup
    fun prepare() {
        lines = getInput(7)
    }

    @Benchmark
    fun part1(): Int = Day7(lines).part1()

    @Benchmark
    fun part2(): Int = Day7(lines).part2()
}
