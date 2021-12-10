package com.github.ephemient.aoc2021

import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day10Bench {
    private lateinit var lines: List<String>

    @Setup
    fun prepare() {
        lines = getInput(10)
    }

    @Benchmark
    fun part1(): Int = Day10(lines).part1()

    @Benchmark
    fun part2(): Long = Day10(lines).part2()
}
