package com.github.ephemient.aoc2021

import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day24Bench {
    private lateinit var lines: List<String>

    @Setup
    fun prepare() {
        lines = getInput(24)
    }

    @Benchmark
    fun part1(): Long? = Day24(lines).part1()

    @Benchmark
    fun part2(): Long? = Day24(lines).part2()
}
