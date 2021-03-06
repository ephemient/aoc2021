package com.github.ephemient.aoc2021

import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day17Bench {
    private lateinit var lines: List<String>

    @Setup
    fun prepare() {
        lines = getInput(17)
    }

    @Benchmark
    fun part1(): IntPair = Day17(lines).solve()
}
