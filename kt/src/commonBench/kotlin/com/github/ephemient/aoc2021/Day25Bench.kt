package com.github.ephemient.aoc2021

import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day25Bench {
    private lateinit var lines: List<String>

    @Setup
    fun prepare() {
        lines = getInput(25)
    }

    @Benchmark
    fun part1(): Int = Day25(lines).part1()
}
