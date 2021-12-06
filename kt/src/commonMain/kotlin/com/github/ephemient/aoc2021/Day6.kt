package com.github.ephemient.aoc2021

class Day6(lines: List<String>) {
    private val initialState = LongArray(9).apply {
        for (fish in lines.single().splitToSequence(',')) {
            this[fish.toInt()]++
        }
    }

    fun part1(): Long = solve(80)

    fun part2(): Long = solve(256)

    private fun solve(times: Int): Long {
        val state = initialState.copyOf()
        repeat(times) {
            val zero = state[0]
            state.copyInto(state, 0, 1)
            state[6] += zero
            state[8] = zero
        }
        return state.sum()
    }
}
