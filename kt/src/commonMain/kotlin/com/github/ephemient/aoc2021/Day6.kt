package com.github.ephemient.aoc2021

class Day6(lines: List<String>) {
    private val initialState = LongArray(9).apply {
        for (fish in lines.single().splitToSequence(',')) {
            this[fish.toInt()]++
        }
    }

    fun part1(): Long = Day6Constants.matrix80.zip(initialState, Long::times).sum()

    fun part2(): Long = Day6Constants.matrix256.zip(initialState, Long::times).sum()
}
