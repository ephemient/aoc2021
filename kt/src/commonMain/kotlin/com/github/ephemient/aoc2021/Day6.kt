package com.github.ephemient.aoc2021

class Day6(lines: List<String>) {
    private val nums = lines.flatMap { line -> line.splitToSequence(',').map { it.toInt() } }

    fun part1(): Long = nums.sumOf { Day6Constants.matrix80[it] }

    fun part2(): Long = nums.sumOf { Day6Constants.matrix256[it] }
}
