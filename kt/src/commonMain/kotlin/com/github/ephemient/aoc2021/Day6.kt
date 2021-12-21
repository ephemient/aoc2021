package com.github.ephemient.aoc2021

/** [Day 6](https://adventofcode.com/2021/day/6): Lanternfish */
class Day6(lines: List<String>) {
    private val nums = lines.flatMap { line -> line.splitToSequence(',').map { it.toInt() } }

    fun part1(): Long = nums.sumOf { Day6Constants.matrix80[it] }

    fun part2(): Long = nums.sumOf { Day6Constants.matrix256[it] }
}
