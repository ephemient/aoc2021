package com.github.ephemient.aoc2021

class Day1(lines: List<String>) {
    private val nums = lines.map { it.toInt() }

    fun part1(): Int = (0 until nums.lastIndex).count { nums[it] < nums[it + 1] }

    fun part2(): Int {
        val sums = nums.windowed(3) { it.sum() }
        return (0 until sums.lastIndex).count { sums[it] < sums[it + 1] }
    }
}
