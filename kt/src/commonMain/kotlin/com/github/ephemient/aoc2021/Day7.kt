package com.github.ephemient.aoc2021

import kotlin.math.abs
import kotlin.math.roundToInt

class Day7(lines: List<String>) {
    private val nums = lines.flatMap { line -> line.splitToSequence(',').map { it.toInt() } }

    fun part1(): Int {
        val median = nums.sorted()[nums.size / 2]
        return nums.sumOf { abs(it - median) }
    }

    fun part2(): Int {
        val mean = nums.sum().toDouble() / nums.size
        val mean0 = mean.roundToInt()
        val mean1 = if (mean0 <= mean) mean0 + 1 else mean0 - 1
        return minOf(nums.sumOf { t(abs(it - mean0)) }, nums.sumOf { t(abs(it - mean1)) })
    }

    companion object {
        private fun t(x: Int): Int = x * (x + 1) / 2
    }
}
