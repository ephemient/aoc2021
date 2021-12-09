package com.github.ephemient.aoc2021

import kotlin.test.Test
import kotlin.test.assertEquals

class Day9Test {
    @Test
    fun part1() {
        assertEquals(15, Day9(SAMPLE_INPUT).part1())
    }

    @Test
    fun part2() {
        assertEquals(1134, Day9(SAMPLE_INPUT).part2())
    }

    companion object {
        private val SAMPLE_INPUT = listOf("2199943210", "3987894921", "9856789892", "8767896789", "9899965678")
    }
}
