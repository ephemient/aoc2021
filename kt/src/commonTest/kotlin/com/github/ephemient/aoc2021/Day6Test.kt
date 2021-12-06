package com.github.ephemient.aoc2021

import kotlin.test.Test
import kotlin.test.assertEquals

class Day6Test {
    @Test
    fun part1() {
        assertEquals(5934, Day6(SAMPLE_INPUT).part1())
    }

    @Test
    fun part2() {
        assertEquals(26984457539, Day6(SAMPLE_INPUT).part2())
    }

    companion object {
        private val SAMPLE_INPUT = listOf("3,4,3,1,2")
    }
}
