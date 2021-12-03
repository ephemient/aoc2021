package com.github.ephemient.aoc2021

import kotlin.test.Test
import kotlin.test.assertEquals

class Day1Test {
    @Test
    fun part1() {
        assertEquals(7, Day1(SAMPLE_INPUT).part1())
    }

    @Test
    fun part2() {
        assertEquals(5, Day1(SAMPLE_INPUT).part2())
    }

    companion object {
        private val SAMPLE_INPUT = listOf("199", "200", "208", "210", "200", "207", "240", "269", "260", "263")
    }
}
