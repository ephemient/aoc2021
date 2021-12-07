package com.github.ephemient.aoc2021

import kotlin.test.Test
import kotlin.test.assertEquals

class Day7Test {
    @Test
    fun part1() {
        assertEquals(37, Day7(SAMPLE_INPUT).part1())
    }

    @Test
    fun part2() {
        assertEquals(168, Day7(SAMPLE_INPUT).part2())
    }

    companion object {
        private val SAMPLE_INPUT = listOf("16,1,2,0,4,2,7,1,2,14")
    }
}
