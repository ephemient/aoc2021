package com.github.ephemient.aoc2021

import kotlin.test.Test
import kotlin.test.assertEquals

class Day2Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(150, Day2(SAMPLE_INPUT).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(900, Day2(SAMPLE_INPUT).part2())
    }

    companion object {
        private val SAMPLE_INPUT = listOf("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")
    }
}
