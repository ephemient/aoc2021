package com.github.ephemient.aoc2021

import kotlin.test.Test
import kotlin.test.assertEquals

class Day17Test {
    @Test
    fun part1() {
        assertEquals(45, Day17(SAMPLE_INPUT).part1())
    }

    @Test
    fun part2() {
        assertEquals(112, Day17(SAMPLE_INPUT).part2())
    }

    companion object {
        private val SAMPLE_INPUT = listOf("target area: x=20..30, y=-10..-5")
    }
}
