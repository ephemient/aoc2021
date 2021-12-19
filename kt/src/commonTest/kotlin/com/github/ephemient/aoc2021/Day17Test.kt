package com.github.ephemient.aoc2021

import kotlin.test.Test
import kotlin.test.assertEquals

class Day17Test {
    @Test
    fun solve() {
        assertEquals(45 to 112, Day17(SAMPLE_INPUT).solve())
    }

    companion object {
        private val SAMPLE_INPUT = listOf("target area: x=20..30, y=-10..-5")
    }
}
