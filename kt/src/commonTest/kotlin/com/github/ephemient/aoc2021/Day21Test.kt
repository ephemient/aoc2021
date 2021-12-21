package com.github.ephemient.aoc2021

import kotlin.test.Test
import kotlin.test.assertEquals

class Day21Test {
    @Test
    fun part1() {
        assertEquals(739785, Day21(SAMPLE_INPUT).part1())
    }

    @Test
    fun part2() {
        assertEquals(444356092776315, Day21(SAMPLE_INPUT).part2())
    }

    companion object {
        private val SAMPLE_INPUT = listOf(
            "Player 1 starting position: 4",
            "Player 2 starting position: 8",
        )
    }
}
