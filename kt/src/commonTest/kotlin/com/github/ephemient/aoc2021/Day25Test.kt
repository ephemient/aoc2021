package com.github.ephemient.aoc2021

import kotlin.test.Test
import kotlin.test.assertEquals

class Day25Test {
    @Test
    fun part1() {
        assertEquals(58, Day25(SAMPLE_INPUT).part1())
    }

    companion object {
        private val SAMPLE_INPUT = listOf(
            "v...>>.vv>",
            ".vv>>.vv..",
            ">>.>v>...v",
            ">>v>>.>.v.",
            "v>v.vv.v..",
            ">.>>..v...",
            ".vv..>.>v.",
            "v.v..>>v.v",
            "....v..v.>",
        )
    }
}
