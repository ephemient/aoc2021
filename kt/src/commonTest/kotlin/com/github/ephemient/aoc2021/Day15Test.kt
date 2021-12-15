package com.github.ephemient.aoc2021

import kotlin.test.Test
import kotlin.test.assertEquals

class Day15Test {
    @Test
    fun part1() {
        assertEquals(40, Day15(SAMPLE_INPUT).part1())
    }

    @Test
    fun part2() {
        assertEquals(315, Day15(SAMPLE_INPUT).part2())
    }

    companion object {
        private val SAMPLE_INPUT = listOf(
            "1163751742",
            "1381373672",
            "2136511328",
            "3694931569",
            "7463417111",
            "1319128137",
            "1359912421",
            "3125421639",
            "1293138521",
            "2311944581",
        )
    }
}
