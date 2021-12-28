package com.github.ephemient.aoc2021

import kotlin.test.Test
import kotlin.test.assertEquals

@ExperimentalMultiplatform
@JsIgnore
class Day23Test {
    @Test
    fun part1() {
        assertEquals(12521, Day23(SAMPLE_INPUT).part1())
    }

    @Test
    fun part2() {
        assertEquals(44169, Day23(SAMPLE_INPUT).part2())
    }

    companion object {
        private val SAMPLE_INPUT = listOf(
            "#############",
            "#...........#",
            "###B#C#B#D###",
            "  #A#D#C#A#",
            "  #########",
        )
    }
}
