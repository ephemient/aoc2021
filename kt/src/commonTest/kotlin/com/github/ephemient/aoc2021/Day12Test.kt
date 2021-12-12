package com.github.ephemient.aoc2021

import kotlin.test.Test
import kotlin.test.assertEquals

class Day12Test {
    @Test
    fun part1() {
        assertEquals(10, Day12(SAMPLE_INPUT_1).part1())
        assertEquals(19, Day12(SAMPLE_INPUT_2).part1())
        assertEquals(226, Day12(SAMPLE_INPUT_3).part1())
    }

    @Test
    fun part2() {
        assertEquals(36, Day12(SAMPLE_INPUT_1).part2())
        assertEquals(103, Day12(SAMPLE_INPUT_2).part2())
        assertEquals(3509, Day12(SAMPLE_INPUT_3).part2())
    }

    companion object {
        private val SAMPLE_INPUT_1 = listOf(
            "start-A",
            "start-b",
            "A-c",
            "A-b",
            "b-d",
            "A-end",
            "b-end",
        )
        private val SAMPLE_INPUT_2 = listOf(
            "dc-end",
            "HN-start",
            "start-kj",
            "dc-start",
            "dc-HN",
            "LN-dc",
            "HN-end",
            "kj-sa",
            "kj-HN",
            "kj-dc",
        )
        private val SAMPLE_INPUT_3 = listOf(
            "fs-end",
            "he-DX",
            "fs-he",
            "start-DX",
            "pj-DX",
            "end-zg",
            "zg-sl",
            "zg-pj",
            "pj-he",
            "RW-he",
            "fs-DX",
            "pj-RW",
            "zg-RW",
            "start-pj",
            "he-WI",
            "zg-he",
            "pj-fs",
            "start-RW",
        )
    }
}
