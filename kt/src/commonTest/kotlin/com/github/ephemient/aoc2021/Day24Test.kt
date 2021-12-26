package com.github.ephemient.aoc2021

import kotlin.test.Test
import kotlin.test.assertEquals

class Day24Test {
    @Test
    fun part1() {
        assertEquals(99999993811817, Day24(SAMPLE_INPUT).part1())
    }

    @Test
    fun part2() {
        assertEquals(11111128657365, Day24(SAMPLE_INPUT).part2())
    }

    companion object {
        private val SAMPLE_INPUT = listOf(
            "inp w",
            "mul z 31",
            "add z w",
            "inp w",
            "mul z 31",
            "add z w",
            "inp w",
            "mul z 31",
            "add z w",
            "inp w",
            "mul z 31",
            "add z w",
            "inp w",
            "mul z 31",
            "add z w",
            "inp w",
            "mul z 31",
            "add z w",
            "inp w",
            "mul z 31",
            "add z w",
            "inp w",
            "mul z 31",
            "add z w",
            "inp w",
            "mul z 31",
            "add z w",
            "inp w",
            "mul z 31",
            "add z w",
            "inp w",
            "mul z 31",
            "add z w",
            "inp w",
            "mul z 31",
            "add z w",
            "inp w",
            "mul z 31",
            "add z w",
            "inp w",
            "mul z 31",
            "add z w",
            "mod z 16777216",
        )
    }
}
