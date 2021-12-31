package com.github.ephemient.aoc2021

import kotlin.test.Test
import kotlin.test.assertEquals

@ExperimentalMultiplatform
@JvmIgnore
class Day24Test {
    @Test
    @JsIgnore
    fun part1Common() {
        assertEquals(99999993811817, Day24Common(SAMPLE_INPUT).part1())
    }

    @Test
    fun part1Range() {
        assertEquals(99999993811817, Day24Range(SAMPLE_INPUT).part1())
    }

    @Test
    @JsIgnore
    fun part2Common() {
        assertEquals(11111128657365, Day24Common(SAMPLE_INPUT).part2())
    }

    @Test
    fun part2Range() {
        assertEquals(11111128657365, Day24Range(SAMPLE_INPUT).part2())
    }

    companion object {
        val SAMPLE_INPUT = listOf(
            "inp w",
            "mul z 31",
            "add z w",
            "mod z 16777216",
            "inp w",
            "mul z 31",
            "add z w",
            "mod z 16777216",
            "inp w",
            "mul z 31",
            "add z w",
            "mod z 16777216",
            "inp w",
            "mul z 31",
            "add z w",
            "mod z 16777216",
            "inp w",
            "mul z 31",
            "add z w",
            "mod z 16777216",
            "inp w",
            "mul z 31",
            "add z w",
            "mod z 16777216",
            "inp w",
            "mul z 31",
            "add z w",
            "mod z 16777216",
            "inp w",
            "mul z 31",
            "add z w",
            "mod z 16777216",
            "inp w",
            "mul z 31",
            "add z w",
            "mod z 16777216",
            "inp w",
            "mul z 31",
            "add z w",
            "mod z 16777216",
            "inp w",
            "mul z 31",
            "add z w",
            "mod z 16777216",
            "inp w",
            "mul z 31",
            "add z w",
            "mod z 16777216",
            "inp w",
            "mul z 31",
            "add z w",
            "mod z 16777216",
            "inp w",
            "mul z 31",
            "add z w",
            "mod z 16777216",
        )
    }
}
