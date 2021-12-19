package com.github.ephemient.aoc2021

import kotlin.test.Test
import kotlin.test.assertEquals

class PermutationsTest {
    @Test
    fun permutationIndices() {
        assertEquals(
            listOf(
                listOf(0, 1, 2) to false,
                listOf(0, 2, 1) to true,
                listOf(1, 0, 2) to true,
                listOf(1, 2, 0) to false,
                listOf(2, 0, 1) to false,
                listOf(2, 1, 0) to true,
            ),
            buildList {
                permutationIndices(3) { indices, parity -> add(indices.toList() to parity) }
            }
        )
    }
}
