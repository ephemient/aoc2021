package com.github.ephemient.aoc2021

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

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

    @Test
    fun permutations() {
        for (str in arrayOf("", "a", "kotlin")) {
            val permutations = str.toList().permutations().toList()
            val set = str.toSet()
            for (permutation in permutations) {
                assertEquals(set, permutation.toSet(), "$str -> ${permutation.joinToString("")}")
            }
            val duplicates = permutations.groupByTo(mutableMapOf()) { it }.values.apply { retainAll { it.size != 1 } }
            assertTrue("$str duplicates: $duplicates") { duplicates.isEmpty() }
            assertEquals((1..str.length).fold(1) { acc, i -> acc * i }, permutations.size, str)
        }
    }
}
