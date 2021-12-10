package com.github.ephemient.aoc2021

class Day10(private val lines: List<String>) {
    fun part1(): Int = lines.sumBy { line ->
        val expected = mutableListOf<Char>()
        for (char in line) {
            pairs[char]?.also { expected += it }
                ?: expected.removeLastOrNull()?.takeIf { it == char }
                ?: return@sumBy part1Points.getValue(char)
        }
        0
    }

    fun part2(): Long = lines.mapNotNullTo(mutableListOf()) { line ->
        val expected = mutableListOf<Char>()
        for (char in line) {
            pairs[char]?.also { expected += it }
                ?: expected.removeLastOrNull()?.takeIf { it == char }
                ?: return@mapNotNullTo null
        }
        expected.asReversed().fold(0L) { acc, char -> 5 * acc + part2Points.getValue(char) }
    }.apply { sort() }.let { it[it.size / 2] }

    companion object {
        private val pairs = "([{<".zip(")]}>").toMap()
        private val part1Points = mapOf(')' to 3, ']' to 57, '}' to 1197, '>' to 25137)
        private val part2Points = mapOf(')' to 1, ']' to 2, '}' to 3, '>' to 4)
    }
}
