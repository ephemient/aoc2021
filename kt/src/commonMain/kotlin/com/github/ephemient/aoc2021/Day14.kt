package com.github.ephemient.aoc2021

class Day14(lines: List<String>) {
    private val initial = lines.first()
    init {
        check(lines[1].isEmpty() && initial.length >= 2)
    }
    private val rules = lines.drop(2).associate { line ->
        val (lhs, rhs) = line.split(" -> ", limit = 2)
        check(lhs.length == 2 && rhs.length == 1)
        lhs to listOf("${lhs.first()}$rhs", "$rhs${lhs.last()}")
    }

    fun part1(): Long = score(10)

    fun part2(): Long = score(40)

    private fun score(steps: Int): Long {
        var state = initial.zip(initial.drop(1)) { a, b -> "$a$b" }
            .groupingBy { it }.eachCount().mapValues { it.value.toLong() }
        repeat(steps) {
            state = buildMap {
                for ((src, n) in state) {
                    for (dst in rules.getValue(src)) {
                        put(dst, getOrElse(dst) { 0 } + n)
                    }
                }
            }
        }
        val counts = buildMap<Char, Long> {
            put(initial.first(), 1)
            put(initial.last(), getOrElse(initial.last()) { 0 } + 1)
            for ((pair, n) in state) {
                for (c in pair) {
                    put(c, getOrElse(c) { 0 } + n)
                }
            }
        }
        return (counts.values.maxOrNull()!! - counts.values.minOrNull()!!) / 2
    }
}
