package com.github.ephemient.aoc2021

/** Day 2: Dive! */
class Day2(private val lines: List<String>) {
    fun part1(): Int {
        var x = 0
        var y = 0
        for (line in lines) {
            val m = requireNotNull(PATTERN.matchEntire(line)) { line }
            m.groups[1]?.value?.let { x += it.toInt() }
                ?: m.groups[2]?.value?.let { y += it.toInt() }
                ?: m.groups[3]?.value?.let { y -= it.toInt() }
                ?: throw IllegalArgumentException(line)
        }
        return x * y
    }

    fun part2(): Int {
        var x = 0
        var y = 0
        var depth = 0
        for (line in lines) {
            val m = requireNotNull(PATTERN.matchEntire(line)) { line }
            m.groups[1]?.value?.let {
                val d = it.toInt()
                x += d
                y += d * depth
            }
                ?: m.groups[2]?.value?.let { depth += it.toInt() }
                ?: m.groups[3]?.value?.let { depth -= it.toInt() }
                ?: throw IllegalArgumentException(line)
        }
        return x * y
    }

    companion object {
        private val PATTERN = """forward (\d+)|down (\d+)|up (\d+)""".toRegex()
    }
}
