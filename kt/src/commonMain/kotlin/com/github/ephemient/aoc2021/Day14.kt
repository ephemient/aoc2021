package com.github.ephemient.aoc2021

/** Day 14: Extended Polymerization */
class Day14(lines: List<String>) {
    private val n: Int
    private val x: LongArray
    private val m: IntArray
    init {
        check(lines.size > 1 && lines[0].length > 1 && lines[1].isEmpty())
        val chars = StringBuilder()
        for (char in lines[0]) if (char !in chars) chars.append(char)
        for (line in lines.drop(2)) {
            check(line.length == 7 && line.regionMatches(2, " -> ", 0, 4))
            if (line[0] !in chars) chars.append(line[0])
            if (line[1] !in chars) chars.append(line[1])
            if (line.last() !in chars) chars.append(line.last())
        }
        n = chars.length
        x = LongArray(n * n)
        for (i in 0 until lines[0].lastIndex) x[chars.indexOf(lines[0][i]) * n + chars.indexOf(lines[0][i + 1])]++
        m = IntArray(n * n) { -1 }
        for (line in lines.drop(2)) m[chars.indexOf(line[0]) * n + chars.indexOf(line[1])] = chars.indexOf(line.last())
    }

    fun part1(): Long = score(10)

    fun part2(): Long = score(40)

    private fun score(steps: Int): Long {
        var x = x
        repeat(steps) {
            val y = LongArray(n * n)
            x.forEachIndexed { i, j ->
                val z = m[i]
                check(z >= 0)
                y[i / n * n + z] += j
                y[z * n + i % n] += j
            }
            x = y
        }
        val counts = LongArray(n)
        counts[0] = 1
        x.forEachIndexed { i, j -> counts[i % n] += j }
        return counts.maxOf { it } - counts.minOf { it }
    }
}
