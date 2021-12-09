package com.github.ephemient.aoc2021

class Day9(private val lines: List<String>) {
    fun part1(): Int = lines.withIndex().sumOf { (i, line) ->
        line.withIndex().sumOf { (j, c) ->
            @Suppress("ComplexCondition")
            if (
                c.isDigit() &&
                line.getOrNull(j - 1)?.let { c < it } != false &&
                line.getOrNull(j + 1)?.let { c < it } != false &&
                lines.getOrNull(i - 1)?.getOrNull(j)?.let { c < it } != false &&
                lines.getOrNull(i + 1)?.getOrNull(j)?.let { c < it } != false
            ) c.digitToInt() + 1 else 0
        }
    }

    fun part2(): Int {
        val basins = mutableListOf<Int>()
        val visited = Array(lines.size) { BooleanArray(lines[it].length) }
        lines.forEachIndexed { i, line ->
            line.forEachIndexed inner@{ j, c ->
                if (visited[i][j] || !c.isDigit() || c.digitToInt() >= 9) return@inner
                val basin = mutableSetOf(i to j)
                val queue = ArrayDeque(listOf(i to j))
                while (true) {
                    val (i2, j2) = queue.removeFirstOrNull() ?: break
                    visited[i2][j2] = true
                    CardinalDirection.forEach(i2, j2) { i3, j3 ->
                        val d = lines.getOrNull(i3)?.getOrNull(j3) ?: return@forEach
                        if (d.isDigit() && d.digitToInt() < 9 && basin.add(i3 to j3)) queue.add(i3 to j3)
                    }
                }
                basins.add(basin.size)
            }
        }
        basins.sort()
        return basins.asReversed().take(3).fold(1) { x, y -> x * y }
    }
}
