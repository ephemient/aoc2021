package com.github.ephemient.aoc2021

/** [Day 9](https://adventofcode.com/2021/day/9): Smoke Basin */
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
        val visited = Array(lines.size) { i ->
            BooleanArray(lines[i].length) { j ->
                val c = lines[i][j]
                !c.isDigit() || c.digitToInt() >= 9
            }
        }
        lines.forEachIndexed { i, line ->
            line.forEachIndexed inner@{ j, c ->
                if (visited[i][j] || !c.isDigit() || c.digitToInt() >= 9) return@inner
                visited[i][j] = true
                var basin = 0
                val stack = mutableListOf(i to j)
                while (true) {
                    val (i2, j2) = stack.removeLastOrNull() ?: break
                    basin++
                    CardinalDirection.forEach(i2, j2) { i3, j3 ->
                        if (i3 in visited.indices && j3 in visited[i3].indices && !visited[i3][j3]) {
                            visited[i3][j3] = true
                            stack.add(i3 to j3)
                        }
                    }
                }
                basins.add(basin)
            }
        }
        basins.sort()
        return basins.asReversed().take(3).fold(1) { x, y -> x * y }
    }
}
