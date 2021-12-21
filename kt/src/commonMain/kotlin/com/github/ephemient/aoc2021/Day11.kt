package com.github.ephemient.aoc2021

/** [Day 11](https://adventofcode.com/2021/day/11): Dumbo Octopus */
class Day11(lines: List<String>) {
    private val flashes = buildList {
        val width = lines.first().length
        val arr = buildList {
            for (line in lines) {
                check(line.length == width)
                line.mapTo(this) { it.digitToInt() }
            }
        }.toIntArray()
        while (arr.any { it != 0 }) {
            arr.forEachIndexed { i, x -> arr[i] = x + 1 }
            var flashes = 0
            while (true) {
                val i = arr.indexOfFirst { it > 9 }
                if (i < 0) break
                flashes++
                val x0 = i % width
                val y0 = i / width
                for (x1 in (x0 - 1).coerceAtLeast(0)..(x0 + 1).coerceAtMost(width - 1)) {
                    for (y1 in (y0 - 1).coerceAtLeast(0)..(y0 + 1).coerceAtMost(lines.lastIndex)) {
                        val j = x1 + y1 * width
                        if (i == j) {
                            arr[j] = -1
                        } else {
                            val n = arr[j]
                            if (n >= 0) arr[j] = n + 1
                        }
                    }
                }
            }
            add(flashes)
            arr.forEachIndexed { i, x -> if (x < 0) arr[i] = 0 }
        }
    }

    fun part1(): Int = flashes.take(100).sum()

    fun part2(): Int = flashes.size
}
