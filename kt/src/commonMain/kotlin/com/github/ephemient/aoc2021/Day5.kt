package com.github.ephemient.aoc2021

class Day5(lines: List<String>) {
    private val segments = lines.map { line ->
        val (first, second) = line.split(" -> ", limit = 2).map { word ->
            val (x, y) = word.split(',', limit = 2)
            x.toInt() to y.toInt()
        }
        if (compareValuesBy(first, second, { it.first }, { it.second }) < 0) first to second else second to first
    }

    fun part1(): Int = buildMap {
        for ((start, end) in segments) {
            val (x0, y0) = start
            val (x1, y1) = end
            when {
                x0 == x1 -> for (y in y0..y1) put(x0 to y, getOrElse(x0 to y) { 0 } + 1)
                y0 == y1 -> for (x in x0..x1) put(x to y0, getOrElse(x to y0) { 0 } + 1)
            }
        }
    }.values.count { it > 1 }

    fun part2(): Int = buildMap {
        for ((start, end) in segments) {
            val (x0, y0) = start
            val (x1, y1) = end
            when {
                x0 == x1 -> for (y in y0..y1) put(x0 to y, getOrElse(x0 to y) { 0 } + 1)
                y0 == y1 -> for (x in x0..x1) put(x to y0, getOrElse(x to y0) { 0 } + 1)
                else -> repeat(x1 - x0 + 1) {
                    val xy = x0 + it to if (y0 < y1) y0 + it else y0 - it
                    put(xy, getOrElse(xy) { 0 } + 1)
                }
            }
        }
    }.values.count { it > 1 }
}
