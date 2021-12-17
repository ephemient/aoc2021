package com.github.ephemient.aoc2021

import kotlin.math.abs

/** Day 17: Trick Shot */
class Day17(lines: List<String>) {
    private val part1: Int
    private val part2: Int
    init {
        val (xRange, yRange) = PATTERN.matchEntire(lines.single())!!.destructured.let { (x0, x1, y0, y1) ->
            (x0.toInt()..x1.toInt()) to (y0.toInt()..y1.toInt())
        }
        var maxDy = 0
        var count = 0
        for (dx in 0..xRange.last) {
            for (dy in yRange.first.coerceAtMost(0)..maxOf(abs(yRange.first), abs(yRange.last))) {
                var x = 0
                var y = 0
                var effectiveDx = dx
                var effectiveDy = dy
                while (x <= xRange.last && (effectiveDy > 0 || y >= yRange.first)) {
                    if (x in xRange && y in yRange) {
                        maxDy = maxOf(maxDy, dy)
                        count++
                        break
                    }
                    if (effectiveDx > 0) {
                        x += effectiveDx
                        effectiveDx--
                    }
                    y += effectiveDy
                    effectiveDy--
                }
            }
        }
        part1 = maxDy * (maxDy + 1) / 2
        part2 = count
    }

    fun part1(): Int = part1

    fun part2(): Int = part2

    companion object {
        private val PATTERN = """target area: x=(\d+)\.\.(\d+), y=(-\d+)\.\.(-\d+)""".toRegex()
    }
}
