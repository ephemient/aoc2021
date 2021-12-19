package com.github.ephemient.aoc2021

import kotlin.math.ceil
import kotlin.math.roundToInt
import kotlin.math.sqrt

/** Day 17: Trick Shot */
class Day17(lines: List<String>) {
    private val xRange: IntRange
    private val yRange: IntRange
    init {
        val (xRange, yRange) = PATTERN.matchEntire(lines.single())!!.destructured.let { (x0, x1, y0, y1) ->
            (x0.toInt()..x1.toInt()) to (y0.toInt()..y1.toInt())
        }
        this.xRange = xRange
        this.yRange = yRange
    }

    @Suppress("NestedBlockDepth")
    fun solve(): IntPair {
        var maxT = 0
        val dyHits = mutableMapOf<Int, MutableList<Int>>()
        for (dy in yRange.first..-yRange.first) {
            for ((t, y) in generateSequence(dy) { it - 1 }.runningFold(0, Int::plus).withIndex()) {
                if (y < yRange.first) break
                if (y in yRange) {
                    maxT = maxOf(maxT, t)
                    dyHits.getOrPut(t) { mutableListOf() }.add(dy)
                }
            }
        }
        var maxDy = 0
        var count = 0
        for (dx in ceil(sqrt(2.0 * xRange.first + 0.25) - 0.5).roundToInt()..xRange.last) {
            val dys = mutableSetOf<Int>()
            for ((t, x) in generateSequence(dx) { (it - 1).coerceAtLeast(0) }.runningFold(0, Int::plus).withIndex()) {
                if (t > maxT || x > xRange.last) break
                if (x in xRange) dyHits[t]?.let { dys.addAll(it) }
            }
            maxDy = dys.fold(maxDy, ::maxOf)
            count += dys.size
        }
        return maxDy * (maxDy + 1) / 2 to count
    }

    companion object {
        private val PATTERN = """target area: x=(\d+)\.\.(\d+), y=(-\d+)\.\.(-\d+)""".toRegex()
    }
}
