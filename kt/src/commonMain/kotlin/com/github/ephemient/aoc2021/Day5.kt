package com.github.ephemient.aoc2021

import kotlin.math.abs
import kotlin.math.sign

class Day5(lines: List<String>) {
    private val segments = lines.map { line ->
        val (start, end) = line.split(" -> ", limit = 2)
        val (x0, y0) = start.split(',', limit = 2)
        val (x1, y1) = end.split(',', limit = 2)
        Pair(x0.toInt() to y0.toInt(), x1.toInt() to y1.toInt())
    }

    fun part1(): Int = buildMap {
        for ((start, end) in segments) {
            val (x0, y0) = start
            val (x1, y1) = end
            when {
                x0 == x1 -> for (y in minOf(y0, y1)..maxOf(y0, y1)) put(x0 to y, getOrElse(x0 to y) { 0 } + 1)
                y0 == y1 -> for (x in minOf(x0, x1)..maxOf(x0, x1)) put(x to y0, getOrElse(x to y0) { 0 } + 1)
            }
        }
    }.values.count { it > 1 }

    fun part2(): Int = buildMap {
        for ((start, end) in segments) {
            val (x0, y0) = start
            val (x1, y1) = end
            when {
                x0 == x1 -> for (y in minOf(y0, y1)..maxOf(y0, y1)) put(x0 to y, getOrElse(x0 to y) { 0 } + 1)
                y0 == y1 -> for (x in minOf(x0, x1)..maxOf(x0, x1)) put(x to y0, getOrElse(x to y0) { 0 } + 1)
                abs(x1 - x0) == abs(y1 - y0) -> {
                    val dx = (x1 - x0).sign
                    val dy = (y1 - y0).sign
                    repeat(abs(x1 - x0)) {
                        val xy = x0 + it * dx to y0 + it * dy
                        put(xy, getOrElse(xy) { 0 } + 1)
                    }
                }
            }
        }
    }.values.count { it > 1 }
}
