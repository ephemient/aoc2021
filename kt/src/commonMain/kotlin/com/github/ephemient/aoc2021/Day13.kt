package com.github.ephemient.aoc2021

import kotlin.math.abs

/** [Day 13](https://adventofcode.com/2021/day/13): Transparent Origami */
class Day13(lines: List<String>) {
    private val points: Set<IntPair>
    private val folds: List<Fold>
    init {
        val iterator = lines.iterator()
        points = buildSet {
            for (line in iterator) {
                if (line.isEmpty()) break
                val (x, y) = line.split(',')
                add(x.toInt() to y.toInt())
            }
        }
        folds = buildList {
            for (line in iterator) {
                when {
                    line.startsWith("fold along x=") -> add(Fold.X(line.substring(13).toInt()))
                    line.startsWith("fold along y=") -> add(Fold.Y(line.substring(13).toInt()))
                    else -> throw IllegalStateException("bad input: $line")
                }
            }
        }
    }

    fun part1(): Int = points.mapTo(mutableSetOf(), folds.first()::invoke).size

    fun part2(): String {
        val points = points.mapTo(mutableSetOf()) { folds.fold(it) { point, fold -> fold(point) } }
        val x0 = points.minOf { it.first }
        val y0 = points.minOf { it.second }
        val x1 = points.maxOf { it.first }
        val y1 = points.maxOf { it.second }
        return (y0..y1).joinToString("\n") { y ->
            (x0..x1).joinToString("") { x ->
                if (x to y in points) "\u2593" else "\u2591"
            }
        }
    }

    private sealed interface Fold {
        operator fun invoke(point: IntPair): IntPair

        class X(private val x: Int) : Fold {
            override fun invoke(point: IntPair): IntPair = point.copy(first = x - abs(point.first - x))
        }

        class Y(private val y: Int) : Fold {
            override fun invoke(point: IntPair): IntPair = point.copy(second = y - abs(point.second - y))
        }
    }
}
