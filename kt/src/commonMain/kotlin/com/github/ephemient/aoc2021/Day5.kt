package com.github.ephemient.aoc2021

class Day5(lines: List<String>) {
    private val segments = lines.map { line ->
        val (first, second) = line.split(" -> ", limit = 2).map { word ->
            val (x, y) = word.split(',', limit = 2)
            Point(x.toInt(), y.toInt())
        }
        if (first < second) Segment(first, second) else Segment(second, first)
    }

    fun part1(): Int = intersections(segments.filter { it.x0 == it.x1 || it.y0 == it.y1 })

    fun part2(): Int = intersections(segments)

    private data class Point(val x: Int, val y: Int) : Comparable<Point> {
        override fun compareTo(other: Point): Int = compareValuesBy(this, other, { it.x }, { it.y })
    }

    private data class Segment(val x0: Int, val y0: Int, val x1: Int, val y1: Int) {
        constructor(first: Point, second: Point) : this(first.x, first.y, second.x, second.y)

        private val m: Int get() = (y1 - y0) / (x1 - x0)
        private val a: Int get() = y0 - x0 * m
        private val xs: IntRange get() = x0..x1
        private val ys: IntRange get() = if (y0 < y1) y0..y1 else y1..y0
        private fun get(x: Int): Int = (y1 - y0) / (x1 - x0) * (x - x0) + y0

        @Suppress("ComplexMethod", "NestedBlockDepth")
        fun intersectTo(dest: MutableCollection<Point>, other: Segment) {
            if (x0 == x1) {
                if (other.x0 == other.x1) {
                    if (x0 == other.x0) for (y in ys intersect other.ys) dest.add(Point(x0, y))
                } else if (x0 in other.xs) {
                    val y = other.get(x0)
                    if (y in ys) dest.add(Point(x0, y))
                }
            } else if (other.x0 == other.x1) {
                if (other.x0 in xs) {
                    val y = get(other.x0)
                    if (y in other.ys) dest.add(Point(other.x0, y))
                }
            } else if ((y1 - y0) / (x1 - x0) == (other.y1 - other.y0) / (other.x1 - other.x0)) {
                if (a == other.a) {
                    for (x in xs intersect other.xs) dest.add(Point(x, get(x)))
                }
            } else {
                val da = a - other.a
                val dm = m - other.m
                if (da % dm == 0) {
                    val x = -da / dm
                    if (x in xs && x in other.xs) dest.add(Point(x, get(x)))
                }
            }
        }
    }

    companion object {
        private fun intersections(segments: List<Segment>): Int = buildSet {
            segments.forEachIndexed { i, segment0 ->
                for (segment1 in segments.drop(i + 1)) {
                    segment0.intersectTo(this, segment1)
                }
            }
        }.size

        private infix fun IntRange.intersect(other: IntRange): IntRange =
            maxOf(first, other.first)..minOf(last, other.last)
    }
}
