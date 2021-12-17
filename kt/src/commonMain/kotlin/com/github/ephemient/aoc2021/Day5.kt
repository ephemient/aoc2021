package com.github.ephemient.aoc2021

/** Day 5: Hydrothermal Venture */
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

        @Suppress("ComplexMethod", "NestedBlockDepth")
        fun intersectTo(dest: MutableCollection<Point>, other: Segment) {
            if (x0 == x1) {
                if (other.x0 == other.x1) {
                    if (x0 == other.x0) for (y in maxOf(y0, other.y0)..minOf(y1, other.y1)) dest.add(Point(x0, y))
                } else if (x0 in other.x0..other.x1) {
                    val y = (other.y1 - other.y0) / (other.x1 - other.x0) * (x0 - other.x0) + other.y0
                    if (y in y0..y1) dest.add(Point(x0, y))
                }
            } else if (other.x0 == other.x1) {
                if (other.x0 in x0..x1) {
                    val y = (y1 - y0) / (x1 - x0) * (other.x0 - x0) + y0
                    if (y in other.y0..other.y1) dest.add(Point(other.x0, y))
                }
            } else {
                val m0 = (y1 - y0) / (x1 - x0)
                val m1 = (other.y1 - other.y0) / (other.x1 - other.x0)
                val a0 = y0 - m0 * x0
                val a1 = other.y0 - m1 * other.x0
                if (m0 == m1) {
                    if (a0 == a1) for (x in maxOf(x0, other.x0)..minOf(x1, other.x1)) dest.add(Point(x, m0 * x + a0))
                } else if ((a1 - a0) % (m1 - m0) == 0) {
                    val x = -(a1 - a0) / (m1 - m0)
                    if (x in maxOf(x0, other.x0)..minOf(x1, other.x1)) dest.add(Point(x, m0 * x + a0))
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
    }
}
