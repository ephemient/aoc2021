package com.github.ephemient.aoc2021

/** Day 15: Chiton */
class Day15(lines: List<String>) {
    private val width: Int = lines.first().length
    private val height: Int = lines.size
    private val risks: IntArray
    init {
        check(lines.all { it.length == width && it.all { it.isDigit() } })
        risks = IntArray(width * lines.size) { lines[it / width][it % width].digitToInt() }
    }

    fun part1(): Int = solve(width, height, risks)

    fun part2(): Int = solve(
        5 * width,
        5 * height,
        IntArray(25 * risks.size) {
            val x = it % (5 * width)
            val y = it / (5 * width)
            (risks[y % (risks.size / width) * width + x % width] - 1 + x / width + y / height) % 9 + 1
        }
    )

    private fun solve(width: Int, height: Int, risks: IntArray): Int {
        val bests = IntArray(risks.size) { Int.MAX_VALUE }
        bests[0] = 0
        val queue = PriorityQueue(compareBy(IntPair::first))
        queue.add(0 to 0)
        while (true) {
            val i = queue.remove().second
            val c0 = bests[i]
            if (i == risks.lastIndex) return c0
            val x0 = i % width
            val y0 = i / width
            for ((x1, y1) in arrayOf(x0 - 1 to y0, x0 to y0 - 1, x0 to y0 + 1, x0 + 1 to y0)) {
                if (x1 !in 0 until width || y1 !in 0 until height) continue
                val j = y1 * width + x1
                val c1 = c0 + risks[j]
                if (c1 < bests[j]) {
                    bests[j] = c1
                    queue.add(c1 to j)
                }
            }
        }
    }
}
