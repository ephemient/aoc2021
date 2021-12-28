package com.github.ephemient.aoc2021

import kotlin.math.abs

/** [Day 23](https://adventofcode.com/2022/day/23): Amphipod */
class Day23(private val lines: List<String>) {
    fun part1(): Int = solve(parse(lines))

    fun part2(): Int = solve(parse(lines.take(3) + PART2_INSERT + lines.drop(3)))

    companion object {
        private const val LINE_0 = "#############"
        private const val LINE_1 = "#...........#"
        private val LINE_2 = "###[A-D]#[A-D]#[A-D]#[A-D]###".toRegex()
        private val LINE_3 = "[ ][ ]#[A-D]#[A-D]#[A-D]#[A-D]#[ ]{0,2}".toRegex()
        private val LINE_4 = "[ ][ ]#########[ ]{0,2}".toRegex()

        private val PART2_INSERT = listOf("  #D#C#B#A#", "  #D#B#A#C#")

        private fun Int.weight() = when (this) {
            1 -> 1
            2 -> 10
            3 -> 100
            4 -> 1000
            else -> 0
        }

        private fun IntArray.copy(index: Int, value: Int) = IntArray(size) { if (it == index) value else get(it) }

        private fun List<IntArray>.flatten(): List<Int> {
            val values = this@flatten.flatMap { it.asList() }.toIntArray()
            return object : AbstractList<Int>() {
                override val size: Int
                    get() = values.size
                override fun get(index: Int): Int = values[index]
            }
        }

        private fun parse(lines: List<String>): List<IntArray> {
            check(
                lines[0] == LINE_0 && lines[1] == LINE_1 && LINE_2.matches(lines[2]) &&
                    lines.subList(3, lines.size - 1).all { LINE_3.matches(it) } && LINE_4.matches(lines.last())
            )
            return List(11) { x ->
                when (x) {
                    in 2..8 step 2 -> IntArray(lines.size - 3) { y -> lines[y + 2][x + 1] - 'A' + 1 }
                    else -> intArrayOf(0)
                }
            }
        }

        @Suppress("ComplexCondition", "ComplexMethod", "NestedBlockDepth")
        private fun solve(initialState: List<IntArray>): Int {
            val queue = PriorityQueue<Pair<Int, List<IntArray>>>(compareBy { it.first })
            val visited = mutableMapOf(initialState.flatten() to 0)
            queue.add(0 to initialState)
            while (true) {
                val (_, state) = queue.remove()
                val cost = visited.getValue(state.flatten())
                if ((1..4).all { state[2 * it].all(it::equals) }) return cost
                val moves = state.withIndex().firstNotNullOfOrNull { (i, arr) ->
                    val x = arr.indexOfFirst { it != 0 }
                    if (x < 0) return@firstNotNullOfOrNull null
                    val j = 2 * arr[x]
                    if (
                        i < j && (i + 1 until j).all { it % 2 == 0 || 0 in state[it] } ||
                        i > j && (i - 1 downTo j + 1).all { it % 2 == 0 || 0 in state[it] }
                    ) {
                        val arr2 = state[j]
                        val k = arr2.indexOfLast { it == 0 }
                        if (k >= 0 && arr2.all { it == 0 || it == arr[x] }) {
                            return@firstNotNullOfOrNull listOf(i to j)
                        }
                    }
                    null
                } ?: (2..8 step 2).flatMap { i ->
                    if (state[i].any { it != 0 }) {
                        (-1..11 step 2).mapNotNull {
                            val j = it.coerceIn(0, 10)
                            if (
                                0 in state[j] &&
                                ((i - 1 downTo j + 1 step 2) + (i + 1 until j step 2)).all { 0 in state[it] }
                            ) i to j else null
                        }
                    } else emptyList()
                }
                for ((i, j) in moves) {
                    val x = state[i].indexOfFirst { it != 0 }
                    val y = state[j].indexOfLast { it == 0 }
                    val state2 = state.toMutableList().apply {
                        this[i] = state[i].copy(x, 0)
                        this[j] = state[j].copy(y, state[i][x])
                    }
                    val key = state2.flatten()
                    val cost2 = cost + (d(i, j) + x + y) * state[i][x].weight()
                    if (visited[key]?.let { it > cost2 } != false) {
                        visited[key] = cost2
                        queue.add(cost2 to state2)
                    }
                }
            }
        }

        private fun d(i: Int, j: Int): Int {
            var d = abs(i.coerceIn(1, 9) - j.coerceIn(1, 9)) + 2 and 1.inv()
            if (i == 0 || i == 10) d++
            if (j == 0 || j == 10) d++
            return d
        }
    }
}
