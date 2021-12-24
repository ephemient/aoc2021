package com.github.ephemient.aoc2021

/** [Day 24](https://adventofcode.com/2021/day/24): Arithmetic Logic Unit */
class Day24(private val lines: List<String>) {
    fun part1(): Long? = solve(9 downTo 1)

    fun part2(): Long? = solve(1..9, visited = LruSet(1 shl 24))

    private fun solve(
        range: IntProgression,
        alu: ALU = ALU(),
        index: Int = 0,
        prefix: Long = 0L,
        visited: MutableSet<IndexedValue<ALU>> = mutableSetOf(),
    ): Long? {
        var i = index
        while (i < lines.size) {
            val line = lines[i++]
            if (line.startsWith("inp ")) {
                return if (visited.add(IndexedValue(i, alu.copy()))) {
                    val lhs = line.substring(4, 5)
                    range.firstNotNullOfOrNull { solve(range, alu.copy(lhs, it), i, 10 * prefix + it, visited) }
                } else null
            } else {
                val op = ops.getValue(line.substring(0, 3))
                val lhs = line.substring(4, 5)
                val rhs = line.substring(6)
                alu[lhs] = op(alu[lhs], rhs.toIntOrNull() ?: alu[rhs])
            }
        }
        return if (alu.z == 0) prefix else null
    }

    private data class ALU(var w: Int = 0, var x: Int = 0, var y: Int = 0, var z: Int = 0) {
        fun copy(key: String, value: Int) = when (key) {
            "w" -> copy(w = value)
            "x" -> copy(x = value)
            "y" -> copy(y = value)
            "z" -> copy(z = value)
            else -> TODO()
        }

        operator fun get(key: String) = when (key) {
            "w" -> w
            "x" -> x
            "y" -> y
            "z" -> z
            else -> TODO()
        }

        operator fun set(key: String, value: Int) {
            when (key) {
                "w" -> w = value
                "x" -> x = value
                "y" -> y = value
                "z" -> z = value
                else -> TODO()
            }
        }
    }

    companion object {
        private val ops: Map<String, (Int, Int) -> Int> = mapOf(
            "add" to Int::plus,
            "mul" to Int::times,
            "div" to Int::div,
            "mod" to Int::rem,
            "eql" to { x, y -> if (x == y) 1 else 0 },
        )
    }
}
