package com.github.ephemient.aoc2021

class Day12(lines: List<String>) {
    private val start: Int
    private val end: Int
    private val edges: Map<Int, List<Pair<Int, Boolean>>>

    init {
        val names = mutableMapOf<String, Int>()
        val edges = mutableMapOf<Int, MutableList<Pair<Int, Boolean>>>()
        for (line in lines) {
            val (lhs, rhs) = line.split('-', limit = 2)
            val src = names.getOrPut(lhs) { names.size }
            val dst = names.getOrPut(rhs) { names.size }
            edges.getOrPut(src) { mutableListOf() }.add(dst to rhs.all { it.isUpperCase() })
            edges.getOrPut(dst) { mutableListOf() }.add(src to lhs.all { it.isUpperCase() })
        }
        check(names.size < Int.SIZE_BITS)
        start = names.getValue("start")
        end = names.getValue("end")
        this.edges = edges
    }

    private fun solve(bonus: Boolean): Int {
        var count = 0
        walk((if (bonus) 0 else Int.MIN_VALUE) to start) { (state, element) ->
            edges[element]?.mapNotNull { (next, big) ->
                when {
                    next == start -> null
                    next == end -> null.also { count++ }
                    big -> state to next
                    state.and(1 shl next) == 0 -> state.or(1 shl next) to next
                    state >= 0 -> state or Int.MIN_VALUE to next
                    else -> null
                }
            }.orEmpty()
        }
        return count
    }

    fun part1(): Int = solve(false)

    fun part2(): Int = solve(true)

    companion object {
        private fun <T> walk(start: T, step: (T) -> Iterable<T>) {
            val stack = mutableListOf(step(start).iterator())
            while (stack.isNotEmpty()) {
                val iterator = stack.last()
                if (!iterator.hasNext()) {
                    stack.removeLast()
                    continue
                }
                stack.add(step(iterator.next()).iterator())
            }
        }
    }
}
