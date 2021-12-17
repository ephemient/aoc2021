package com.github.ephemient.aoc2021

/** Day 4: Giant Squid */
class Day4(lines: List<String>) {
    private val boards = buildList {
        val iterator = lines.iterator()
        check(iterator.hasNext())
        val draws = iterator.next().split(',').map { it.toInt() }
        val drawTurns = buildMap<Int, Int> {
            draws.forEachIndexed { turn, draw -> getOrPut(draw) { turn } }
        }.withDefault { Int.MAX_VALUE }
        if (!iterator.hasNext()) return@buildList
        check(iterator.next().isEmpty())
        while (iterator.hasNext()) {
            var width = -1
            val values = buildList {
                for (line in iterator) {
                    if (line.isEmpty()) break
                    val row = line.split(WHITESPACE).mapNotNull { if (it.isEmpty()) null else it.toInt() }
                    if (width < 0) width = row.size else check(width == row.size)
                    addAll(row)
                }
            }
            val turns = values.map { drawTurns.getValue(it) }
            val turn = minOf(
                (turns.indices step width).minOf { start -> (start until start + width).maxOf { turns[it] } },
                (0 until width).minOf { start -> (start..turns.lastIndex step width).maxOf { turns[it] } },
            )
            if (turn < Int.MAX_VALUE) {
                add(IndexedValue(turn, draws[turn] * values.sumOf { if (drawTurns.getValue(it) > turn) it else 0 }))
            }
        }
    }

    fun part1(): Int? = boards.minByOrNull { it.index }?.value

    fun part2(): Int? = boards.maxByOrNull { it.index }?.value

    companion object {
        private val WHITESPACE = """\s""".toRegex()
    }
}
