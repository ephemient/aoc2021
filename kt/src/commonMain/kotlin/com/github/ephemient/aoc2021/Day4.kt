package com.github.ephemient.aoc2021

private typealias BingoBoard = List<List<Int?>>
private typealias MutableBingoBoard = MutableList<MutableList<Int?>>

class Day4(lines: List<String>) {
    private val draws: List<Int>
    private val boards: List<BingoBoard>
    init {
        val iterator = lines.iterator()
        check(iterator.hasNext())
        draws = iterator.next().split(',').map { it.toInt() }
        check(iterator.hasNext() && iterator.next().isEmpty())
        boards = buildList {
            while (iterator.hasNext()) {
                add(
                    buildList {
                        for (line in iterator) {
                            if (line.isEmpty()) break
                            add(line.split(WHITESPACE).filter { it.isNotEmpty() }.map { it.toIntOrNull() })
                        }
                    }
                )
            }
        }
    }

    fun part1(): Int? {
        val boards: List<MutableBingoBoard> = boards.map { it.toMutableBoard() }
        for (draw in draws) {
            for (board in boards) {
                board.mark(draw)
                if (board.isBingo()) return draw * board.sum()
            }
        }
        return null
    }

    fun part2(): Int? = boards.mapNotNull {
        val board = it.toMutableBoard()
        for ((i, draw) in draws.withIndex()) {
            board.mark(draw)
            if (board.isBingo()) return@mapNotNull i to draw * board.sum()
        }
        return@mapNotNull null
    }.maxByOrNull { it.first }?.second

    companion object {
        private val WHITESPACE = """\s""".toRegex()

        private fun BingoBoard.toMutableBoard(): MutableBingoBoard = mapTo(mutableListOf()) { it.toMutableList() }
        private fun MutableBingoBoard.mark(draw: Int) {
            for (row in this) {
                val iterator = row.listIterator()
                while (iterator.hasNext()) {
                    if (iterator.next() == draw) iterator.set(null)
                }
            }
        }
        private fun BingoBoard.isBingo(): Boolean = any { row -> row.all { it == null } } ||
            (0 until (maxOfOrNull { it.size } ?: 0)).any { column ->
                all { it.getOrNull(column) == null }
            }
        private fun BingoBoard.sum(): Int = sumOf { row -> row.sumOf { it ?: 0 } }
    }
}
