package com.github.ephemient.aoc2021

class Day4(lines: List<String>) {
    private val draws: List<Int>
    private val boards: List<BingoBoard>
    init {
        val iterator = lines.iterator()
        check(iterator.hasNext())
        draws = iterator.next().split(',').map { it.toInt() }
        boards = buildList {
            if (!iterator.hasNext()) return@buildList
            check(iterator.next().isEmpty())
            while (iterator.hasNext()) {
                var width = -1
                var height = 0
                val unmarked = mutableMapOf<Int, Int>()
                while (iterator.hasNext()) {
                    val line = iterator.next()
                    if (line.isEmpty()) break
                    val row = line.split(WHITESPACE).filter { it.isNotEmpty() }.map { it.toInt() }
                    check(row.isNotEmpty())
                    if (height++ == 0) {
                        width = row.size
                    } else {
                        check(width == row.size)
                    }
                    for ((x, n) in row.withIndex()) {
                        unmarked[n] = unmarked.getOrElse(n) { 0 } or (1 shl x + width * (height - 1))
                    }
                }
                check(height > 0)
                add(BingoBoard(width, height, unmarked, 0))
            }
        }
    }

    fun part1(): Int? {
        val boards = boards.map { it.copy() }
        for (draw in draws) {
            for (board in boards) {
                if (board.play(draw)) {
                    return draw * board.sum()
                }
            }
        }
        return null
    }

    fun part2(): Int? {
        val boards = boards.mapTo(mutableListOf()) { it.copy() }
        var score: Int? = null
        for (draw in draws) {
            boards.retainAll { board ->
                if (board.play(draw)) {
                    score = draw * board.sum()
                    false
                } else true
            }
            if (boards.isEmpty()) break
        }
        return score
    }

    private class BingoBoard(
        private val width: Int,
        private val height: Int,
        private val unmarked: MutableMap<Int, Int>,
        private var marked: Int,
    ) {
        init {
            require(width * height < Int.SIZE_BITS)
        }

        fun copy(): BingoBoard = BingoBoard(width, height, unmarked.toMutableMap(), marked)

        fun play(draw: Int): Boolean {
            val marks = unmarked.remove(draw) ?: return false
            marked = marks or marked
            return (0 until height).any { y ->
                val mask = (1 shl width) - 1 shl y * width
                marked and mask == mask
            } || run {
                (0 until width).any { x ->
                    val mask = (((1u shl width * height) - 1u) / ((1u shl width) - 1u)).toInt() shl x
                    marked and mask == mask
                }
            }
        }

        fun sum(): Int = unmarked.entries.sumOf { (key, value) -> key * value.countOneBits() }
    }

    companion object {
        private val WHITESPACE = """\s""".toRegex()
    }
}
