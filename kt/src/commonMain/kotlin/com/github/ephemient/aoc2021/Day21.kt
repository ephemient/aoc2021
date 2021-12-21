package com.github.ephemient.aoc2021

import kotlin.native.concurrent.ThreadLocal

/** Day 21: Dirac Dice */
class Day21(lines: List<String>) {
    private val player1Start = lines[0].removePrefix("Player 1 starting position: ").toInt()
    private val player2Start = lines[1].removePrefix("Player 2 starting position: ").toInt()

    fun part1(): Int {
        var player1 = player1Start
        var player2 = player2Start
        var score1 = 0
        var score2 = 0
        var n = 0
        while (score2 < 1000) {
            player1 = player2.also { player2 = (player1 + n % 100 + (n + 1) % 100 + (n + 2) % 100 + 2) % 10 + 1 }
            score1 = score2.also { score2 = score1 + player2 }
            n += 3
        }
        return n * score1
    }

    fun part2(): Long = Score(player1Start, player2Start, 0, 0).let { (first, second) -> maxOf(first, second) }

    @ThreadLocal
    private object Score {
        private val dice = sequence {
            for (i in 1..3) for (j in 1..3) for (k in 1..3) yield(i + j + k)
        }.groupingBy { it }.eachCount()
        private val x = LongArray(44100)
        private val y = LongArray(44100)

        operator fun invoke(player1: Int, player2: Int, score1: Int, score2: Int): LongPair {
            val i = player1 + 10 * player2 + 100 * score1 + 2100 * score2 - 11
            if (x[i] == 0L && y[i] == 0L) {
                var x1 = 0L
                var y1 = 0L
                for ((d, n) in dice) {
                    val play = (player1 + d - 1) % 10 + 1
                    if (score1 + play < 21) {
                        val (x2, y2) = this(player2, play, score2, score1 + play)
                        x1 += n * y2
                        y1 += n * x2
                    } else {
                        x1 += n
                    }
                }
                x[i] = x1
                y[i] = y1
            }
            return x[i] to y[i]
        }
    }
}
