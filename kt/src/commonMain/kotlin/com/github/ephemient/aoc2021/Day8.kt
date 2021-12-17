package com.github.ephemient.aoc2021

/** Day 8: Seven Segment Search */
class Day8(private val lines: List<String>) {
    fun part1(): Int = lines.sumOf {
        it.substringAfter(" | ")
            .split(' ')
            .count { it.length == 2 || it.length == 4 || it.length == 3 || it.length == 7 }
    }

    fun part2(): Int = lines.sumOf { line ->
        val (lhs, rhs) = line.split(" | ", limit = 2)
        val signals = lhs.split(' ').map { it.toBits() }.groupBy { it.countOneBits() }
        val one = signals.getValue(2).single()
        val seven = signals.getValue(3).single()
        val four = signals.getValue(4).single()
        val fourSeven = four or seven
        val (twos, threeFive) = signals.getValue(5).partition { (it and fourSeven.inv()).countOneBits() == 2 }
        val two = twos.single()
        val (threes, fives) = threeFive.partition { (it and two.inv()).countOneBits() == 1 }
        val three = threes.single()
        val five = fives.single()
        val (sixes, zeroNine) = signals.getValue(6).partition { one and it.inv() != 0 }
        val six = sixes.single()
        val (zeros, nines) = zeroNine.partition { (it and three.inv()).countOneBits() == 2 }
        val nine = nines.single()
        val zero = zeros.single()
        val eight = signals.getValue(7).single()
        val digits = intArrayOf(zero, one, two, three, four, five, six, seven, eight, nine)
        rhs.split(' ').fold(0) { acc: Int, s -> 10 * acc + digits.indexOf(s.toBits()) }
    }

    companion object {
        private fun String.toBits(): Int = fold(0) { acc, c -> acc or 1.shl(c.code) }
    }
}
