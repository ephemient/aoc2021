package com.github.ephemient.aoc2021

class Day8(private val lines: List<String>) {
    fun part1(): Int = lines.sumOf {
        it.substringAfter(" | ")
            .split(' ')
            .count { it.length == 2 || it.length == 4 || it.length == 3 || it.length == 7 }
    }

    fun part2(): Int = lines.sumOf { line ->
        val (lhs, rhs) = line.split(" | ", limit = 2)
        val signals = lhs.split(' ').map { it.toSet() }.groupBy { it.size }
        val one = signals.getValue(2).single()
        val seven = signals.getValue(3).single()
        val four = signals.getValue(4).single()
        val (twos, threeFive) = signals.getValue(5).partition { (it - four).size == 3 }
        val two = twos.single()
        val (threes, fives) = threeFive.partition { (it - two).size == 1 }
        val three = threes.single()
        val five = fives.single()
        val (sixes, zeroNine) = signals.getValue(6).partition { (one - it).isNotEmpty() }
        val six = sixes.single()
        val (zeros, nines) = zeroNine.partition { (it - three).size == 2 }
        val nine = nines.single()
        val zero = zeros.single()
        val eight = signals.getValue(7).single()
        val digits = arrayOf(zero, one, two, three, four, five, six, seven, eight, nine)
        rhs.split(' ').fold(0) { acc: Int, s -> 10 * acc + digits.indexOf(s.toSet()) }
    }
}
