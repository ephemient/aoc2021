package com.github.ephemient.aoc2021

/** Day 3: Binary Diagnostic */
class Day3(private val lines: List<String>) {
    fun part1(): Int {
        val common = CharArray(lines.minOfOrNull { it.length } ?: 0) { i ->
            lines.groupingBy { it[i] }.eachCount().entries.maxByOrNull { it.value }!!.key
        }
        val gamma = common.concatToString().toInt(2)
        common.forEachIndexed { i, c -> common[i] = (c.code xor 1).toChar() }
        val epsilon = common.concatToString().toInt(2)
        return gamma * epsilon
    }

    fun part2(): Int {
        val o2 = lines.toMutableList()
        val co2 = lines.toMutableList()
        for (i in 0 until (lines.minOfOrNull { it.length } ?: 0)) {
            val common = o2.groupingBy { it[i] }.eachCount().entries.maxWithOrNull(comparator)!!.key
            val uncommon = co2.groupingBy { it[i] }.eachCount().entries.minWithOrNull(comparator)!!.key
            o2.retainAll { it[i] == common }
            co2.retainAll { it[i] == uncommon }
        }
        return o2.single().toInt(2) * co2.single().toInt(2)
    }

    companion object {
        private val comparator: Comparator<Map.Entry<Char, Int>> = compareBy({ it.value }, { it.key })
    }
}
