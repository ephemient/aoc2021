package com.github.ephemient.aoc2021

/** [Day 2](https://adventofcode.com/2021/day/2): Dive! */
class Day2(private val lines: List<String>) {
    fun part1(): Int {
        var x = 0
        var y = 0
        for (line in lines) {
            val (prefix, suffix) = line.split(" ", limit = 2)
            when (prefix) {
                "forward" -> x += suffix.toInt()
                "down" -> y += suffix.toInt()
                "up" -> y -= suffix.toInt()
            }
        }
        return x * y
    }

    fun part2(): Int {
        var x = 0
        var y = 0
        var depth = 0
        for (line in lines) {
            val (prefix, suffix) = line.split(" ", limit = 2)
            when (prefix) {
                "forward" -> suffix.toInt().let {
                    x += it
                    y += it * depth
                }
                "down" -> depth += suffix.toInt()
                "up" -> depth -= suffix.toInt()
            }
        }
        return x * y
    }
}
