package com.github.ephemient.aoc2021

expect fun getInput(day: Int): List<String>

fun main(args: Array<String>) {
    val days = args.mapNotNull { it.toIntOrNull() }.takeIf { it.isNotEmpty() }

    if (days?.contains(1) != false) {
        val day1 = Day1(getInput(1))
        println("Day 1")
        println(day1.part1())
        println(day1.part2())
        println()
    }

    if (days?.contains(2) != false) {
        val day2 = Day2(getInput(2))
        println("Day 2")
        println(day2.part1())
        println(day2.part2())
        println()
    }
}
