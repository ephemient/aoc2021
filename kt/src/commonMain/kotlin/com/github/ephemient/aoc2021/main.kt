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

    if (days?.contains(3) != false) {
        val day3 = Day3(getInput(3))
        println("Day 3")
        println(day3.part1())
        println(day3.part2())
        println()
    }

    if (days?.contains(4) != false) {
        val day4 = Day4(getInput(4))
        println("Day 4")
        println(day4.part1())
        println(day4.part2())
        println()
    }
}
