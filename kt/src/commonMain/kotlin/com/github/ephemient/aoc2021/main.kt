package com.github.ephemient.aoc2021

expect fun getInput(day: Int): List<String>

@Suppress("ComplexMethod", "LongMethod")
@ExperimentalStdlibApi
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

    if (days?.contains(5) != false) {
        val day5 = Day5(getInput(5))
        println("Day 5")
        println(day5.part1())
        println(day5.part2())
        println()
    }

    if (days?.contains(6) != false) {
        val day6 = Day6(getInput(6))
        println("Day 6")
        println(day6.part1())
        println(day6.part2())
        println()
    }

    if (days?.contains(7) != false) {
        val day7 = Day7(getInput(7))
        println("Day 7")
        println(day7.part1())
        println(day7.part2())
        println()
    }

    if (days?.contains(8) != false) {
        val day8 = Day8(getInput(8))
        println("Day 8")
        println(day8.part1())
        println(day8.part2())
        println()
    }

    if (days?.contains(9) != false) {
        val day9 = Day9(getInput(9))
        println("Day 9")
        println(day9.part1())
        println(day9.part2())
        println()
    }

    if (days?.contains(10) != false) {
        val day10 = Day10(getInput(10))
        println("Day 10")
        println(day10.part1())
        println(day10.part2())
        println()
    }

    if (days?.contains(11) != false) {
        val day11 = Day11(getInput(11))
        println("Day 11")
        println(day11.part1())
        println(day11.part2())
        println()
    }

    if (days?.contains(12) != false) {
        val day12 = Day12(getInput(12))
        println("Day 12")
        println(day12.part1())
        println(day12.part2())
        println()
    }

    if (days?.contains(13) != false) {
        val day13 = Day13(getInput(13))
        println("Day 13")
        println(day13.part1())
        println(day13.part2())
        println()
    }

    if (days?.contains(14) != false) {
        val day14 = Day14(getInput(14))
        println("Day 14")
        println(day14.part1())
        println(day14.part2())
        println()
    }

    if (days?.contains(15) != false) {
        val day15 = Day15(getInput(15))
        println("Day 15")
        println(day15.part1())
        println(day15.part2())
        println()
    }

    if (days?.contains(16) != false) {
        val day16 = Day16(getInput(16))
        println("Day 16")
        println(day16.part1())
        println(day16.part2())
        println()
    }

    if (days?.contains(17) != false) {
        val day17 = Day17(getInput(17))
        println("Day 17")
        val (part1, part2) = day17.solve()
        println(part1)
        println(part2)
        println()
    }

    if (days?.contains(18) != false) {
        val day18 = Day18(getInput(18))
        println("Day 18")
        println(day18.part1())
        println(day18.part2())
        println()
    }

    if (days?.contains(19) != false) {
        val day19 = Day19(getInput(19))
        println("Day 19")
        val (part1, part2) = day19.solve()!!
        println(part1)
        println(part2)
        println()
    }

    if (days?.contains(20) != false) {
        val day20 = Day20(getInput(20))
        println("Day 20")
        println(day20.part1())
        println(day20.part2())
        println()
    }

    if (days?.contains(21) != false) {
        val day21 = Day21(getInput(21))
        println("Day 21")
        println(day21.part1())
        println(day21.part2())
        println()
    }

    if (days?.contains(22) != false) {
        val day22 = Day22(getInput(22))
        println("Day 22")
        println(day22.part1())
        println(day22.part2())
        println()
    }
}
