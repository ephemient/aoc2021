package com.github.ephemient.aoc2021

import java.util.ServiceLoader

actual class Day24 actual constructor(lines: List<String>) {
    private val impl = ServiceLoader.load(Day24Impl.Provider::class.java).firstNotNullOf { it(lines) }

    actual fun part1(): Long? = impl.part1()

    actual fun part2(): Long? = impl.part2()
}
