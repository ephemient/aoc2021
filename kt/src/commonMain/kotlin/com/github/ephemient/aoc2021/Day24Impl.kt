package com.github.ephemient.aoc2021

interface Day24Impl {
    fun part1(): Long?
    fun part2(): Long?

    interface Provider<T : Day24Impl> {
        operator fun invoke(lines: List<String>): T?
    }
}
