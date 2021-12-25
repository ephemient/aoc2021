package com.github.ephemient.aoc2021

/** [Day 25](https://adventofcode.com/2022/day/25): Sea cucumber */
class Day25(lines: List<String>) {
    private val height = lines.size
    private val width = lines.maxOfOrNull { it.length } ?: 0
    private val initialState = CharArray(width * height) { lines[it / width].getOrElse(it % width) { '.' } }

    fun part1(): Int {
        var n = 1
        val state = initialState.copyOf()
        while (true) {
            val a = (0 until height).fold(true) { acc, y ->
                val indices = (0 until width).filter { x ->
                    state[y * width + x] == '>' && state[y * width + (x + 1) % width] == '.'
                }
                for (x in indices) {
                    state[y * width + x] = '.'
                    state[y * width + (x + 1) % width] = '>'
                }
                acc && indices.isEmpty()
            }
            val b = (0 until width).fold(true) { acc, x ->
                val indices = (0 until height).filter { y ->
                    state[y * width + x] == 'v' && state[(y + 1) % height * width + x] == '.'
                }
                for (y in indices) {
                    state[y * width + x] = '.'
                    state[(y + 1) % height * width + x] = 'v'
                }
                acc && indices.isEmpty()
            }
            if (a && b) return n else n++
        }
    }
}
