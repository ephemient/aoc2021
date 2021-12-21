package com.github.ephemient.aoc2021

data class LongPair(val first: Long, val second: Long) {
    override fun toString(): String = "($first, $second)"
}

infix fun Long.to(other: Long): LongPair = LongPair(this, other)
