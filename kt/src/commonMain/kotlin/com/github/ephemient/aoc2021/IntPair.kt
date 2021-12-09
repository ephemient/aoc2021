package com.github.ephemient.aoc2021

data class IntPair(val first: Int, val second: Int)

infix fun Int.to(other: Int): IntPair = IntPair(this, other)
