package com.github.ephemient.aoc2021

import kotlin.contracts.ExperimentalContracts
import kotlin.contracts.InvocationKind
import kotlin.contracts.contract

enum class CardinalDirection {
    NORTH, WEST, EAST, SOUTH;

    companion object
}

operator fun CardinalDirection.invoke(x: Int, y: Int): IntPair = when (this) {
    CardinalDirection.NORTH -> x to y - 1
    CardinalDirection.WEST -> x - 1 to y
    CardinalDirection.EAST -> x + 1 to y
    CardinalDirection.SOUTH -> x to y + 1
}

operator fun CardinalDirection.invoke(x: Long, y: Long): LongPair = when (this) {
    CardinalDirection.NORTH -> x to y - 1
    CardinalDirection.WEST -> x - 1 to y
    CardinalDirection.EAST -> x + 1 to y
    CardinalDirection.SOUTH -> x to y + 1
}

@OptIn(ExperimentalContracts::class)
inline fun CardinalDirection.Companion.forEach(x: Int, y: Int, block: (x: Int, y: Int) -> Unit) {
    contract {
        callsInPlace(block, InvocationKind.AT_LEAST_ONCE)
    }

    block(x, y - 1)
    block(x - 1, y)
    block(x + 1, y)
    block(x, y + 1)
}

@OptIn(ExperimentalContracts::class)
inline fun CardinalDirection.Companion.forEach(x: Long, y: Long, block: (x: Long, y: Long) -> Unit) {
    contract {
        callsInPlace(block, InvocationKind.AT_LEAST_ONCE)
    }

    block(x, y - 1)
    block(x - 1, y)
    block(x + 1, y)
    block(x, y + 1)
}
