package com.github.ephemient.aoc2021

import kotlin.contracts.ExperimentalContracts
import kotlin.contracts.InvocationKind
import kotlin.contracts.contract

enum class CardinalDirection {
    NORTH, WEST, EAST, SOUTH;

    companion object
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
