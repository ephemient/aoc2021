package com.github.ephemient.aoc2021

internal interface HashWithSalt {
    fun hashWithSalt(salt: Int): Int
}
