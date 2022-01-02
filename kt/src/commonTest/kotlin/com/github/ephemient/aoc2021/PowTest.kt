package com.github.ephemient.aoc2021

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFails

class PowTest {
    @Test
    fun testInt() {
        for (x in -9..9) {
            for (y in -2..9) {
                if (y < 0) assertFails("$x ^ $y") { x.pow(y) }
                else assertEquals((0 until y).fold(1) { acc, _ -> acc * x }, x.pow(y), "$x ^ $y")
            }
        }
    }

    @Test
    fun testLong() {
        for (x in -15..15L) {
            for (y in -2..15) {
                if (y < 0) assertFails("$x ^ $y") { x.pow(y) }
                else assertEquals((0 until y).fold(1L) { acc, _ -> acc * x }, x.pow(y), "$x ^ $y")
            }
        }
    }

}
