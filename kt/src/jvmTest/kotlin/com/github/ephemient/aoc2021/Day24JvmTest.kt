package com.github.ephemient.aoc2021

import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource
import kotlin.test.assertEquals

class Day24JvmTest {
    @ExperimentalMultiplatform
    @ParameterizedTest
    @ValueSource(classes = [Day24Common::class, Day24Jvm::class, Day24Range::class])
    fun part1(implClass: Class<out Day24Impl>) {
        val impl = implClass.getDeclaredConstructor(List::class.java).newInstance(Day24Test.SAMPLE_INPUT)
        assertEquals(99999993811817, impl.part1())
    }

    @ExperimentalMultiplatform
    @ParameterizedTest
    @ValueSource(classes = [Day24Common::class, Day24Jvm::class, Day24Range::class])
    fun part2(implClass: Class<out Day24Impl>) {
        val impl = implClass.getDeclaredConstructor(List::class.java).newInstance(Day24Test.SAMPLE_INPUT)
        assertEquals(11111128657365, impl.part2())
    }
}
