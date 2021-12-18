package com.github.ephemient.aoc2021

import kotlin.reflect.KMutableProperty0

/** Day 18: Snailfish */
@ExperimentalStdlibApi
class Day18(lines: List<String>) {
    private val snails = lines.map { it.toSnail() }

    fun part1(): Int = snails.reduce { x, y -> (x + y) }.magnitude()

    @Suppress("NestedBlockDepth")
    fun part2(): Int {
        check(snails.size > 1)
        var bestMagnitude: Int = Int.MIN_VALUE
        for ((i, x) in snails.withIndex()) {
            for ((j, y) in snails.withIndex()) {
                if (i != j) {
                    val magnitude = (x + y).magnitude()
                    if (bestMagnitude < magnitude) bestMagnitude = magnitude
                }
            }
        }
        return bestMagnitude
    }

    private interface Snailfish {
        val firstSnailfish: Snailfish?
        val firstValue: Int
        val secondSnailfish: Snailfish?
        val secondValue: Int

        operator fun plus(other: Snailfish): Snailfish = MutableSnailfish(
            firstSnailfish = this.toMutable(),
            firstValue = 0,
            secondSnailfish = other.toMutable(),
            secondValue = 0,
        ).apply { reduce() }

        fun toMutable(): MutableSnailfish = toMutable(this)

        fun magnitude(): Int = magnitude(this)

        companion object {
            private val toMutable = DeepRecursiveFunction<Snailfish, MutableSnailfish> { snail ->
                MutableSnailfish(
                    firstSnailfish = snail.firstSnailfish?.let { callRecursive(it) },
                    firstValue = snail.firstValue,
                    secondSnailfish = snail.secondSnailfish?.let { callRecursive(it) },
                    secondValue = snail.secondValue,
                )
            }

            private val magnitude = DeepRecursiveFunction<Snailfish, Int> { snail ->
                3 * (snail.firstSnailfish?.let { callRecursive(it) } ?: snail.firstValue) +
                    2 * (snail.secondSnailfish?.let { callRecursive(it) } ?: snail.secondValue)
            }
        }
    }

    private class SnailfishImpl(
        override val firstSnailfish: Snailfish?,
        override val firstValue: Int,
        override val secondSnailfish: Snailfish?,
        override val secondValue: Int,
    ) : Snailfish

    private class MutableSnailfish(
        override var firstSnailfish: MutableSnailfish?,
        override var firstValue: Int,
        override var secondSnailfish: MutableSnailfish?,
        override var secondValue: Int,
    ) : Snailfish {
        fun reduce() {
            @Suppress("LoopWithTooManyJumpStatements")
            while (true) {
                if (explode(ExplodeInput(this)) != null) continue
                if (split(this)) continue
                break
            }
        }

        override fun toString(): String = "[${firstSnailfish ?: firstValue},${secondSnailfish ?: secondValue}]"

        private data class ExplodeInput(val snail: MutableSnailfish, val depth: Int = 0)

        private data class ExplodeOutput(val replace: Boolean, val first: Int, val second: Int)

        companion object {
            private val explode = DeepRecursiveFunction<ExplodeInput, ExplodeOutput?> explode@{ (snail, depth) ->
                val firstSnail = snail.firstSnailfish
                val secondSnail = snail.secondSnailfish
                if (depth > 3 && firstSnail == null && secondSnail == null) {
                    return@explode ExplodeOutput(true, snail.firstValue, snail.secondValue)
                }
                firstSnail?.let { callRecursive(ExplodeInput(it, depth + 1)) }?.let { (replace, first, second) ->
                    if (replace) {
                        snail.firstSnailfish = null
                        snail.firstValue = 0
                    }
                    if (secondSnail != null) {
                        generateSequence(secondSnail) { it.firstSnailfish }.last().firstValue += second
                    } else snail.secondValue += second
                    ExplodeOutput(
                        false,
                        first,
                        0,
                    )
                } ?: secondSnail?.let { callRecursive(ExplodeInput(it, depth + 1)) }?.let { (replace, first, second) ->
                    if (replace) {
                        snail.secondSnailfish = null
                        snail.secondValue = 0
                    }
                    if (firstSnail != null) {
                        generateSequence(firstSnail) { it.secondSnailfish }.last().secondValue += first
                    } else snail.firstValue += first
                    ExplodeOutput(
                        false,
                        0,
                        second,
                    )
                }
            }

            private val split = DeepRecursiveFunction<MutableSnailfish, Boolean> {
                splitHelper(it::firstSnailfish, it::firstValue) || splitHelper(it::secondSnailfish, it::secondValue)
            }

            private suspend fun DeepRecursiveScope<MutableSnailfish, Boolean>.splitHelper(
                snailProperty: KMutableProperty0<MutableSnailfish?>,
                valueProperty: KMutableProperty0<Int>
            ): Boolean {
                snailProperty.get()?.let { return callRecursive(it) }
                val value = valueProperty.get()
                return if (value > 9) {
                    snailProperty.set(MutableSnailfish(null, value shr 1, null, value + 1 shr 1))
                    true
                } else false
            }
        }
    }

    companion object {
        private fun String.toSnail(): Snailfish {
            var i = 0
            val snail = DeepRecursiveFunction<Unit, Snailfish> {
                check(get(i++) == '[')
                var firstSnailfish: Snailfish? = null
                var firstValue = 0
                if (get(i).isDigit()) {
                    i = indexOf(',', i).also { firstValue = substring(i, it).toInt() } + 1
                } else {
                    firstSnailfish = callRecursive(Unit)
                    check(get(i++) == ',')
                }
                var secondSnailfish: Snailfish? = null
                var secondValue = 0
                if (get(i).isDigit()) {
                    i = indexOf(']', i).also { secondValue = substring(i, it).toInt() } + 1
                } else {
                    secondSnailfish = callRecursive(Unit)
                    check(get(i++) == ']')
                }
                SnailfishImpl(firstSnailfish, firstValue, secondSnailfish, secondValue)
            }(Unit)
            check(i == length)
            return snail
        }
    }
}
