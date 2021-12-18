package com.github.ephemient.aoc2021

/** Day 18: Snailfish */
@ExperimentalStdlibApi
class Day18(lines: List<String>) {
    private val snails = lines.map { it.tokens() }

    fun part1(): Int = snails.reduce { x, y -> x + y }.magnitude()

    fun part2(): Int = sequence {
        for ((i, x) in snails.withIndex()) {
            for ((j, y) in snails.withIndex()) {
                if (i != j) yield(x + y)
            }
        }
    }.maxOf { it.magnitude() }

    private sealed class Token {
        object Open : Token()
        object Close : Token()
        class Value(val value: Int) : Token()
    }

    companion object {
        private fun String.tokens(): List<Token> = buildList {
            var i = 0
            DeepRecursiveFunction<Unit, Unit> {
                check(this@tokens[i++] == '[')
                add(Token.Open)
                if (this@tokens[i].isDigit()) add(Token.Value(this@tokens[i++].digitToInt())) else callRecursive(Unit)
                check(this@tokens[i++] == ',')
                if (this@tokens[i].isDigit()) add(Token.Value(this@tokens[i++].digitToInt())) else callRecursive(Unit)
                check(this@tokens[i++] == ']')
                add(Token.Close)
            }(Unit)
            check(i == this@tokens.length)
        }

        @Suppress(
            "ComplexMethod", "NestedBlockDepth", "LoopWithTooManyJumpStatements",
            "UnusedPrivateMember", // Detekt false-positive
        )
        private operator fun List<Token>.plus(other: List<Token>): List<Token> =
            ArrayList<Token>(this.size + other.size + 2).apply {
                add(Token.Open)
                addAll(this@plus)
                addAll(other)
                add(Token.Close)
                loop@while (true) {
                    var depth = 0
                    for (i in 0 until this.size - 4) {
                        if (depth > 3 && this[i] == Token.Open && this[i + 3] == Token.Close) {
                            val lhs = this[i + 1]
                            val rhs = this[i + 2]
                            if (lhs is Token.Value && rhs is Token.Value) {
                                this[i] = Token.Value(0)
                                this.subList(i + 1, i + 4).clear()
                                for (j in i - 1 downTo 0) {
                                    val token = this[j] as? Token.Value ?: continue
                                    this[j] = Token.Value(token.value + lhs.value)
                                    break
                                }
                                for (j in i + 1 until this.size) {
                                    val token = this[j] as? Token.Value ?: continue
                                    this[j] = Token.Value(token.value + rhs.value)
                                    break
                                }
                                continue@loop
                            }
                        }
                        when (this[i]) {
                            Token.Open -> depth++
                            Token.Close -> depth--
                            else -> {}
                        }
                    }
                    for ((i, token) in this.withIndex()) {
                        if (token is Token.Value && token.value > 9) {
                            this[i] = Token.Open
                            this.addAll(
                                i + 1,
                                listOf(Token.Value(token.value / 2), Token.Value((token.value + 1) / 2), Token.Close),
                            )
                            continue@loop
                        }
                    }
                    break@loop
                }
            }

        private fun List<Token>.magnitude(): Int {
            var i = 0
            return DeepRecursiveFunction<Unit, Int> {
                when (val token = this@magnitude[i++]) {
                    is Token.Open -> (3 * callRecursive(Unit) + 2 * callRecursive(Unit)).also { i++ }
                    is Token.Value -> token.value
                    else -> TODO()
                }
            }.invoke(Unit)
        }
    }
}
