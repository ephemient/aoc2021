package com.github.ephemient.aoc2021

fun Int.pow(x: Int): Int = when {
    x < 0 -> throw ArithmeticException("negative exponent")
    x == 0 -> 1
    x == 1 || this == 0 || this == 1 -> this
    this == -1 -> if (x and 1 == 0) 1 else -1
    else -> {
        var r = 1
        var b = this
        var e = x
        while (true) {
            if (e and 1 != 0) r *= b
            e = e ushr 1
            if (e == 0) break
            b *= b
        }
        r
    }
}

fun Long.pow(x: Int): Long = when {
    x < 0 -> throw ArithmeticException("negative exponent")
    x == 0 -> 1
    x == 1 || this == 0L || this == 1L -> this
    this == -1L -> if (x and 1 == 0) 1 else -1
    else -> {
        var r = 1L
        var b = this
        var e = x
        while (true) {
            if (e and 1 != 0) r *= b
            e = e ushr 1
            if (e == 0) break
            b *= b
        }
        r
    }
}

inline fun permutationIndices(size: Int, block: (IntArray, Boolean) -> Unit) {
    val indices = IntArray(size) { it }
    var parity = false
    while (true) {
        block(indices, parity)
        val i = (indices.size - 2 downTo 0).firstOrNull { indices[it] < indices[it + 1] } ?: break
        var j = i + 1
        for (k in i + 2 until indices.size) if (indices[k] in indices[i]..indices[j]) j = k
        indices[i] = indices[j].also { indices[j] = indices[i] }
        indices.reverse(i + 1, indices.size)
        if (i xor indices.size and 1 == 0) parity = !parity
    }
}

fun <T> List<T>.permutations(): Sequence<List<T>> = sequence {
    permutationIndices(size) { indices, _ -> yield(indices.map(this@permutations::get)) }
}
