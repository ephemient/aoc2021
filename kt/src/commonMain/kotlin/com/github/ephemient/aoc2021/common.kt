package com.github.ephemient.aoc2021

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
