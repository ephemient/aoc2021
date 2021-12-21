package com.github.ephemient.aoc2021

/** Day 20: Trench Map*/
class Day20(lines: List<String>) {
    private val alg = lines.first()
    private fun lookup(vararg key: Char): Char = alg[key.fold(0) { acc, c -> 2 * acc + c.code.and(1) }]
    private val images = generateSequence(lines.drop(2) to '.') { (image, fill) ->
        List(image.size + 2) { y ->
            val line1 = image.getOrNull(y - 2)
            val line2 = image.getOrNull(y - 1)
            val line3 = image.getOrNull(y)
            CharArray(image[(y - 1).coerceIn(image.indices)].length + 2) { x ->
                lookup(
                    line1?.getOrNull(x - 2) ?: fill, line1?.getOrNull(x - 1) ?: fill, line1?.getOrNull(x) ?: fill,
                    line2?.getOrNull(x - 2) ?: fill, line2?.getOrNull(x - 1) ?: fill, line2?.getOrNull(x) ?: fill,
                    line3?.getOrNull(x - 2) ?: fill, line3?.getOrNull(x - 1) ?: fill, line3?.getOrNull(x) ?: fill,
                )
            }.concatToString()
        } to lookup(fill, fill, fill, fill, fill, fill, fill, fill, fill)
    }.map { (image, fill) -> if (fill == '.') image else null }

    fun part1(): Int? = images.drop(2).firstOrNull()?.sumOf { it.count { it == '#' } }

    fun part2(): Int? = images.drop(50).firstOrNull()?.sumOf { it.count { it == '#' } }
}
