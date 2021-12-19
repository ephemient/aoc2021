package com.github.ephemient.aoc2021

import kotlin.math.abs

/** Day 19: Beacon Scanner */
class Day19(lines: List<String>) {
    private val scanners = buildList {
        val iterator = lines.iterator()
        while (iterator.hasNext()) {
            check(HEADER.matches(iterator.next()))
            val scanner = buildList {
                while (iterator.hasNext()) {
                    val line = iterator.next()
                    if (line.isEmpty()) break
                    add(line.split(',').map { it.toInt() })
                }
            }
            add(scanner)
        }
    }

    private val size = scanners.first().first().size
    init {
        check(size in 1 until Int.SIZE_BITS - 1)
        check(scanners.all { it.isNotEmpty() && it.all { it.size == size } })
    }

    private val allTransformations = buildList {
        permutationIndices(this@Day19.size) { indices ->
            repeat(1 shl this@Day19.size) { bits ->
                add(IntArray(this@Day19.size) { if (1 shl it and bits == 0) indices[it] else indices[it].inv() })
            }
        }
    }
    private fun transform(transformationIndex: Int, point: List<Int>): List<Int> =
        allTransformations[transformationIndex].map { if (it < 0) -point[it.inv()] else point[it] }

    @ExperimentalStdlibApi
    fun solve(): IntPair? {
        val untransformedDeltas = scanners.map { scanner ->
            buildSet { for (x in scanner) for (y in scanner) add(x.zip(y, Int::minus)) }
        }
        val deltas: Map<Set<List<Int>>, List<IntPair>> = sequence {
            for (i in 1 until scanners.size) for (t in allTransformations.indices) yield(i to t)
        }.groupBy { (i, t) -> untransformedDeltas[i].mapTo(mutableSetOf()) { transform(t, it) } }
        return DeepRecursiveFunction<Triple<Set<List<Int>>, List<List<Int>>, Set<Int>>, IntPair?> {
                (beacons, positions, remaining) ->
            if (remaining.isEmpty()) {
                return@DeepRecursiveFunction beacons.size to
                        positions.maxOf { x -> positions.maxOf { y -> (0 until size).sumOf { abs(x[it] - y[it]) } } }
            }
            val delta = buildSet { for (x in beacons) for (y in beacons) add(x.zip(y, Int::minus)) }
            val next = deltas.entries.mapTo(mutableListOf()) { (key, value) -> key.count { it in delta } to value }
            next.sortByDescending { it.first }
            next.firstNotNullOfOrNull { (_, value) ->
                value.firstNotNullOfOrNull loop@{ (i, t) ->
                    if (i !in remaining) return@loop null
                    val points = scanners[i].map { transform(t, it) }
                    val newPositions = buildSet { for (x in beacons) for (y in points) add(x.zip(y, Int::minus)) }
                    newPositions.firstNotNullOfOrNull { position ->
                        val newBeacons = points.mapTo(mutableSetOf()) { it.zip(position, Int::plus) }
                        if (beacons.count { !newBeacons.add(it) } >= 12) {
                            callRecursive(Triple(newBeacons, positions + listOf(position), remaining - i))
                        } else null
                    }
                }
            }
        }(Triple(scanners.first().toSet(), listOf(List(size) { 0 }), (1 until scanners.size).toSet()))
    }

    companion object {
        private val HEADER = """--- scanner \w+ ---""".toRegex()
    }
}
