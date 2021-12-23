package com.github.ephemient.aoc2021

/** [Day 22](https://adventofcode.com/2022/day/22): Reactor Reboot */
class Day22(lines: List<String>) {
    private val cuboids = lines.map { line ->
        val (on, x0, x1, y0, y1, z0, z1) = PATTERN.matchEntire(line)!!.destructured
        Cuboid(on == "on", x0.toInt(), x1.toInt(), y0.toInt(), y1.toInt(), z0.toInt(), z1.toInt())
    }

    fun part1(): Long = solve(
        cuboids.filter { (_, x0, x1, y0, y1, z0, z1) ->
            x0 in -50..50 && x1 in -50..50 && y0 in -50..50 && y1 in -50..50 && z0 in -50..50 && z1 in -50..50
        }
    )

    fun part2(): Long = solve(cuboids)

    private data class Cuboid(
        val on: Boolean,
        val x0: Int,
        val x1: Int,
        val y0: Int,
        val y1: Int,
        val z0: Int,
        val z1: Int,
    ) {
        val volume: Long
            get() = (x1 - x0 + 1L) * (y1 - y0 + 1L) * (z1 - z0 + 1L)
    }

    private interface FilterMap {
        operator fun invoke(first: Int, second: Int): IntPair?
    }

    private class Above(private val min: Int) : FilterMap {
        override fun invoke(first: Int, second: Int): IntPair? =
            if (second > min) first.coerceAtLeast(min + 1) to second else null
    }

    private class Below(private val max: Int) : FilterMap {
        override fun invoke(first: Int, second: Int): IntPair? =
            if (first < max) first to second.coerceAtMost(max - 1) else null
    }

    private class Clip(private val min: Int, private val max: Int) : FilterMap {
        override fun invoke(first: Int, second: Int): IntPair? =
            if (first <= max && second >= min) first.coerceAtLeast(min) to second.coerceAtMost(max) else null
    }

    private object Identity : FilterMap {
        override operator fun invoke(first: Int, second: Int): IntPair = first to second
    }

    companion object {
        private val PATTERN = """(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)""".toRegex()

        private fun Iterable<Cuboid>.filterMap(x: FilterMap, y: FilterMap, z: FilterMap): List<Cuboid> = mapNotNull {
            val (x0, x1) = x(it.x0, it.x1) ?: return@mapNotNull null
            val (y0, y1) = y(it.y0, it.y1) ?: return@mapNotNull null
            val (z0, z1) = z(it.z0, it.z1) ?: return@mapNotNull null
            Cuboid(it.on, x0, x1, y0, y1, z0, z1)
        }

        private fun solve(cuboids: List<Cuboid>, on: Boolean = true): Long {
            val i = cuboids.indexOfFirst { it.on == on }
            if (i < 0) return 0
            val top = cuboids[i]
            val rest = cuboids.drop(i + 1)
            return top.volume -
                solve(rest.filterMap(Clip(top.x0, top.x1), Clip(top.y0, top.y1), Clip(top.z0, top.z1)), !on) +
                solve(rest.filterMap(Identity, Identity, Below(top.z0)), on) +
                solve(rest.filterMap(Identity, Identity, Above(top.z1)), on) +
                solve(rest.filterMap(Identity, Below(top.y0), Clip(top.z0, top.z1)), on) +
                solve(rest.filterMap(Identity, Above(top.y1), Clip(top.z0, top.z1)), on) +
                solve(rest.filterMap(Below(top.x0), Clip(top.y0, top.y1), Clip(top.z0, top.z1)), on) +
                solve(rest.filterMap(Above(top.x1), Clip(top.y0, top.y1), Clip(top.z0, top.z1)), on)
        }
    }
}
