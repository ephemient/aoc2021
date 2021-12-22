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

    companion object {
        private val PATTERN = """(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)""".toRegex()

        @Suppress("ComplexCondition", "ComplexMethod", "LongMethod")
        private fun solve(cuboids: List<Cuboid>): Long {
            val i = cuboids.indexOfFirst { it.on }
            if (i < 0) return 0
            val top = cuboids[i]
            return top.volume - solve(
                cuboids.drop(i + 1).mapNotNull { (on, x0, x1, y0, y1, z0, z1) ->
                    if (x0 <= top.x1 && x1 >= top.x0 && y0 <= top.y1 && y1 >= top.y0 && z0 <= top.z1 && z1 >= top.z0) {
                        Cuboid(
                            !on,
                            x0.coerceIn(top.x0..top.x1), x1.coerceIn(top.x0..top.x1),
                            y0.coerceIn(top.y0..top.y1), y1.coerceIn(top.y0..top.y1),
                            z0.coerceIn(top.z0..top.z1), z1.coerceIn(top.z0..top.z1),
                        )
                    } else null
                }
            ) + solve(
                cuboids.drop(i + 1).mapNotNull { (on, x0, x1, y0, y1, z0, z1) ->
                    if (z0 < top.z0) {
                        Cuboid(on, x0, x1, y0, y1, z0.coerceAtMost(top.z0 - 1), z1.coerceAtMost(top.z0 - 1))
                    } else null
                }
            ) + solve(
                cuboids.drop(i + 1).mapNotNull { (on, x0, x1, y0, y1, z0, z1) ->
                    if (z1 > top.z1) {
                        Cuboid(on, x0, x1, y0, y1, z0.coerceAtLeast(top.z1 + 1), z1.coerceAtLeast(top.z1 + 1))
                    } else null
                }
            ) + solve(
                cuboids.drop(i + 1).mapNotNull { (on, x0, x1, y0, y1, z0, z1) ->
                    if (z0 <= top.z1 && z1 >= top.z0 && y0 < top.y0) {
                        Cuboid(
                            on, x0, x1,
                            y0.coerceAtMost(top.y0 - 1), y1.coerceAtMost(top.y0 - 1),
                            z0.coerceIn(top.z0..top.z1), z1.coerceIn(top.z0..top.z1)
                        )
                    } else null
                }
            ) + solve(
                cuboids.drop(i + 1).mapNotNull { (on, x0, x1, y0, y1, z0, z1) ->
                    if (z0 <= top.z1 && z1 >= top.z0 && y1 > top.y1) {
                        Cuboid(
                            on, x0, x1,
                            y0.coerceAtLeast(top.y1 + 1), y1.coerceAtLeast(top.y1 + 1),
                            z0.coerceIn(top.z0..top.z1), z1.coerceIn(top.z0..top.z1)
                        )
                    } else null
                }
            ) + solve(
                cuboids.drop(i + 1).mapNotNull { (on, x0, x1, y0, y1, z0, z1) ->
                    if (z0 <= top.z1 && z1 >= top.z0 && y0 <= top.y1 && y1 >= top.y0 && x0 < top.x0) {
                        Cuboid(
                            on,
                            x0.coerceAtMost(top.x0 - 1), x1.coerceAtMost(top.x0 - 1),
                            y0.coerceIn(top.y0..top.y1), y1.coerceIn(top.y0..top.y1),
                            z0.coerceIn(top.z0..top.z1), z1.coerceIn(top.z0..top.z1)
                        )
                    } else null
                }
            ) + solve(
                cuboids.drop(i + 1).mapNotNull { (on, x0, x1, y0, y1, z0, z1) ->
                    if (z0 <= top.z1 && z1 >= top.z0 && y0 <= top.y1 && y1 >= top.y0 && x1 > top.x1) {
                        Cuboid(
                            on,
                            x0.coerceAtLeast(top.x1 + 1), x1.coerceAtLeast(top.x1 + 1),
                            y0.coerceIn(top.y0..top.y1), y1.coerceIn(top.y0..top.y1),
                            z0.coerceIn(top.z0..top.z1), z1.coerceIn(top.z0..top.z1)
                        )
                    } else null
                }
            )
        }
    }
}
