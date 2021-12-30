package com.github.ephemient.aoc2021

import kotlin.reflect.KMutableProperty1
import kotlin.reflect.KProperty1

class Day24Common(private val lines: List<String>) : Day24Impl {
    private val initialAlu = ALU()
    private val groups: List<Pair<KMutableProperty1<ALU, Int>, List<Instruction>>> =
        buildList<Pair<KMutableProperty1<ALU, Int>, MutableList<Instruction>>> {
            for (line in lines) {
                if (line.startsWith("inp ")) {
                    add(ALU[line.substring(4)] to mutableListOf())
                } else {
                    check(line[3] == ' ' && line[5] == ' ')
                    val op = Op.valueOf(line.substring(0, 3))
                    val lhs = ALU[line.substring(4, 5)]
                    val rhs = line.substring(6)
                    val instruction = rhs.toIntOrNull()
                        ?.let { Instruction.RegImm(lhs, it, op) }
                        ?: Instruction.RegReg(lhs, ALU[rhs], op)
                    lastOrNull()?.also { (_, group) -> group.add(instruction) } ?: instruction(initialAlu)
                }
            }
        }

    override fun part1(): Long? = solve(9 downTo 1)

    override fun part2(): Long? = solve(1..9)

    @Suppress("ReturnCount")
    private fun solve(
        range: IntProgression,
        alu: ALU = initialAlu.copy(),
        index: Int = 0,
        prefix: Long = 0L,
        visited: MutableSet<in State> = CacheSet(0x1000000),
    ): Long? {
        val (inp, group) = groups.getOrNull(index) ?: return prefix.takeIf { alu.z == 0 }
        with(alu.copy().also { inp.set(it, 0) }) {
            if (!visited.add(State(index, w, x, y, z))) return null
        }
        return range.firstNotNullOfOrNull {
            val aluCopy = alu.copy()
            inp.set(aluCopy, it)
            for (instruction in group) instruction(aluCopy)
            solve(range, aluCopy, index + 1, 10 * prefix + it, visited)
        }
    }

    internal data class State(
        val index: Int,
        val w: Int = 0,
        val x: Int = 0,
        val y: Int = 0,
        val z: Int = 0,
    )

    private data class ALU(var w: Int = 0, var x: Int = 0, var y: Int = 0, var z: Int = 0) {
        companion object {
            operator fun get(key: String): KMutableProperty1<ALU, Int> = when (key) {
                "w" -> ALU::w
                "x" -> ALU::x
                "y" -> ALU::y
                "z" -> ALU::z
                else -> TODO()
            }
        }
    }

    private sealed class Instruction {
        abstract operator fun invoke(alu: ALU)

        class RegImm(
            private val lhs: KMutableProperty1<ALU, Int>,
            private val rhs: Int,
            private val op: Op,
        ) : Instruction() {
            override fun invoke(alu: ALU) {
                lhs.set(alu, op(lhs.get(alu), rhs))
            }
        }

        class RegReg(
            private val lhs: KMutableProperty1<ALU, Int>,
            private val rhs: KProperty1<ALU, Int>,
            private val op: Op,
        ) : Instruction() {
            override fun invoke(alu: ALU) {
                lhs.set(alu, op(lhs.get(alu), rhs.get(alu)))
            }
        }
    }

    @Suppress("EnumNaming")
    private enum class Op {
        add {
            override fun invoke(x: Int, y: Int): Int = x + y
        },
        mul {
            override fun invoke(x: Int, y: Int): Int = x * y
        },
        div {
            override fun invoke(x: Int, y: Int): Int = x / y
        },
        mod {
            override fun invoke(x: Int, y: Int): Int = x % y
        },
        eql {
            override fun invoke(x: Int, y: Int): Int = if (x == y) 1 else 0
        };

        abstract operator fun invoke(x: Int, y: Int): Int
    }

    class Provider : Day24Impl.Provider<Day24Common> {
        override fun invoke(lines: List<String>): Day24Common = Day24Common(lines)
    }
}
