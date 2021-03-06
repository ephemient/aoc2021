package com.github.ephemient.aoc2021

class Day24Range(lines: List<String>) : Day24Impl {
    private val instructions = lines.map { line ->
        if (line.startsWith("inp ")) {
            Instruction.Input(Register.valueOf(line.substring(4)))
        } else {
            require(line[3] == ' ' && line [5] == ' ')
            val operator = Operator.valueOf(line.substring(0, 3))
            val lhs = Register.valueOf(line.substring(4, 5))
            val rhs = line.substring(6)
            val value = rhs.toIntOrNull()
            Instruction.Operation(operator, lhs, if (value == null) Register.valueOf(rhs) else null, value ?: 0)
        }
    }

    override fun part1(): Long? = solve(instructions, 9 downTo 1)

    override fun part2(): Long? = solve(instructions, 1..9)

    @Suppress("EnumNaming")
    private enum class Register {
        w, x, y, z
    }

    @Suppress("EnumNaming")
    private enum class Operator {
        add, mul, div, mod, eql
    }

    private sealed class Instruction {
        data class Input(val lhs: Register) : Instruction()
        data class Operation(
            val operator: Operator,
            val lhs: Register,
            val rhs: Register?,
            val value: Int,
        ) : Instruction()
    }

    private data class ALU(var w: Long, var x: Long, var y: Long, var z: Long) {
        operator fun get(register: Register): Long = when (register) {
            Register.w -> w
            Register.x -> x
            Register.y -> y
            Register.z -> z
        }

        operator fun set(register: Register, value: Long) {
            when (register) {
                Register.w -> w = value
                Register.x -> x = value
                Register.y -> y = value
                Register.z -> z = value
            }
        }
    }

    private data class ALURange(var w: LongPair, var x: LongPair, var y: LongPair, var z: LongPair) {
        constructor(w: Long, x: Long, y: Long, z: Long) : this(w to w, x to x, y to y, z to z)
        constructor(alu: ALU) : this(alu.w, alu.x, alu.y, alu.z)

        operator fun get(register: Register): LongPair = when (register) {
            Register.w -> w
            Register.x -> x
            Register.y -> y
            Register.z -> z
        }

        operator fun set(register: Register, value: LongPair) {
            when (register) {
                Register.w -> w = value
                Register.x -> x = value
                Register.y -> y = value
                Register.z -> z = value
            }
        }
    }

    private enum class RangeResult {
        IMPOSSIBLE, POSSIBLE, UNKNOWN,
    }

    class Provider : Day24Impl.Provider<Day24Range> {
        override fun invoke(lines: List<String>): Day24Range = Day24Range(lines)

        override fun toString(): String = "Day24Range"
    }

    companion object {
        @Suppress("NestedBlockDepth")
        private fun solve(
            instructions: List<Instruction>,
            nums: IntProgression,
            prefix: Long = 0,
            alu: ALU = ALU(0, 0, 0, 0),
        ): Long? {
            for ((i, instruction) in instructions.withIndex()) {
                when (instruction) {
                    is Instruction.Input -> {
                        val instructions2 = instructions.subList(i + 1, instructions.size)
                        return nums.firstNotNullOfOrNull { num ->
                            alu[instruction.lhs] = num.toLong()
                            if (checkRange(instructions2, ALURange(alu)) != RangeResult.IMPOSSIBLE) {
                                solve(instructions2, nums, 10 * prefix + num, alu.copy())
                            } else null
                        }
                    }
                    is Instruction.Operation -> {
                        val lhs = alu[instruction.lhs]
                        val rhs = when (instruction.rhs) {
                            null -> instruction.value.toLong()
                            else -> alu[instruction.rhs]
                        }
                        alu[instruction.lhs] = when (instruction.operator) {
                            Operator.add -> lhs + rhs
                            Operator.mul -> lhs * rhs
                            Operator.div -> lhs / rhs
                            Operator.mod -> lhs % rhs
                            Operator.eql -> if (lhs == rhs) 1 else 0
                        }
                    }
                }
            }
            return if (alu.z == 0L) prefix else null
        }

        @Suppress("ComplexMethod", "NestedBlockDepth", "ReturnCount")
        private fun checkRange(instructions: List<Instruction>, alu: ALURange): RangeResult {
            for (instruction in instructions) {
                when (instruction) {
                    is Instruction.Input -> alu[instruction.lhs] = 1L to 9L
                    is Instruction.Operation -> {
                        val (a, b) = alu[instruction.lhs]
                        val (c, d) = when (instruction.rhs) {
                            null -> instruction.value.toLong() to instruction.value.toLong()
                            else -> alu[instruction.rhs]
                        }
                        alu[instruction.lhs] = when (instruction.operator) {
                            Operator.add -> a + c to b + d
                            Operator.mul -> when {
                                a >= 0 && c >= 0 -> a * c to b * d
                                a <= 0 && c <= 0 -> b * d to a * c
                                a >= 0 && c <= 0 -> a * d to b * c
                                a <= 0 && c >= 0 -> b * c to a * d
                                else -> {
                                    val xs = longArrayOf(a * c, a * d, b * c, b * d)
                                    xs.fold(0L, ::minOf) to xs.fold(0L, ::maxOf)
                                }
                            }
                            Operator.div -> when {
                                c > 0 -> a / d to b / c
                                d < 0 -> b / c to a / d
                                else -> return RangeResult.UNKNOWN
                            }
                            Operator.mod -> if (0 < c && c == d) {
                                if (b - a + 1 < c && a % c <= b % c) a % c to b % c else 0L to c - 1
                            } else return RangeResult.UNKNOWN
                            Operator.eql -> when {
                                a == b && b == c && c == d -> 1L to 1L
                                a <= d && c <= b -> 0L to 1L
                                else -> 0L to 0L
                            }
                        }
                    }
                }
            }
            return if (alu.z.first <= 0 && alu.z.second >= 0) RangeResult.POSSIBLE else RangeResult.IMPOSSIBLE
        }
    }
}
