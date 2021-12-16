package com.github.ephemient.aoc2021

class Day16(lines: List<String>) {
    private val packet = parsePacket(lines.single().iterator().hexBitsIterator())

    @ExperimentalStdlibApi
    fun part1(): Int = DeepRecursiveFunction<Packet, Int> { packet ->
        when (packet) {
            is Packet.Literal -> packet.version
            is Packet.Operator -> packet.version + packet.children.sumOf { callRecursive(it) }
        }
    }(packet)

    @ExperimentalStdlibApi
    fun part2(): Long = DeepRecursiveFunction<Packet, Long> { packet ->
        when (packet) {
            is Packet.Literal -> packet.value.toLong()
            is Packet.Operator -> when (packet.type) {
                0 -> packet.children.sumOf { callRecursive(it) }
                1 -> packet.children.fold(1L) { acc, child -> acc * callRecursive(child) }
                2 -> packet.children.minOf { callRecursive(it) }
                3 -> packet.children.maxOf { callRecursive(it) }
                5 -> if (callRecursive(packet.children[0]) > callRecursive(packet.children[1])) 1 else 0
                6 -> if (callRecursive(packet.children[0]) < callRecursive(packet.children[1])) 1 else 0
                7 -> if (callRecursive(packet.children[0]) == callRecursive(packet.children[1])) 1 else 0
                else -> TODO()
            }
        }
    }(packet)

    private sealed class Packet {
        abstract val version: Int

        data class Literal(override val version: Int, val value: Long) : Packet()

        data class Operator(override val version: Int, val type: Int, val children: List<Packet>) : Packet()
    }

    companion object {
        private fun parsePacket(iterator: BooleanIterator): Packet {
            val version = iterator.nextInt(3)
            val type = iterator.nextInt(3)
            return if (type == 4) {
                var value = 0L
                var continuation: Boolean
                do {
                    continuation = iterator.nextBoolean()
                    value = 16 * value + iterator.nextInt(4)
                } while (continuation)
                Packet.Literal(version, value)
            } else {
                val children = if (iterator.nextBoolean()) {
                    List(iterator.nextInt(11)) { parsePacket(iterator) }
                } else {
                    val childIterator = iterator.take(iterator.nextInt(15))
                    generateSequence { childIterator }.takeWhile { it.hasNext() }.map { parsePacket(it) }.toList()
                }
                Packet.Operator(version, type, children)
            }
        }

        private fun CharIterator.hexBitsIterator(): BooleanIterator = object : BooleanIterator() {
            private var buf = 0
            private var i = -1

            override fun hasNext(): Boolean = i >= 0 || this@hexBitsIterator.hasNext()

            override fun nextBoolean(): Boolean {
                if (i < 0) {
                    buf = nextChar().digitToInt(16)
                    i = 3
                }
                return buf shr i-- and 1 != 0
            }
        }

        private fun BooleanIterator.take(n: Int): BooleanIterator = object : BooleanIterator() {
            private var remaining = n

            override fun hasNext(): Boolean = remaining > 0 && this@take.hasNext()

            override fun nextBoolean(): Boolean = this@take.nextBoolean().also { remaining-- }
        }

        private fun BooleanIterator.nextInt(bits: Int): Int =
            (1..bits).fold(0) { acc, _ -> 2 * acc + if (nextBoolean()) 1 else 0 }
    }
}
