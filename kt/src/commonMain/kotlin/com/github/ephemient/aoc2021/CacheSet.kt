package com.github.ephemient.aoc2021

import kotlin.random.Random

internal class CacheSet<T : HashWithSalt>(
    capacity: Int,
    private val salt: Int = Random.nextInt(),
) : AbstractMutableSet<T>() {
    init {
        require(capacity > 0)
    }

    private val data = arrayOfNulls<Any>(capacity)
    override var size: Int = 0
        private set

    override fun contains(element: T): Boolean = data[element.hashWithSalt(salt).mod(data.size)] == element

    override fun add(element: T): Boolean {
        val index = element.hashWithSalt(salt).mod(data.size)
        val previous = data[index]
        data[index] = element
        return element != previous
    }

    override fun remove(element: T): Boolean {
        val index = element.hashWithSalt(salt).mod(data.size)
        val previous = data[index]
        return if (element == previous) {
            data[index] = null
            true
        } else false
    }

    override fun iterator(): MutableIterator<T> = object : MutableIterator<T> {
        private var index = 0

        override fun hasNext(): Boolean {
            while (index < data.size) {
                if (data[index] != null) return true
                index++
            }
            return false
        }

        override fun next(): T {
            while (index < data.size) {
                @Suppress("UNCHECKED_CAST")
                return (data[index++] ?: continue) as T
            }
            throw NoSuchElementException()
        }

        override fun remove() {
            for (i in index - 1 downTo 0) {
                if (data[i] != null) {
                    data[i] = null
                    return
                }
            }
            throw NoSuchElementException()
        }
    }
}
