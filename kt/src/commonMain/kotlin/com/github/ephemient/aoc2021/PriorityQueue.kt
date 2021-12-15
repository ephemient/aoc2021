package com.github.ephemient.aoc2021

expect class PriorityQueue<E : Any>(comparator: Comparator<in E>) {
    fun add(element: E): Boolean

    @Throws(NoSuchElementException::class)
    fun remove(): E
}
