package com.github.ephemient.aoc2021

class LruSet<T>(val capacity: Int) : AbstractMutableSet<T>() {
    init {
        require(capacity > 0)
    }

    private val mapping = mutableMapOf<T, Entry<T>>()
    private val root = Entry.Root<T>()

    override val size: Int
        get() = mapping.size

    override fun contains(element: T): Boolean = mapping[element]?.also { entry ->
        entry.prev.next
    } != null

    override fun add(element: T): Boolean = mapping[element]?.let { entry ->
        entry.prev.next = entry.next
        entry.next.prev = entry.prev
        entry.prev = root
        entry.next = root.next
        root.next.prev = entry
        root.next = entry
        false
    } ?: run {
        while (mapping.size >= capacity) check(remove(root.prev.value))
        val entry = Entry.Impl(element, root, root.next)
        check(mapping.put(element, entry) == null)
        root.next.prev = entry
        root.next = entry
        true
    }

    override fun remove(element: T): Boolean = mapping.remove(element)?.also { entry ->
        entry.prev.next = entry.next
        entry.next.prev = entry.prev
    } != null

    override fun iterator(): MutableIterator<T> = object : MutableIterator<T> {
        private var entry: Entry<T> = root

        override fun hasNext(): Boolean = entry.next !== root

        override fun next(): T = entry.next.also {
            check(it !== root)
            entry = it
        }.value

        override fun remove() {
            entry = entry.prev.also { check(entry !== root && remove(entry.value)) }
        }
    }

    private sealed class Entry<T> {
        abstract val value: T
        abstract var prev: Entry<T>
        abstract var next: Entry<T>

        class Impl<T>(override val value: T, override var prev: Entry<T>, override var next: Entry<T>) : Entry<T>()

        class Root<T> : Entry<T>() {
            override val value: Nothing
                get() = TODO()
            override var prev: Entry<T> = this
            override var next: Entry<T> = this
        }
    }
}
