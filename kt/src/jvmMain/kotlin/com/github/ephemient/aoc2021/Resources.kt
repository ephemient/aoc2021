package com.github.ephemient.aoc2021

private enum class Resources

actual fun getInput(day: Int): List<String> =
    checkNotNull(Resources::class.java.classLoader.getResourceAsStream("day$day.txt")) { "No data for day $day" }
        .bufferedReader()
        .use { it.readLines() }
