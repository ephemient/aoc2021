package com.github.ephemient.aoc2021

import org.graalvm.nativeimage.ImageInfo

private enum class Resources

internal actual fun getInput(day: Int): List<String> {
    val testResource = if (System.getProperty(ImageInfo.PROPERTY_IMAGE_CODE_KEY) == "agent") {
        Resources::class.java.classLoader.getResourceAsStream("day$day-agent.txt")
    } else null
    val resource = testResource ?: Resources::class.java.classLoader.getResourceAsStream("day$day.txt")
    return checkNotNull(resource) { "No data for day $day" }.bufferedReader().use { it.readLines() }
}
