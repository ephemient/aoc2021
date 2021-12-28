package com.github.ephemient.aoc2021

// https://youtrack.jetbrains.com/issue/KT-31888
internal actual fun getRunAllIfArgsEmpty(): Boolean = !js("require('detect-mocha')").unsafeCast<() -> Boolean>()()
