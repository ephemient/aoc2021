package com.github.ephemient.aoc2021

@JsModule("detect-mocha")
@JsNonModule
external fun detectMocha(): Boolean

// https://youtrack.jetbrains.com/issue/KT-31888
internal actual fun getRunAllIfArgsEmpty(): Boolean = !detectMocha()
